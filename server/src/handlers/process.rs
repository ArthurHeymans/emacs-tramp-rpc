//! Process execution operations

use crate::msgpack_map;
use crate::protocol::{ProcessResult, RpcError, from_value};
use nix::pty::{OpenptyResult, openpty};
use nix::sys::signal::Signal;
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::{Pid, tcgetpgrp};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsRawFd, BorrowedFd, RawFd};
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, ExitStatus, Stdio};
use std::sync::{Arc, OnceLock};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command};
use tokio::sync::Mutex;

use super::HandlerResult;

const MAX_READ_BYTES: usize = 16 * 1024 * 1024;

fn command_path(command: &str, cwd: Option<&str>) -> PathBuf {
    let path = Path::new(command);
    if path.is_relative() && command.contains('/') {
        cwd.map_or_else(
            || path.to_path_buf(),
            |dir| PathBuf::from(super::expand_tilde(dir)).join(path),
        )
    } else {
        path.to_path_buf()
    }
}

fn executable_is_missing(
    command: &str,
    cwd: Option<&str>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    if cwd.is_some_and(|dir| !Path::new(&super::expand_tilde(dir)).is_dir()) {
        return false;
    }

    if command.contains('/') {
        return matches!(
            std::fs::metadata(command_path(command, cwd)),
            Err(error) if error.kind() == ErrorKind::NotFound
        );
    }

    let path = env
        .and_then(|variables| variables.get("PATH").cloned())
        .or_else(|| (!clear_env).then(|| std::env::var("PATH").ok()).flatten())
        .unwrap_or_else(|| "/usr/bin:/bin".to_string());
    std::env::split_paths(&path).all(|dir| !dir.join(command).is_file())
}

fn spawn_error(error: std::io::Error, executable_missing: bool) -> RpcError {
    let mut data = Vec::new();
    if let Some(errno) = error.raw_os_error() {
        data.push((
            Value::String("os_errno".into()),
            Value::Integer(errno.into()),
        ));
    }
    data.push((
        Value::String("spawn_not_found".into()),
        Value::Boolean(error.kind() == ErrorKind::NotFound && executable_missing),
    ));
    RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to spawn process: {error}"),
        data: Some(Value::Map(data)),
    }
}

// ============================================================================
// Process management for async processes
// ============================================================================

static PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedProcess>>> = OnceLock::new();
static PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

#[cfg(test)]
static PROCESS_TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

#[cfg(test)]
pub(crate) async fn test_process_map_lock() -> tokio::sync::MutexGuard<'static, ()> {
    PROCESS_TEST_LOCK
        .get_or_init(|| Mutex::new(()))
        .lock()
        .await
}

fn get_process_map() -> &'static Mutex<HashMap<u32, ManagedProcess>> {
    PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

async fn get_next_pid() -> u32 {
    let counter = PID_COUNTER.get_or_init(|| Mutex::new(1));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

struct ManagedProcess {
    child: Child,
    exit_status: Option<ExitStatus>,
    stdin: Arc<Mutex<Option<ChildStdin>>>,
    stdout: Arc<Mutex<Option<ChildStdout>>>,
    stderr: Arc<Mutex<Option<ChildStderr>>>,
    cmd: String,
}

// ============================================================================
// Synchronous process execution (but async-friendly)
// ============================================================================

/// Run a command and wait for it to complete
pub async fn run(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Command to run
        cmd: String,
        /// Arguments
        #[serde(default)]
        args: Vec<String>,
        /// Working directory
        #[serde(default)]
        cwd: Option<String>,
        /// Environment variables to set
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        /// Stdin input as binary
        #[serde(default, with = "serde_bytes")]
        stdin: Option<Vec<u8>>,
        /// Clear environment before setting env vars
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        for (key, value) in env {
            cmd.env(key, value);
        }
    }

    // Set up stdin if provided
    if params.stdin.is_some() {
        cmd.stdin(Stdio::piped());
    }

    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let executable_missing = executable_is_missing(
        &params.cmd,
        params.cwd.as_deref(),
        params.env.as_ref(),
        params.clear_env,
    );
    let mut child = cmd
        .spawn()
        .map_err(|error| spawn_error(error, executable_missing))?;

    // Write stdin if provided (no base64 decoding needed!)
    if let Some(stdin_data) = params.stdin
        && let Some(mut stdin) = child.stdin.take()
    {
        let _ = stdin.write_all(&stdin_data).await;
    }

    // Wait for process to complete (async!)
    let output = child
        .wait_with_output()
        .await
        .map_err(|e| RpcError::process_error(format!("Failed to wait for process: {}", e)))?;

    // Return binary data directly (no encoding needed!)
    let exit_code = crate::protocol::exit_code_from_status(output.status);
    let result = ProcessResult {
        exit_code,
        stdout: output.stdout,
        stderr: output.stderr,
    };

    Ok(result.to_value())
}

// ============================================================================
// Asynchronous process management
// ============================================================================

/// Start an async process
pub async fn start(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        cmd: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        cwd: Option<String>,
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        for (key, value) in env {
            cmd.env(key, value);
        }
    }

    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let executable_missing = executable_is_missing(
        &params.cmd,
        params.cwd.as_deref(),
        params.env.as_ref(),
        params.clear_env,
    );
    let mut child = cmd
        .spawn()
        .map_err(|error| spawn_error(error, executable_missing))?;

    let pid = get_next_pid().await;

    let managed = ManagedProcess {
        exit_status: None,
        stdin: Arc::new(Mutex::new(child.stdin.take())),
        stdout: Arc::new(Mutex::new(child.stdout.take())),
        stderr: Arc::new(Mutex::new(child.stderr.take())),
        child,
        cmd: params.cmd.clone(),
    };

    get_process_map().lock().await.insert(pid, managed);

    Ok(msgpack_map! {
        "pid" => pid
    })
}

/// Write to an async process's stdin
pub async fn write(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    let stdin = {
        let processes = get_process_map().lock().await;
        processes
            .get(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?
            .stdin
            .clone()
    };

    let mut stdin_guard = stdin.lock().await;
    if let Some(stdin) = stdin_guard.as_mut() {
        stdin
            .write_all(&data)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to write to stdin: {}", e)))?;
    }

    Ok(msgpack_map! {
        "written" => data.len()
    })
}

/// Read from an async process's stdout/stderr
pub async fn read(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Maximum bytes to read
        #[serde(default = "default_max_read")]
        max_bytes: usize,
        /// Timeout in milliseconds to wait for data. If 0 or not specified, returns immediately.
        #[serde(default)]
        timeout_ms: Option<u64>,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.max_bytes == 0 || params.max_bytes > MAX_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);

    let (stdout, stderr) = {
        let processes = get_process_map().lock().await;
        let managed = processes
            .get(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        (managed.stdout.clone(), managed.stderr.clone())
    };

    // Try to read stdout/stderr (with optional blocking timeout) without
    // holding the global process map lock.  `process.read` is long-polled by
    // the Emacs client; holding that lock here makes concurrent
    // `process.write` calls wait behind the read timeout, which turns LSP
    // typing into a synchronous round-trip bottleneck.
    let (stdout_result, stderr_result) =
        try_read_streams(stdout, stderr, params.max_bytes, timeout).await?;

    let stdout_eof = matches!(stdout_result, ReadResult::Eof);
    let stderr_eof = matches!(stderr_result, ReadResult::Eof);
    let stdout_data = stdout_result.into_data();
    let stderr_data = stderr_result.into_data();

    // Check if process has exited.  Reacquire the map briefly; do not hold it
    // across any await points above.
    let exit_status = {
        let mut processes = get_process_map().lock().await;
        let managed = processes
            .get_mut(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        poll_exit_status(managed)
            .map_err(|e| RpcError::process_error(format!("Failed to query process status: {e}")))?
    };

    // Child exit and pipe EOF are separate events.  A child can exit after a
    // read returns data while additional bytes are still buffered in either
    // pipe.  Only report the terminal state after both streams have returned
    // EOF so the client continues issuing process.read requests until all
    // output has been delivered.
    let exited = exit_status.is_some() && stdout_eof && stderr_eof;

    // Return binary data directly (no encoding!)
    let stdout_val = if stdout_data.is_empty() {
        Value::Nil
    } else {
        Value::Binary(stdout_data)
    };

    let stderr_val = if stderr_data.is_empty() {
        Value::Nil
    } else {
        Value::Binary(stderr_data)
    };

    let exit_code = if exited {
        exit_status
            .map(crate::protocol::exit_code_from_status)
            .map(|code| Value::Integer(code.into()))
            .unwrap_or(Value::Nil)
    } else {
        Value::Nil
    };

    Ok(msgpack_map! {
        "stdout" => stdout_val,
        "stderr" => stderr_val,
        "exited" => exited,
        "exit_code" => exit_code
    })
}

enum ReadResult {
    Data(Vec<u8>),
    Pending,
    Eof,
}

impl ReadResult {
    fn into_data(self) -> Vec<u8> {
        match self {
            Self::Data(data) => data,
            Self::Pending | Self::Eof => Vec::new(),
        }
    }
}

fn poll_exit_status(managed: &mut ManagedProcess) -> std::io::Result<Option<ExitStatus>> {
    if managed.exit_status.is_none() {
        managed.exit_status = managed.child.try_wait()?;
    }
    Ok(managed.exit_status)
}

/// Read both output streams until either produces data or the shared timeout expires.
async fn try_read_streams<ROut, RErr>(
    stdout: Arc<Mutex<Option<ROut>>>,
    stderr: Arc<Mutex<Option<RErr>>>,
    max_bytes: usize,
    timeout_ms: u64,
) -> Result<(ReadResult, ReadResult), RpcError>
where
    ROut: AsyncRead + Unpin,
    RErr: AsyncRead + Unpin,
{
    let stdout_read = async {
        try_read_optional_stream(stdout, max_bytes)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stdout: {e}")))
    };
    let stderr_read = async {
        try_read_optional_stream(stderr, max_bytes)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stderr: {e}")))
    };
    let deadline = tokio::time::sleep(std::time::Duration::from_millis(if timeout_ms == 0 {
        1
    } else {
        timeout_ms
    }));

    tokio::pin!(stdout_read, stderr_read, deadline);

    // AsyncReadExt::read is cancellation-safe: when one stream produces data,
    // dropping the other branch cannot consume bytes from the idle stream.
    tokio::select! {
        stdout_result = &mut stdout_read => {
            let stdout_result = stdout_result?;
            if matches!(stdout_result, ReadResult::Data(_)) {
                return Ok((stdout_result, ReadResult::Pending));
            }

            let stderr_result = tokio::select! {
                stderr_result = &mut stderr_read => stderr_result?,
                _ = &mut deadline => ReadResult::Pending,
            };
            Ok((stdout_result, stderr_result))
        }
        stderr_result = &mut stderr_read => {
            let stderr_result = stderr_result?;
            if matches!(stderr_result, ReadResult::Data(_)) {
                return Ok((ReadResult::Pending, stderr_result));
            }

            let stdout_result = tokio::select! {
                stdout_result = &mut stdout_read => stdout_result?,
                _ = &mut deadline => ReadResult::Pending,
            };
            Ok((stdout_result, stderr_result))
        }
        _ = &mut deadline => Ok((ReadResult::Pending, ReadResult::Pending)),
    }
}

/// Try to read from an optional async reader.
async fn try_read_optional_stream<R>(
    stream: Arc<Mutex<Option<R>>>,
    max_bytes: usize,
) -> std::io::Result<ReadResult>
where
    R: AsyncRead + Unpin,
{
    let mut stream_guard = stream.lock().await;
    if let Some(reader) = stream_guard.as_mut() {
        let result = try_read_async(reader, max_bytes).await?;
        if matches!(result, ReadResult::Eof) {
            *stream_guard = None;
        }
        Ok(result)
    } else {
        Ok(ReadResult::Eof)
    }
}

/// Try to read from an async reader.
async fn try_read_async<R: AsyncRead + Unpin>(
    reader: &mut R,
    max_bytes: usize,
) -> std::io::Result<ReadResult> {
    let mut buf = vec![0u8; max_bytes];

    match reader.read(&mut buf).await {
        Ok(0) => Ok(ReadResult::Eof),
        Ok(n) => {
            buf.truncate(n);
            Ok(ReadResult::Data(buf))
        }
        Err(e) if e.kind() == ErrorKind::WouldBlock => Ok(ReadResult::Pending),
        Err(e) => Err(e),
    }
}

/// Close the stdin of an async process (signals EOF)
pub async fn close_stdin(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let stdin = {
        let processes = get_process_map().lock().await;
        processes
            .get(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?
            .stdin
            .clone()
    };

    // Flush any buffered data before closing stdin, then drop to close the pipe.
    // This is a defensive measure: the client should drain its write queue before
    // calling close_stdin, but flushing here guards against data loss if a
    // concurrent process.write task wrote data that hasn't been flushed yet.
    let mut stdin_guard = stdin.lock().await;
    if let Some(mut stdin) = stdin_guard.take() {
        let _ = stdin.flush().await;
        // stdin is dropped here, closing the pipe
    }

    Ok(Value::Boolean(true))
}

/// Kill an async process
pub async fn kill(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Signal to send (default: SIGTERM)
        #[serde(default = "default_signal")]
        signal: i32,
    }

    fn default_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes
        .get_mut(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;

    // Get the actual OS PID
    let os_pid = managed
        .child
        .id()
        .ok_or_else(|| RpcError::process_error("Process has no PID (already exited?)"))?;

    // Send the signal
    let result = unsafe { libc::kill(os_pid as i32, params.signal) };

    if result != 0 {
        return Err(RpcError::process_error(format!(
            "Failed to send signal: {}",
            std::io::Error::last_os_error()
        )));
    }

    // If SIGKILL, remove from process map
    if params.signal == libc::SIGKILL {
        processes.remove(&params.pid);
    }

    Ok(Value::Boolean(true))
}

/// Return status of an async process without consuming stdout/stderr.
pub async fn status(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes
        .get_mut(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;

    let exit_status = poll_exit_status(managed)
        .map_err(|e| RpcError::process_error(format!("Failed to query process status: {e}")))?;

    Ok(msgpack_map! {
        "exited" => exit_status.is_some(),
        "exit_code" => exit_status.map(crate::protocol::exit_code_from_status).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

/// List all managed async processes
pub async fn list(_params: Value) -> HandlerResult {
    let mut processes = get_process_map().lock().await;

    let list: Vec<Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            let exited = poll_exit_status(managed).ok().flatten();
            msgpack_map! {
                "pid" => *pid,
                "os_pid" => managed.child.id().map(|id| Value::Integer((id as i64).into())).unwrap_or(Value::Nil),
                "cmd" => managed.cmd.clone(),
                "exited" => exited.is_some(),
                "exit_code" => exited.map(crate::protocol::exit_code_from_status).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
            }
        })
        .collect();

    Ok(Value::Array(list))
}

// ============================================================================
// PTY (Pseudo-Terminal) Process Management
// ============================================================================

use std::os::unix::io::{FromRawFd, IntoRawFd, OwnedFd};
use std::os::unix::process::CommandExt;
use tokio::io::Interest;
use tokio::io::unix::AsyncFd;

static PTY_PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedPtyProcess>>> = OnceLock::new();
static PTY_PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

fn get_pty_process_map() -> &'static Mutex<HashMap<u32, ManagedPtyProcess>> {
    PTY_PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

async fn get_next_pty_pid() -> u32 {
    let counter = PTY_PID_COUNTER.get_or_init(|| Mutex::new(10000));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

struct ManagedPtyProcess {
    async_fd: AsyncFd<OwnedFd>,
    child_pid: Pid,
    cmd: String,
    exit_status: Option<i32>,
}

fn checked_fcntl(result: libc::c_int) -> Result<libc::c_int, std::io::Error> {
    if result < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(result)
    }
}

fn set_fd_nonblocking(fd: RawFd) -> Result<(), std::io::Error> {
    let flags = checked_fcntl(unsafe { libc::fcntl(fd, libc::F_GETFL) })?;
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) })?;
    Ok(())
}

fn set_fd_cloexec(fd: RawFd) -> Result<(), std::io::Error> {
    let flags = checked_fcntl(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) })?;
    Ok(())
}

fn dup_cloexec(fd: RawFd) -> Result<RawFd, std::io::Error> {
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0) })
}

fn set_window_size(fd: RawFd, rows: u16, cols: u16) -> Result<(), std::io::Error> {
    let ws = libc::winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    let result = unsafe { libc::ioctl(fd, libc::TIOCSWINSZ as _, &ws) };
    if result < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

#[derive(Clone)]
struct PtyStartParams {
    cmd: String,
    args: Vec<String>,
    cwd: Option<String>,
    env: Option<HashMap<String, String>>,
    clear_env: bool,
    rows: u16,
    cols: u16,
}

struct ForkResult2 {
    master_fd: RawFd,
    child_pid: Pid,
    tty_name: String,
}

fn do_fork_exec(params: PtyStartParams) -> Result<ForkResult2, RpcError> {
    let OpenptyResult { master, slave } = openpty(None, None)
        .map_err(|e| RpcError::process_error(format!("Failed to open PTY: {}", e)))?;

    set_fd_cloexec(master.as_raw_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {}", e)))?;
    set_fd_cloexec(slave.as_raw_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {}", e)))?;

    let tty_name = {
        let mut buf = vec![0u8; 256];
        let ret = unsafe {
            libc::ttyname_r(
                slave.as_raw_fd(),
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
            )
        };
        if ret != 0 {
            return Err(RpcError::process_error(format!(
                "Failed to get tty name: {}",
                std::io::Error::from_raw_os_error(ret)
            )));
        }
        let nul_pos = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
        String::from_utf8_lossy(&buf[..nul_pos]).into_owned()
    };

    set_window_size(master.as_raw_fd(), params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to set window size: {}", e)))?;

    let mut cmd = StdCommand::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        cmd.envs(env);
    }

    let slave_fd = slave.as_raw_fd();
    let master_fd = master.as_raw_fd();
    let stdin_fd = dup_cloexec(slave_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?;
    let stdout_fd = dup_cloexec(slave_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?;
    let stderr_fd = dup_cloexec(slave_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?;

    // SAFETY: `dup_cloexec` returned fresh owned file descriptors; `Stdio` takes ownership.
    cmd.stdin(unsafe { Stdio::from_raw_fd(stdin_fd) });
    cmd.stdout(unsafe { Stdio::from_raw_fd(stdout_fd) });
    cmd.stderr(unsafe { Stdio::from_raw_fd(stderr_fd) });

    // SAFETY: the pre-exec hook only calls async-signal-safe libc syscalls.
    unsafe {
        cmd.pre_exec(move || {
            libc::close(master_fd);
            if libc::setsid() < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if libc::ioctl(slave_fd, libc::TIOCSCTTY as _, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if slave_fd > 2 {
                libc::close(slave_fd);
            }
            Ok(())
        });
    }

    let child = cmd
        .spawn()
        .map_err(|e| RpcError::process_error(format!("Failed to spawn PTY process: {}", e)))?;
    drop(slave);

    Ok(ForkResult2 {
        master_fd: master.into_raw_fd(),
        child_pid: Pid::from_raw(child.id() as i32),
        tty_name,
    })
}

/// Start a process with a PTY (pseudo-terminal)
pub async fn start_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        cmd: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        cwd: Option<String>,
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        #[serde(default)]
        clear_env: bool,
        #[serde(default = "default_rows")]
        rows: u16,
        #[serde(default = "default_cols")]
        cols: u16,
    }

    fn default_rows() -> u16 {
        24
    }
    fn default_cols() -> u16 {
        80
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let start_params = PtyStartParams {
        cmd: params.cmd.clone(),
        args: params.args,
        cwd: params.cwd,
        env: params.env,
        clear_env: params.clear_env,
        rows: params.rows,
        cols: params.cols,
    };

    let fork_result = tokio::task::spawn_blocking(move || do_fork_exec(start_params))
        .await
        .map_err(|e| RpcError::process_error(format!("Task join error: {}", e)))??;

    set_fd_nonblocking(fork_result.master_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to set non-blocking: {}", e)))?;

    let owned_fd = unsafe { OwnedFd::from_raw_fd(fork_result.master_fd) };
    let async_fd = AsyncFd::new(owned_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to create AsyncFd: {}", e)))?;

    let our_pid = get_next_pty_pid().await;

    let managed = ManagedPtyProcess {
        async_fd,
        child_pid: fork_result.child_pid,
        cmd: params.cmd.clone(),
        exit_status: None,
    };

    get_pty_process_map().lock().await.insert(our_pid, managed);

    Ok(msgpack_map! {
        "pid" => our_pid,
        "os_pid" => fork_result.child_pid.as_raw(),
        "tty_name" => fork_result.tty_name
    })
}

/// Resize a PTY terminal
pub async fn resize_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        rows: u16,
        cols: u16,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let processes = get_pty_process_map().lock().await;
    let managed = processes
        .get(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("PTY process not found: {}", params.pid)))?;

    let fd = managed.async_fd.get_ref().as_raw_fd();

    set_window_size(fd, params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to resize PTY: {}", e)))?;

    match tcgetpgrp(unsafe { BorrowedFd::borrow_raw(fd) }) {
        Ok(fg_pgrp) => {
            let _ = nix::sys::signal::kill(Pid::from_raw(-fg_pgrp.as_raw()), Signal::SIGWINCH);
        }
        Err(_) => {
            let _ = nix::sys::signal::kill(
                Pid::from_raw(-managed.child_pid.as_raw()),
                Signal::SIGWINCH,
            );
        }
    }

    Ok(Value::Boolean(true))
}

/// Read from a PTY process with optional blocking
pub async fn read_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_max_read")]
        max_bytes: usize,
        #[serde(default)]
        timeout_ms: Option<u64>,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.max_bytes == 0 || params.max_bytes > MAX_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);
    let mut buf = vec![0u8; params.max_bytes];

    let read_result = {
        let mut processes = get_pty_process_map().lock().await;
        let managed = match processes.get_mut(&params.pid) {
            Some(m) => m,
            None => {
                return Ok(msgpack_map! {
                    "output" => Value::Nil,
                    "exited" => true,
                    "exit_code" => Value::Nil
                });
            }
        };

        let fd = managed.async_fd.get_ref().as_raw_fd();
        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };

        if n > 0 {
            buf.truncate(n as usize);
            Some((buf.clone(), false, None))
        } else if timeout == 0 {
            Some((vec![], false, None))
        } else {
            let (exited, exit_code) = check_exit_status(managed);
            if exited {
                Some((vec![], true, exit_code))
            } else {
                None
            }
        }
    };

    if let Some((output, exited, exit_code)) = read_result {
        let (exited, exit_code) = if exited {
            (exited, exit_code)
        } else {
            let mut processes = get_pty_process_map().lock().await;
            if let Some(managed) = processes.get_mut(&params.pid) {
                check_exit_status(managed)
            } else {
                (true, None)
            }
        };

        let output_val = if output.is_empty() {
            Value::Nil
        } else {
            Value::Binary(output)
        };

        return Ok(msgpack_map! {
            "output" => output_val,
            "exited" => exited,
            "exit_code" => exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
        });
    }

    let wait_result = tokio::time::timeout(
        std::time::Duration::from_millis(timeout),
        wait_for_pty_readable(params.pid),
    )
    .await;

    let mut processes = get_pty_process_map().lock().await;
    let managed = match processes.get_mut(&params.pid) {
        Some(m) => m,
        None => {
            return Ok(msgpack_map! {
                "output" => Value::Nil,
                "exited" => true,
                "exit_code" => Value::Nil
            });
        }
    };

    let output = if wait_result.is_ok() {
        let fd = managed.async_fd.get_ref().as_raw_fd();
        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        if n > 0 {
            buf.truncate(n as usize);
            buf
        } else {
            vec![]
        }
    } else {
        vec![]
    };

    let (exited, exit_code) = check_exit_status(managed);

    let output_val = if output.is_empty() {
        Value::Nil
    } else {
        Value::Binary(output)
    };

    Ok(msgpack_map! {
        "output" => output_val,
        "exited" => exited,
        "exit_code" => exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

fn check_exit_status(managed: &mut ManagedPtyProcess) -> (bool, Option<i32>) {
    if managed.exit_status.is_some() {
        (true, managed.exit_status)
    } else {
        match waitpid(managed.child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Exited(_, code)) => {
                managed.exit_status = Some(code);
                (true, Some(code))
            }
            Ok(WaitStatus::Signaled(_, signal, _)) => {
                let code = 128 + signal as i32;
                managed.exit_status = Some(code);
                (true, Some(code))
            }
            Ok(WaitStatus::StillAlive) => (false, None),
            _ => (false, None),
        }
    }
}

async fn wait_for_pty_readable(pid: u32) -> bool {
    let fd = {
        let processes = get_pty_process_map().lock().await;
        match processes.get(&pid) {
            Some(m) => m.async_fd.get_ref().as_raw_fd(),
            None => return false,
        }
    };

    loop {
        let ready = tokio::task::spawn_blocking(move || {
            let mut pollfd = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            let ret = unsafe { libc::poll(&mut pollfd, 1, 100) };
            ret > 0 && (pollfd.revents & libc::POLLIN) != 0
        })
        .await
        .unwrap_or(false);

        if ready {
            return true;
        }

        let processes = get_pty_process_map().lock().await;
        if !processes.contains_key(&pid) {
            return false;
        }
        tokio::task::yield_now().await;
    }
}

/// Write to a PTY process (async)
pub async fn write_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    let processes = get_pty_process_map().lock().await;
    let managed = processes
        .get(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("PTY process not found: {}", params.pid)))?;

    let mut guard = managed
        .async_fd
        .ready(Interest::WRITABLE)
        .await
        .map_err(|e| RpcError::process_error(format!("Failed to wait for writable: {}", e)))?;

    let written = match guard.try_io(|inner| {
        let n = unsafe {
            libc::write(
                inner.get_ref().as_raw_fd(),
                data.as_ptr() as *const libc::c_void,
                data.len(),
            )
        };
        if n >= 0 {
            Ok(n as usize)
        } else {
            Err(std::io::Error::last_os_error())
        }
    }) {
        Ok(Ok(n)) => n,
        Ok(Err(e)) => {
            return Err(RpcError::process_error(format!(
                "Failed to write to PTY: {}",
                e
            )));
        }
        Err(_would_block) => 0,
    };

    Ok(msgpack_map! {
        "written" => written
    })
}

/// Kill a PTY process
pub async fn kill_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_pty_signal")]
        signal: i32,
    }

    fn default_pty_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;
    let managed = processes
        .get(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("PTY process not found: {}", params.pid)))?;

    let signal = Signal::try_from(params.signal).map_err(|_| RpcError {
        code: RpcError::INVALID_PARAMS,
        message: format!("Invalid signal: {}", params.signal),
        data: None,
    })?;

    nix::sys::signal::kill(managed.child_pid, signal)
        .map_err(|e| RpcError::process_error(format!("Failed to send signal: {}", e)))?;

    if params.signal == libc::SIGKILL {
        processes.remove(&params.pid);
    }

    Ok(Value::Boolean(true))
}

/// Close a PTY process and clean up
pub async fn close_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;

    match processes.remove(&params.pid) {
        Some(managed) => {
            let _ = nix::sys::signal::kill(managed.child_pid, Signal::SIGKILL);
            Ok(Value::Boolean(true))
        }
        _ => Err(RpcError::process_error(format!(
            "PTY process not found: {}",
            params.pid
        ))),
    }
}

/// List all PTY processes
pub async fn list_pty(_params: Value) -> HandlerResult {
    let mut processes = get_pty_process_map().lock().await;

    let list: Vec<Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            let (exited, exit_code) = check_exit_status(managed);

            msgpack_map! {
                "pid" => *pid,
                "os_pid" => managed.child_pid.as_raw(),
                "cmd" => managed.cmd.clone(),
                "exited" => exited,
                "exit_code" => exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
            }
        })
        .collect();

    Ok(Value::Array(list))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map_get<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
        value.as_map().and_then(|m| {
            m.iter()
                .find(|(k, _)| k.as_str() == Some(key))
                .map(|(_, v)| v)
        })
    }

    async fn start_pipe_process(script: &str) -> u32 {
        let result = start(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String(script.into()),
                ]),
            ),
        ]))
        .await
        .expect("start pipe process");

        map_get(&result, "pid")
            .and_then(Value::as_u64)
            .expect("process pid") as u32
    }

    async fn read_pipe_process(pid: u32, max_bytes: usize, timeout_ms: u64) -> Value {
        read(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("max_bytes".into()),
                Value::Integer((max_bytes as u64).into()),
            ),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(timeout_ms.into()),
            ),
        ]))
        .await
        .expect("read pipe process")
    }

    async fn child_has_exited(pid: u32) -> bool {
        for _ in 0..100 {
            let result = status(Value::Map(vec![(
                Value::String("pid".into()),
                Value::Integer(pid.into()),
            )]))
            .await
            .expect("query pipe process status");
            if map_get(&result, "exited").and_then(Value::as_bool) == Some(true) {
                return true;
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }

        false
    }

    async fn wait_for_child_exit(pid: u32) {
        assert!(child_has_exited(pid).await, "process {pid} did not exit");
    }

    async fn pipe_streams_at_eof(pid: u32) -> bool {
        let (stdout, stderr) = {
            let processes = get_process_map().lock().await;
            let managed = processes.get(&pid).expect("pipe process");
            (managed.stdout.clone(), managed.stderr.clone())
        };
        stdout.lock().await.is_none() && stderr.lock().await.is_none()
    }

    async fn remove_pipe_process(pid: u32) {
        let managed = get_process_map().lock().await.remove(&pid);
        if let Some(mut managed) = managed
            && managed.exit_status.is_none()
        {
            let _ = managed.child.start_kill();
            let _ = managed.child.wait().await;
        }
    }

    async fn collect_pipe_output(pid: u32, max_bytes: usize) -> (Vec<u8>, Vec<u8>, i64) {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();

        for _ in 0..64 {
            let result = read_pipe_process(pid, max_bytes, 500).await;
            if let Some(Value::Binary(bytes)) = map_get(&result, "stdout") {
                stdout.extend_from_slice(bytes);
            }
            if let Some(Value::Binary(bytes)) = map_get(&result, "stderr") {
                stderr.extend_from_slice(bytes);
            }

            if map_get(&result, "exited").and_then(Value::as_bool) == Some(true) {
                let exit_code = map_get(&result, "exit_code")
                    .and_then(Value::as_i64)
                    .expect("exit code");
                remove_pipe_process(pid).await;
                return (stdout, stderr, exit_code);
            }

            if pipe_streams_at_eof(pid).await {
                tokio::task::yield_now().await;
                if !child_has_exited(pid).await {
                    remove_pipe_process(pid).await;
                    panic!("process {pid} did not exit after pipe EOF");
                }
            }
        }

        remove_pipe_process(pid).await;
        panic!("process {pid} did not reach EOF");
    }

    #[tokio::test]
    async fn process_read_rejects_zero_max_bytes() {
        let error = read(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(1.into())),
            (Value::String("max_bytes".into()), Value::Integer(0.into())),
        ]))
        .await
        .expect_err("zero max_bytes should be rejected");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn process_spawn_not_found_preserves_errno() {
        let error = run(Value::Map(vec![(
            Value::String("cmd".into()),
            Value::String("/definitely/not/a/tramp-rpc-command".into()),
        )]))
        .await
        .expect_err("missing command should fail to spawn");

        assert_eq!(error.code, RpcError::PROCESS_ERROR);
        let errno = error
            .data
            .as_ref()
            .and_then(Value::as_map)
            .and_then(|data| {
                data.iter()
                    .find(|(key, _)| key.as_str() == Some("os_errno"))
            })
            .and_then(|(_, value)| value.as_i64());
        assert_eq!(errno, Some(i64::from(libc::ENOENT)));
        assert_eq!(
            map_get(error.data.as_ref().unwrap(), "spawn_not_found"),
            Some(&Value::Boolean(true))
        );
    }

    #[tokio::test]
    async fn process_spawn_missing_cwd_is_not_command_not_found() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let missing_cwd = tmp.path().join("missing");
        let error = run(Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/true".into()),
            ),
            (
                Value::String("cwd".into()),
                Value::String(missing_cwd.to_string_lossy().into_owned().into()),
            ),
        ]))
        .await
        .expect_err("missing cwd should fail to spawn");

        assert_eq!(error.code, RpcError::PROCESS_ERROR);
        assert_eq!(
            map_get(error.data.as_ref().unwrap(), "os_errno").and_then(Value::as_i64),
            Some(i64::from(libc::ENOENT))
        );
        assert_eq!(
            map_get(error.data.as_ref().unwrap(), "spawn_not_found"),
            Some(&Value::Boolean(false))
        );
    }

    #[tokio::test]
    async fn read_size_limit_rejects_oversized_request() {
        let error = read(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(1.into())),
            (
                Value::String("max_bytes".into()),
                Value::Integer(((MAX_READ_BYTES + 1) as u64).into()),
            ),
        ]))
        .await
        .expect_err("oversized process read should be rejected");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn process_read_drains_pipes_after_status_reports_exit() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("printf abc; printf XYZ >&2").await;
        wait_for_child_exit(pid).await;

        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 1).await;
        assert_eq!(stdout, b"abc");
        assert_eq!(stderr, b"XYZ");
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn process_read_waits_for_eof_after_child_exit() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("printf first; sleep 0.1; printf second").await;

        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;
        assert_eq!(stdout, b"firstsecond");
        assert!(stderr.is_empty());
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn process_read_drains_output_larger_than_max_bytes() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("printf 0123456789abcdef").await;
        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 3).await;

        assert_eq!(stdout, b"0123456789abcdef");
        assert!(stderr.is_empty());
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn process_read_returns_stdout_before_idle_stderr_timeout() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("printf stdout; sleep 1").await;

        let started = std::time::Instant::now();
        let first = read_pipe_process(pid, 65_536, 500).await;
        assert!(
            started.elapsed() < std::time::Duration::from_millis(250),
            "stdout was delayed behind the idle stderr timeout"
        );
        assert_eq!(
            map_get(&first, "stdout"),
            Some(&Value::Binary(b"stdout".to_vec()))
        );
        assert_eq!(map_get(&first, "stderr"), Some(&Value::Nil));
        assert_eq!(
            map_get(&first, "exited").and_then(Value::as_bool),
            Some(false)
        );

        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;
        assert!(stdout.is_empty());
        assert!(stderr.is_empty());
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn process_read_delivers_output_written_immediately_before_exit() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("sleep 0.05; printf final").await;
        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;

        assert_eq!(stdout, b"final");
        assert!(stderr.is_empty());
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn process_read_drains_stdout_and_stderr_separately() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("printf stdout; printf stderr >&2").await;
        let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;

        assert_eq!(stdout, b"stdout");
        assert_eq!(stderr, b"stderr");
        assert_eq!(exit_code, 0);
    }

    #[tokio::test]
    async fn read_pty_rejects_oversized_max_bytes() {
        let error = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(1.into())),
            (
                Value::String("max_bytes".into()),
                Value::Integer(((MAX_READ_BYTES + 1) as u64).into()),
            ),
        ]))
        .await
        .expect_err("oversized PTY read should be rejected");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn start_pty_applies_env_without_mutating_process_env() {
        let parent_value = std::env::var("TRAMP_RPC_PTY_TEST").ok();
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("printf %s \"$TRAMP_RPC_PTY_TEST\"; read _".into()),
                ]),
            ),
            (Value::String("clear_env".into()), Value::Boolean(true)),
            (
                Value::String("env".into()),
                Value::Map(vec![(
                    Value::String("TRAMP_RPC_PTY_TEST".into()),
                    Value::String("ok".into()),
                )]),
            ),
        ]))
        .await
        .expect("start pty");

        let pid = map_get(&start, "pid").and_then(Value::as_u64).expect("pid") as u32;
        let mut output = Vec::new();

        for _ in 0..5 {
            let read = read_pty(Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(1_000.into()),
                ),
            ]))
            .await
            .expect("read pty");

            if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
                output.extend_from_slice(bytes);
            }
            if !output.is_empty() {
                break;
            }
        }

        let _ = close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await;

        assert_eq!(String::from_utf8_lossy(&output), "ok");
        assert_eq!(std::env::var("TRAMP_RPC_PTY_TEST").ok(), parent_value);
    }
}
