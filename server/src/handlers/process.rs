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
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex as StdMutex, OnceLock};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command};
use tokio::sync::{Mutex, Notify};

use super::HandlerResult;

const MAX_PROCESS_READ_BYTES: usize = 1024 * 1024;

async fn read_sync_output<R>(
    mut reader: R,
    remaining: Arc<AtomicUsize>,
) -> Result<Vec<u8>, RpcError>
where
    R: AsyncRead + Unpin,
{
    let mut output = Vec::new();
    let mut buffer = [0u8; 8192];
    loop {
        let read = reader.read(&mut buffer).await.map_err(|error| {
            RpcError::process_error(format!("Failed to read process output: {error}"))
        })?;
        if read == 0 {
            return Ok(output);
        }
        if remaining
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
                current.checked_sub(read)
            })
            .is_err()
        {
            return Err(RpcError::process_error(format!(
                "Process output exceeds {} byte limit",
                crate::MAX_RESPONSE_OUTPUT_BYTES
            )));
        }
        output.extend_from_slice(&buffer[..read]);
    }
}

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

fn executable_is_missing_sync(
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
    let cwd = cwd.map(|dir| PathBuf::from(super::expand_tilde(dir)));
    std::env::split_paths(&path).all(|dir| {
        let dir = if dir.is_relative() {
            cwd.as_ref().map_or(dir.clone(), |cwd| cwd.join(dir))
        } else {
            dir
        };
        !dir.join(command).is_file()
    })
}

async fn executable_is_missing(
    command: &str,
    cwd: Option<&str>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    let command = command.to_owned();
    let cwd = cwd.map(str::to_owned);
    let env = env.cloned();
    tokio::task::spawn_blocking(move || {
        executable_is_missing_sync(&command, cwd.as_deref(), env.as_ref(), clear_env)
    })
    .await
    .unwrap_or(false)
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

// Production starts one server OS process per RPC transport connection.  These
// process-local maps therefore cannot mix processes from separate connections;
// connection cleanup drains the maps when that one transport ends.  Tests
// serialize connection loops with `test_process_map_lock` for the same reason.
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

#[cfg(test)]
pub(crate) async fn test_managed_maps_empty() -> bool {
    get_process_map().lock().await.is_empty() && get_pty_process_map().lock().await.is_empty()
}

#[cfg(test)]
pub(crate) async fn test_managed_os_pids() -> Vec<i32> {
    let mut pids: Vec<_> = get_process_map()
        .lock()
        .await
        .values()
        .filter_map(|managed| managed.child.id().map(|pid| pid as i32))
        .collect();
    pids.extend(
        get_pty_process_map()
            .lock()
            .await
            .values()
            .map(|managed| managed.child_pid.as_raw()),
    );
    pids
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
    lifecycle: Arc<Mutex<()>>,
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

    // Never let a synchronous child consume the RPC transport.  A pipe is
    // only needed when the caller supplied input.
    cmd.stdin(if params.stdin.is_some() {
        Stdio::piped()
    } else {
        Stdio::null()
    });

    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    cmd.kill_on_drop(true);

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(error) => {
            let executable_missing = executable_is_missing(
                &params.cmd,
                params.cwd.as_deref(),
                params.env.as_ref(),
                params.clear_env,
            )
            .await;
            return Err(spawn_error(error, executable_missing));
        }
    };

    // Drive stdin, bounded output drains, and child exit concurrently.  Any
    // pipe or size error cancels the other operations and kills the child.
    let stdin_data = params.stdin;
    let mut stdin = child.stdin.take();
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| RpcError::process_error("Failed to capture process stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| RpcError::process_error("Failed to capture process stderr"))?;
    let write_stdin = async move {
        if let Some(data) = stdin_data
            && let Some(mut stdin) = stdin.take()
        {
            stdin
                .write_all(&data)
                .await
                .map_err(|e| RpcError::process_error(format!("Failed to write stdin: {e}")))?;
        }
        Ok::<(), RpcError>(())
    };
    let remaining = Arc::new(AtomicUsize::new(crate::MAX_RESPONSE_OUTPUT_BYTES));
    let result = tokio::try_join!(
        write_stdin,
        read_sync_output(stdout, Arc::clone(&remaining)),
        read_sync_output(stderr, remaining),
        async {
            child
                .wait()
                .await
                .map_err(|e| RpcError::process_error(format!("Failed to wait for process: {e}")))
        }
    );
    let ((), stdout, stderr, status) = match result {
        Ok(result) => result,
        Err(error) => {
            let _ = child.kill().await;
            let _ = child.wait().await;
            return Err(error);
        }
    };

    // Return binary data directly (no encoding needed!)
    let exit_code = crate::protocol::exit_code_from_status(status);
    let result = ProcessResult {
        exit_code,
        stdout,
        stderr,
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

    // Keep descendants in a group we can terminate without affecting the
    // server or unrelated processes.
    unsafe {
        cmd.pre_exec(|| {
            if libc::setpgid(0, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(error) => {
            let executable_missing = executable_is_missing(
                &params.cmd,
                params.cwd.as_deref(),
                params.env.as_ref(),
                params.clear_env,
            )
            .await;
            return Err(spawn_error(error, executable_missing));
        }
    };

    let pid = get_next_pid().await;

    let managed = ManagedProcess {
        lifecycle: Arc::new(Mutex::new(())),
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
    let Some(stdin) = stdin_guard.as_mut() else {
        return Err(RpcError::process_error(format!(
            "Process stdin is closed: {}",
            params.pid
        )));
    };
    stdin
        .write_all(&data)
        .await
        .map_err(|e| RpcError::process_error(format!("Failed to write to stdin: {}", e)))?;

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

    if params.max_bytes == 0 || params.max_bytes > MAX_PROCESS_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_PROCESS_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);

    let (stdout, stderr, lifecycle) = {
        let processes = get_process_map().lock().await;
        let managed = processes
            .get(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        (
            managed.stdout.clone(),
            managed.stderr.clone(),
            managed.lifecycle.clone(),
        )
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

    // Serialize terminal reads with kill/close without holding the global map
    // lock across the wait.
    let _lifecycle_guard = lifecycle.lock().await;

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

    // The terminal read is also the ownership handoff: the child has already
    // been reaped by poll_exit_status and both pipes have reached EOF.
    if exited {
        get_process_map().lock().await.remove(&params.pid);
    }

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

fn signal_process_group(pid: u32, signal: i32) -> std::io::Result<()> {
    let result = unsafe { libc::kill(-(pid as libc::pid_t), signal) };
    if result == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

async fn wait_pipe_child(os_pid: u32) -> Result<Option<ExitStatus>, nix::errno::Errno> {
    loop {
        match waitpid(Pid::from_raw(os_pid as i32), Some(WaitPidFlag::WNOHANG)) {
            Ok(status @ (WaitStatus::Exited(_, _) | WaitStatus::Signaled(_, _, _))) => {
                return Ok(Some(exit_status_from_wait_status(status)));
            }
            Ok(WaitStatus::StillAlive) => {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
            // Another status poll can only have consumed the status while the
            // lifecycle lock is not held.  The map entry remains until its
            // streams reach EOF, even in that case.
            Err(nix::errno::Errno::ECHILD) => return Ok(None),
            Err(error) => return Err(error),
            Ok(_) => return Err(nix::errno::Errno::EINVAL),
        }
    }
}

fn exit_status_from_wait_status(status: WaitStatus) -> ExitStatus {
    match status {
        WaitStatus::Exited(_, code) => ExitStatus::from_raw(code << 8),
        WaitStatus::Signaled(_, signal, core_dumped) => {
            ExitStatus::from_raw(signal as i32 | if core_dumped { 0x80 } else { 0 })
        }
        _ => ExitStatus::from_raw(0),
    }
}

const MANAGED_CHILD_WAIT: std::time::Duration = std::time::Duration::from_millis(500);
// PTY signal handlers may need a scheduler turn to flush their final output
// before exit; retain that output before escalating to SIGKILL.
const MANAGED_PTY_CHILD_WAIT: std::time::Duration = std::time::Duration::from_secs(2);

async fn terminate_pipe_process(pid: u32, signal: i32, escalate: bool) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle)) = ({
        let processes = get_process_map().lock().await;
        processes.get(&pid).and_then(|managed| {
            managed
                .child
                .id()
                .map(|os_pid| (os_pid, managed.lifecycle.clone()))
        })
    }) else {
        return Ok(true);
    };

    let _lifecycle_guard = lifecycle.lock().await;
    if let Err(error) = signal_process_group(os_pid, signal)
        && error.kind() != ErrorKind::NotFound
        && error.raw_os_error() != Some(libc::ESRCH)
    {
        return Err(RpcError::process_error(format!(
            "Failed to send signal: {error}"
        )));
    }

    let mut reap = tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(os_pid))
        .await
        .ok()
        .and_then(Result::ok);
    if reap.is_none() && escalate {
        let _ = signal_process_group(os_pid, libc::SIGKILL);
        reap = tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(os_pid))
            .await
            .ok()
            .and_then(Result::ok);
    }
    if let Some(exit_status) = reap {
        // Reaping is separate from stream ownership.  Keep the entry and its
        // pipes alive so a later read can deliver buffered output and observe
        // terminal EOF before removing it.
        if let Some(managed) = get_process_map().lock().await.get_mut(&pid) {
            managed.exit_status = exit_status;
        }
        return Ok(true);
    }

    // A signal request is not successful merely because it was sent.  Keep
    // the entry reachable so the caller can retry with SIGKILL.
    let already_reaped = get_process_map()
        .lock()
        .await
        .get(&pid)
        .is_some_and(|managed| managed.exit_status.is_some());
    if already_reaped {
        Ok(true)
    } else {
        Err(RpcError::process_error(format!(
            "Signal {signal} did not terminate process {pid} within the bounded wait"
        )))
    }
}

/// Kill an async process and reap its direct child.
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
    terminate_pipe_process(params.pid, params.signal, false).await?;
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

struct PtyIoState {
    // The write lock covers the whole logical input stream, not just one
    // syscall, so concurrent requests cannot interleave their bytes.
    write_lock: Mutex<()>,
    // Serialize the close transition with the final nonblocking write syscall.
    syscall_lock: StdMutex<()>,
    // This is retained state: cancellation publishes `closed` before the
    // permit notification.  There is at most one waiter because writes are
    // serialized, so notify_one cannot lose a wakeup.
    closed: AtomicBool,
    cancelled: Notify,
}

impl PtyIoState {
    /// Publish cancellation after any in-flight write syscall finishes.
    fn cancel(&self) {
        let _syscall_guard = self.syscall_lock.lock().expect("PTY syscall lock");
        if !self.closed.swap(true, Ordering::AcqRel) {
            self.cancelled.notify_one();
        }
    }

    fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire)
    }
}

struct ManagedPtyProcess {
    async_fd: AsyncFd<OwnedFd>,
    lifecycle: Arc<Mutex<()>>,
    io: Arc<PtyIoState>,
    child_pid: Pid,
    cmd: String,
    exit_status: Option<i32>,
    output_eof: bool,
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
        lifecycle: Arc::new(Mutex::new(())),
        io: Arc::new(PtyIoState {
            write_lock: Mutex::new(()),
            syscall_lock: StdMutex::new(()),
            closed: AtomicBool::new(false),
            cancelled: Notify::new(),
        }),
        child_pid: fork_result.child_pid,
        cmd: params.cmd.clone(),
        exit_status: None,
        output_eof: false,
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

    let lifecycle = {
        let processes = get_pty_process_map().lock().await;
        processes
            .get(&params.pid)
            .map(|managed| managed.lifecycle.clone())
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?
    };
    let _lifecycle_guard = lifecycle.lock().await;
    let (fd, child_pid, io) = {
        // Re-check the registry after acquiring lifecycle ownership: close or
        // terminal read may have removed the process while resize was waiting.
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (fd, managed.child_pid, managed.io.clone())
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY process is closed: {}",
            params.pid
        )));
    }

    set_window_size(owned_fd.as_raw_fd(), params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to resize PTY: {}", e)))?;

    match tcgetpgrp(&owned_fd) {
        Ok(fg_pgrp) => {
            let _ = nix::sys::signal::kill(Pid::from_raw(-fg_pgrp.as_raw()), Signal::SIGWINCH);
        }
        Err(_) => {
            let _ = nix::sys::signal::kill(Pid::from_raw(-child_pid.as_raw()), Signal::SIGWINCH);
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

    if params.max_bytes == 0 || params.max_bytes > MAX_PROCESS_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_PROCESS_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);
    let mut result = read_pty_now(params.pid, params.max_bytes).await?;
    if result.pending && timeout > 0 {
        let _ = tokio::time::timeout(
            std::time::Duration::from_millis(timeout),
            wait_for_pty_readable(params.pid),
        )
        .await;
        result = read_pty_now(params.pid, params.max_bytes).await?;
    }

    let output = if result.output.is_empty() {
        Value::Nil
    } else {
        Value::Binary(result.output)
    };
    Ok(msgpack_map! {
        "output" => output,
        "exited" => result.exited,
        "exit_code" => result.exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

struct PtyReadResult {
    output: Vec<u8>,
    pending: bool,
    exited: bool,
    exit_code: Option<i32>,
}

async fn read_pty_now(pid: u32, max_bytes: usize) -> Result<PtyReadResult, RpcError> {
    // Take an owned descriptor before awaiting either lifecycle lock or
    // readiness.  Registry removal can then safely close the original fd
    // without invalidating this read or allowing fd-number reuse to target a
    // newly-created PTY.
    let (lifecycle, io, fd) = {
        let processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get(&pid) else {
            return Ok(PtyReadResult {
                output: Vec::new(),
                pending: false,
                exited: true,
                exit_code: None,
            });
        };
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (managed.lifecycle.clone(), managed.io.clone(), fd)
    };
    // SAFETY: dup_cloexec returned a fresh descriptor owned by this read.
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let _lifecycle_guard = lifecycle.lock().await;
    let mut processes = get_pty_process_map().lock().await;
    let Some(managed) = processes.get_mut(&pid) else {
        return Ok(PtyReadResult {
            output: Vec::new(),
            pending: false,
            exited: true,
            exit_code: None,
        });
    };

    let mut output = vec![0u8; max_bytes];
    let (pending, eof) = match unsafe {
        libc::read(
            owned_fd.as_raw_fd(),
            output.as_mut_ptr() as *mut libc::c_void,
            output.len(),
        )
    } {
        n if n > 0 => {
            output.truncate(n as usize);
            (false, false)
        }
        0 => {
            output.clear();
            (false, true)
        }
        -1 if matches!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(errno) if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK
        ) =>
        {
            output.clear();
            (true, false)
        }
        // Linux reports PTY master EOF as EIO after the slave closes.
        -1 if std::io::Error::last_os_error().raw_os_error() == Some(libc::EIO) => {
            output.clear();
            (false, true)
        }
        -1 => {
            return Err(RpcError::process_error(format!(
                "Failed to read PTY: {}",
                std::io::Error::last_os_error()
            )));
        }
        _ => unreachable!(),
    };
    if eof {
        managed.output_eof = true;
    }

    let (child_exited, exit_code) = check_exit_status(managed);
    let exited = child_exited && managed.output_eof;
    drop(processes);
    if exited {
        // Do not hold the registry lock while waiting for an in-flight write
        // syscall to finish.  The lifecycle guard keeps close/kill ordered
        // with this terminal read.
        io.cancel();
        get_pty_process_map().lock().await.remove(&pid);
    }
    Ok(PtyReadResult {
        output,
        pending,
        exited,
        exit_code: exited.then_some(exit_code).flatten(),
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
    // Wait on a duplicate so close/kill can remove the registry entry and
    // close the real master without racing an in-flight readiness wait.
    let fd = {
        let processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get(&pid) else {
            return false;
        };
        match dup_cloexec(managed.async_fd.get_ref().as_raw_fd()) {
            Ok(fd) => fd,
            Err(_) => return false,
        }
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let async_fd = match AsyncFd::new(owned_fd) {
        Ok(fd) => fd,
        Err(_) => return false,
    };
    async_fd.readable().await.is_ok()
}

enum PtyWriteAction {
    Progress,
    Retry,
}

fn apply_pty_write(
    offset: &mut usize,
    total: usize,
    result: std::io::Result<usize>,
) -> Result<PtyWriteAction, RpcError> {
    match result {
        Ok(written) if written > 0 => {
            *offset += written;
            Ok(PtyWriteAction::Progress)
        }
        Ok(_) => Err(RpcError::process_error("PTY write returned zero bytes")),
        Err(error) if matches!(error.kind(), ErrorKind::Interrupted | ErrorKind::WouldBlock) => {
            debug_assert!(*offset <= total);
            Ok(PtyWriteAction::Retry)
        }
        Err(error) => Err(RpcError::process_error(format!(
            "Failed to write to PTY: {error}"
        ))),
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

    // Capture the descriptor and cancellation state while briefly holding the
    // registry lock.  All waits and writes happen on owned state afterwards.
    let (fd, io) = {
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?;
        (fd, managed.io.clone())
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let async_fd = AsyncFd::new(owned_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to monitor PTY: {}", e)))?;
    let _write_guard = io.write_lock.lock().await;
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY write cancelled: {}",
            params.pid
        )));
    }

    let mut offset = 0;
    while offset < data.len() {
        let cancelled = io.cancelled.notified();
        if io.is_closed() {
            return Err(RpcError::process_error(format!(
                "PTY write cancelled: {}",
                params.pid
            )));
        }
        let mut guard = tokio::select! {
            result = async_fd.ready(Interest::WRITABLE) => result
                .map_err(|e| RpcError::process_error(format!("Failed to wait for writable: {e}")))?,
            _ = cancelled => {
                return Err(RpcError::process_error(format!(
                    "PTY write cancelled: {}", params.pid
                )));
            }
        };

        // Cancellation and the final closed check are atomic with the
        // nonblocking syscall: no write can begin after close is published.
        let _syscall_guard = io.syscall_lock.lock().expect("PTY syscall lock");
        if io.is_closed() {
            return Err(RpcError::process_error(format!(
                "PTY write cancelled: {}",
                params.pid
            )));
        }
        let result = guard.try_io(|inner| {
            let n = unsafe {
                libc::write(
                    inner.get_ref().as_raw_fd(),
                    data[offset..].as_ptr() as *const libc::c_void,
                    data.len() - offset,
                )
            };
            if n >= 0 {
                Ok(n as usize)
            } else {
                Err(std::io::Error::last_os_error())
            }
        });
        match result {
            Ok(result) => {
                apply_pty_write(&mut offset, data.len(), result)?;
            }
            // `AsyncFd` clears stale writable readiness before retrying.
            Err(_would_block) => {}
        }
    }

    Ok(msgpack_map! {
        "written" => data.len()
    })
}

async fn wait_pty_pid(child_pid: Pid) -> Result<Option<i32>, nix::errno::Errno> {
    loop {
        match waitpid(child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Exited(_, code)) => return Ok(Some(code)),
            Ok(WaitStatus::Signaled(_, signal, _)) => return Ok(Some(128 + signal as i32)),
            Ok(WaitStatus::StillAlive) => {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
            Err(nix::errno::Errno::ECHILD) => return Ok(None),
            Err(error) => return Err(error),
            Ok(_) => return Err(nix::errno::Errno::EINVAL),
        }
    }
}

async fn terminate_pty_process(
    pid: u32,
    signal: i32,
    escalate: bool,
    remove: bool,
) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle, io)) = ({
        let processes = get_pty_process_map().lock().await;
        processes.get(&pid).map(|managed| {
            (
                managed.child_pid.as_raw() as u32,
                managed.lifecycle.clone(),
                managed.io.clone(),
            )
        })
    }) else {
        return Ok(true);
    };
    // Publish cancellation before waiting for lifecycle/reaping.  A writer
    // blocked in readiness wakes immediately and cannot start another syscall.
    io.cancel();
    let _lifecycle_guard = lifecycle.lock().await;
    if let Err(error) = signal_process_group(os_pid, signal)
        && error.kind() != ErrorKind::NotFound
        && error.raw_os_error() != Some(libc::ESRCH)
    {
        return Err(RpcError::process_error(format!(
            "Failed to send signal: {error}"
        )));
    }

    let mut reap = tokio::time::timeout(
        MANAGED_PTY_CHILD_WAIT,
        wait_pty_pid(Pid::from_raw(os_pid as i32)),
    )
    .await
    .ok()
    .and_then(Result::ok);
    if reap.is_none() && escalate {
        let _ = signal_process_group(os_pid, libc::SIGKILL);
        reap = tokio::time::timeout(
            MANAGED_PTY_CHILD_WAIT,
            wait_pty_pid(Pid::from_raw(os_pid as i32)),
        )
        .await
        .ok()
        .and_then(Result::ok);
    }
    if let Some(exit_code) = reap {
        // Kill reaps the direct child but deliberately leaves the PTY master
        // registered until read_pty observes terminal EOF, unless this is an
        // explicit close.
        let mut processes = get_pty_process_map().lock().await;
        if remove {
            processes.remove(&pid);
        } else if let Some(managed) = processes.get_mut(&pid) {
            managed.exit_status = exit_code;
        }
        return Ok(true);
    }

    if remove {
        // Explicit close discards the PTY even if its status was already
        // consumed by another status poll.
        get_pty_process_map().lock().await.remove(&pid);
        return Ok(true);
    }

    let already_reaped = get_pty_process_map()
        .lock()
        .await
        .get(&pid)
        .is_some_and(|managed| managed.exit_status.is_some());
    if already_reaped {
        Ok(true)
    } else {
        Err(RpcError::process_error(format!(
            "Signal {signal} did not terminate PTY process {pid} within the bounded wait"
        )))
    }
}

/// Kill a PTY process group and reap its direct child.
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
    Signal::try_from(params.signal).map_err(|_| RpcError {
        code: RpcError::INVALID_PARAMS,
        message: format!("Invalid signal: {}", params.signal),
        data: None,
    })?;
    // Do not silently turn SIGTERM (or another requested signal) into
    // SIGKILL.  The caller can retry explicitly with SIGKILL.
    terminate_pty_process(params.pid, params.signal, false, false).await?;
    Ok(Value::Boolean(true))
}

/// Close a PTY process and discard buffered output.  Repeating close is harmless.
pub async fn close_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    // Explicit close is the opt-out from kill's drain-preserving ownership.
    terminate_pty_process(params.pid, libc::SIGKILL, true, true).await?;
    Ok(Value::Boolean(true))
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

/// Stop all managed children after the transport is gone.  Signal every group
/// before awaiting any child so descendants cannot keep another child alive.
pub async fn cleanup_managed_processes() {
    let pipe_pids: Vec<(u32, u32)> = {
        let processes = get_process_map().lock().await;
        processes
            .iter()
            .filter_map(|(pid, managed)| managed.child.id().map(|os_pid| (*pid, os_pid)))
            .collect()
    };
    let pty_pids: Vec<(u32, u32)> = {
        let processes = get_pty_process_map().lock().await;
        processes
            .iter()
            .map(|(pid, managed)| (*pid, managed.child_pid.as_raw() as u32))
            .collect()
    };

    for (_, os_pid) in pipe_pids.iter().chain(pty_pids.iter()) {
        let _ = signal_process_group(*os_pid, libc::SIGTERM);
    }

    let pipe_reaps = pipe_pids
        .into_iter()
        .map(|(pid, _)| terminate_pipe_process(pid, libc::SIGTERM, true));
    let pty_reaps = pty_pids
        .into_iter()
        .map(|(pid, _)| terminate_pty_process(pid, libc::SIGTERM, true, true));
    tokio::join!(
        futures::future::join_all(pipe_reaps),
        futures::future::join_all(pty_reaps)
    );

    // The transport is gone, so there is no reader left to drain these
    // streams.  Unlike ordinary kill, connection cleanup may discard them.
    get_process_map().lock().await.clear();
    get_pty_process_map().lock().await.clear();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn synchronous_output_reader_enforces_shared_limit() {
        let error = read_sync_output(&b"oversized"[..], Arc::new(AtomicUsize::new(4)))
            .await
            .expect_err("output above the remaining response budget should fail");
        assert_eq!(error.code, RpcError::PROCESS_ERROR);
        assert!(error.message.contains("output exceeds"));
    }

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

    async fn pipe_os_pid(pid: u32) -> i32 {
        get_process_map()
            .lock()
            .await
            .get(&pid)
            .and_then(|managed| managed.child.id())
            .expect("pipe OS pid") as i32
    }

    async fn pty_os_pid(pid: u32) -> i32 {
        get_pty_process_map()
            .lock()
            .await
            .get(&pid)
            .expect("PTY process")
            .child_pid
            .as_raw()
    }

    #[cfg(target_os = "linux")]
    fn fd_target_count(target: &Path) -> usize {
        std::fs::read_dir("/proc/self/fd")
            .expect("list open file descriptors")
            .filter_map(Result::ok)
            .filter(|entry| std::fs::read_link(entry.path()).ok().as_deref() == Some(target))
            .count()
    }

    fn full_nonblocking_pipe() -> (OwnedFd, OwnedFd) {
        let mut fds = [-1; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "create pipe");
        let read_fd = unsafe { OwnedFd::from_raw_fd(fds[0]) };
        let write_fd = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        set_fd_nonblocking(read_fd.as_raw_fd()).expect("make pipe reader nonblocking");
        set_fd_nonblocking(write_fd.as_raw_fd()).expect("make pipe writer nonblocking");
        set_fd_cloexec(read_fd.as_raw_fd()).expect("mark pipe reader close-on-exec");
        set_fd_cloexec(write_fd.as_raw_fd()).expect("mark pipe writer close-on-exec");
        let bytes = [0_u8; 8192];
        loop {
            let written = unsafe {
                libc::write(
                    write_fd.as_raw_fd(),
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                )
            };
            if written >= 0 {
                continue;
            }
            assert_eq!(
                std::io::Error::last_os_error().raw_os_error(),
                Some(libc::EAGAIN),
                "fill pipe to EAGAIN"
            );
            return (read_fd, write_fd);
        }
    }

    async fn install_full_pipe_pty(pid: u32) -> OwnedFd {
        let (read_fd, write_fd) = full_nonblocking_pipe();
        let async_fd = AsyncFd::new(write_fd).expect("monitor full pipe");
        get_pty_process_map()
            .lock()
            .await
            .get_mut(&pid)
            .expect("PTY process")
            .async_fd = async_fd;
        read_fd
    }

    async fn start_signal_ignoring_pty(marker: &Path) -> u32 {
        let script = format!(
            "import signal,time; signal.signal(signal.SIGHUP, signal.SIG_IGN); signal.signal(signal.SIGTERM, signal.SIG_IGN); open({:?}, 'w').close(); time.sleep(30)",
            marker
        );
        let result = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("python3".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String(script.into()),
                ]),
            ),
        ]))
        .await
        .expect("start PTY child");
        let pid = map_get(&result, "pid")
            .and_then(Value::as_u64)
            .expect("PTY pid") as u32;
        wait_for_marker(marker).await;
        pid
    }

    fn assert_reaped(os_pid: i32) {
        assert!(matches!(
            waitpid(Pid::from_raw(os_pid), Some(WaitPidFlag::WNOHANG)),
            Err(nix::errno::Errno::ECHILD)
        ));
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
    async fn write_closed_stdin_is_process_error() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("sleep 1").await;
        close_stdin(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close stdin");

        let error = write(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("data".into()),
                Value::Binary(b"data".to_vec()),
            ),
        ]))
        .await
        .expect_err("writing closed stdin should fail");
        assert_eq!(error.code, RpcError::PROCESS_ERROR);

        let missing = write(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(u32::MAX.into())),
            (
                Value::String("data".into()),
                Value::Binary(b"data".to_vec()),
            ),
        ]))
        .await
        .expect_err("writing to a missing process should fail");
        assert_eq!(missing.code, RpcError::PROCESS_ERROR);
        remove_pipe_process(pid).await;
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

    #[test]
    fn executable_lookup_resolves_relative_path_against_cwd() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let bin = tmp.path().join("bin");
        std::fs::create_dir(&bin).unwrap();
        std::fs::write(bin.join("tool"), b"").unwrap();
        let env = HashMap::from([("PATH".to_string(), "bin".to_string())]);

        assert!(!executable_is_missing_sync(
            "tool",
            tmp.path().to_str(),
            Some(&env),
            true
        ));
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
                Value::Integer(((MAX_PROCESS_READ_BYTES + 1) as u64).into()),
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
        assert!(!get_process_map().lock().await.contains_key(&pid));
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
    async fn kill_default_term_and_explicit_kill_reap_process_group() {
        let _test_lock = test_process_map_lock().await;
        let start_result = start(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("python3".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String(
                        "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(30)"
                            .into(),
                    ),
                ]),
            ),
        ]))
        .await
        .expect("start ignoring process");
        let pid = map_get(&start_result, "pid")
            .and_then(Value::as_u64)
            .unwrap() as u32;
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;

        let term_error = kill(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect_err("ignored SIGTERM must report failure");
        assert_eq!(term_error.code, RpcError::PROCESS_ERROR);
        assert!(get_process_map().lock().await.contains_key(&pid));

        let os_pid = pipe_os_pid(pid).await;
        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("SIGKILL");
        assert!(get_process_map().lock().await.contains_key(&pid));
        assert_reaped(os_pid);
        let _ = collect_pipe_output(pid, 65_536).await;
        assert!(!get_process_map().lock().await.contains_key(&pid));
    }

    async fn wait_for_marker(path: &std::path::Path) {
        for _ in 0..100 {
            if path.exists() {
                return;
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
        panic!("marker was not created: {}", path.display());
    }

    #[tokio::test]
    async fn pty_kill_ignored_sigterm_then_sigkill_reaps_child() {
        let _test_lock = test_process_map_lock().await;
        let temp = tempfile::tempdir().expect("temporary PTY directory");
        let marker = temp.path().join("ready");
        let pid = start_signal_ignoring_pty(&marker).await;
        let os_pid = pty_os_pid(pid).await;

        let term_error = kill_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGTERM as i64).into()),
            ),
        ]))
        .await
        .expect_err("ignored SIGTERM must report failure");
        assert_eq!(term_error.code, RpcError::PROCESS_ERROR);
        assert!(get_pty_process_map().lock().await.contains_key(&pid));

        kill_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("SIGKILL should terminate and reap the PTY child");
        assert_reaped(os_pid);
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close PTY after SIGKILL");
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pipe_kill_preserves_output_after_term_and_drains_descendant_fds() {
        let _test_lock = test_process_map_lock().await;
        let temp = tempfile::tempdir().expect("temporary marker directory");
        let marker = temp.path().join("ready");
        let script = format!(
            "python3 -c 'import os,signal,time; os.fork(); signal.signal(signal.SIGTERM, lambda *_: (os.write(1,b\"final\"), os._exit(0))); open(\"{}\",\"w\").close(); time.sleep(30)'",
            marker.display()
        );
        let pid = start_pipe_process(&script).await;
        wait_for_marker(&marker).await;
        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGTERM as i64).into()),
            ),
        ]))
        .await
        .expect("SIGTERM");

        assert!(get_process_map().lock().await.contains_key(&pid));
        let (stdout, _, exit_code) = collect_pipe_output(pid, 65_536).await;
        assert!(
            stdout
                .windows(b"final".len())
                .any(|chunk| chunk == b"final")
        );
        assert_eq!(exit_code, 0);
        assert!(!get_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pipe_sigkill_preserves_pending_output_and_reaps_direct_child() {
        let _test_lock = test_process_map_lock().await;
        let temp = tempfile::tempdir().expect("temporary marker directory");
        let marker = temp.path().join("ready");
        let script = format!(
            "python3 -c 'import os,time; os.write(1,b\"pending\"); os.fork(); open(\"{}\",\"w\").close(); time.sleep(30)'",
            marker.display()
        );
        let pid = start_pipe_process(&script).await;
        wait_for_marker(&marker).await;
        let os_pid = pipe_os_pid(pid).await;
        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("SIGKILL");

        assert_reaped(os_pid);
        let (stdout, _, _) = collect_pipe_output(pid, 65_536).await;
        assert_eq!(stdout, b"pending");
    }

    #[tokio::test]
    async fn pty_kill_preserves_output_until_terminal_eof() {
        let _test_lock = test_process_map_lock().await;
        let temp = tempfile::tempdir().expect("temporary marker directory");
        let marker = temp.path().join("ready");
        let script = format!(
            "trap 'printf final; exit 0' TERM; touch '{}'; sleep 30",
            marker.display()
        );
        let start = start_pty(Value::Map(vec![
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
        .expect("start pty");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        wait_for_marker(&marker).await;
        let os_pid = {
            get_pty_process_map()
                .lock()
                .await
                .get(&pid)
                .expect("pty process")
                .child_pid
                .as_raw()
        };

        kill_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGTERM as i64).into()),
            ),
        ]))
        .await
        .expect("SIGTERM");
        assert!(get_pty_process_map().lock().await.contains_key(&pid));
        assert_reaped(os_pid);

        let mut output = Vec::new();
        let mut exited = false;
        for _ in 0..40 {
            let read = read_pty(Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(100.into()),
                ),
            ]))
            .await
            .expect("read pty");
            if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
                output.extend_from_slice(bytes);
            }
            exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
            if exited {
                break;
            }
        }
        assert!(exited);
        assert!(
            output
                .windows(b"final".len())
                .any(|chunk| chunk == b"final")
        );
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pty_read_removes_entry_after_terminal_eof() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("printf final".into()),
                ]),
            ),
        ]))
        .await
        .expect("start pty");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        let mut output = Vec::new();
        let mut exited = false;
        for _ in 0..40 {
            let read = read_pty(Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(100.into()),
                ),
            ]))
            .await
            .expect("read pty");
            if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
                output.extend_from_slice(bytes);
            }
            exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
            if exited {
                break;
            }
        }
        assert!(exited);
        assert!(
            output
                .windows(b"final".len())
                .any(|chunk| chunk == b"final")
        );
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pty_read_drains_output_larger_than_max_bytes_after_exit() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("python3".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("import os; os.write(1, b'x' * 131072)".into()),
                ]),
            ),
        ]))
        .await
        .expect("start pty");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        let mut output = Vec::new();
        let mut exited = false;
        for _ in 0..64 {
            let result = read_pty(Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("max_bytes".into()),
                    Value::Integer(4096.into()),
                ),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(500.into()),
                ),
            ]))
            .await
            .expect("read pty output");
            if let Some(Value::Binary(bytes)) = map_get(&result, "output") {
                output.extend_from_slice(bytes);
            }
            exited = map_get(&result, "exited").and_then(Value::as_bool) == Some(true);
            if exited {
                break;
            }
        }
        assert!(exited);
        assert_eq!(output.len(), 131072);
        assert!(output.iter().all(|byte| *byte == b'x'));
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pty_read_errors_are_terminal_process_errors() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/sleep".into()),
            ),
            (
                Value::String("args".into()),
                Value::Array(vec![Value::String("30".into())]),
            ),
        ]))
        .await
        .expect("start pty");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;

        let mut fds = [-1; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let _read_end = unsafe { OwnedFd::from_raw_fd(fds[0]) };
        let write_end = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        set_fd_nonblocking(write_end.as_raw_fd()).expect("make test fd nonblocking");
        let async_fd = AsyncFd::new(write_end).expect("monitor test fd");
        get_pty_process_map()
            .lock()
            .await
            .get_mut(&pid)
            .expect("PTY process")
            .async_fd = async_fd;

        let error = read_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect_err("reading a write-only descriptor should fail");
        assert_eq!(error.code, RpcError::PROCESS_ERROR);
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close broken PTY");
    }

    #[tokio::test]
    async fn close_pty_is_idempotent_and_reaps_child() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("printf pending; sleep 30".into()),
                ]),
            ),
        ]))
        .await
        .expect("start pty");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("first close");
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("second close");
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }

    #[tokio::test]
    async fn pty_writes_are_serialized_in_acquisition_order() {
        let io = Arc::new(PtyIoState {
            write_lock: Mutex::new(()),
            syscall_lock: StdMutex::new(()),
            closed: AtomicBool::new(false),
            cancelled: Notify::new(),
        });
        let first_acquired = Arc::new(Notify::new());
        let release_first = Arc::new(Notify::new());
        let order = Arc::new(Mutex::new(Vec::new()));

        let first = {
            let io = io.clone();
            let first_acquired = first_acquired.clone();
            let release_first = release_first.clone();
            let order = order.clone();
            tokio::spawn(async move {
                let _guard = io.write_lock.lock().await;
                order.lock().await.push(1);
                first_acquired.notify_one();
                release_first.notified().await;
                order.lock().await.push(2);
            })
        };
        first_acquired.notified().await;

        let second = {
            let io = io.clone();
            let order = order.clone();
            tokio::spawn(async move {
                let _guard = io.write_lock.lock().await;
                order.lock().await.push(3);
            })
        };
        tokio::task::yield_now().await;
        assert_eq!(*order.lock().await, vec![1]);
        release_first.notify_one();
        first.await.expect("first writer");
        second.await.expect("second writer");
        assert_eq!(*order.lock().await, vec![1, 2, 3]);
    }

    #[test]
    fn pty_write_loop_handles_partial_eintr_and_eagain() {
        let mut offset = 0;
        let mut readiness_waits = 0;
        let script = [
            Ok(2),
            Err(std::io::Error::from_raw_os_error(libc::EINTR)),
            Err(std::io::Error::from_raw_os_error(libc::EAGAIN)),
            Ok(3),
        ];
        for result in script {
            match apply_pty_write(&mut offset, 5, result).expect("scripted write") {
                PtyWriteAction::Progress => {}
                PtyWriteAction::Retry => readiness_waits += 1,
            }
        }
        assert_eq!(offset, 5);
        assert_eq!(readiness_waits, 2);
    }

    #[tokio::test]
    async fn pty_fd_duplication_survives_concurrent_read_write_close() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("sleep".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![Value::String("30".into())]),
            ),
        ]))
        .await
        .expect("start PTY");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        let read = tokio::spawn(read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(1_000.into()),
            ),
        ])));
        let write = tokio::spawn(write_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("data".into()),
                Value::Binary(b"concurrent".to_vec()),
            ),
        ])));
        tokio::task::yield_now().await;
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close PTY");

        let read_result = tokio::time::timeout(std::time::Duration::from_secs(1), read)
            .await
            .expect("duplicated read should finish")
            .expect("read task should join");
        let write_result = tokio::time::timeout(std::time::Duration::from_secs(1), write)
            .await
            .expect("duplicated write should finish")
            .expect("write task should join");
        assert!(matches!(
            read_result,
            Ok(_)
                | Err(RpcError {
                    code: RpcError::PROCESS_ERROR,
                    ..
                })
        ));
        assert!(matches!(
            write_result,
            Ok(_)
                | Err(RpcError {
                    code: RpcError::PROCESS_ERROR,
                    ..
                })
        ));
        assert!(test_managed_maps_empty().await);
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn pty_read_close_race_does_not_leak_duplicated_fds() {
        let _test_lock = test_process_map_lock().await;

        for iteration in 0..100 {
            let pid = u32::MAX - iteration;
            let mut fds = [-1; 2];
            assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "create pipe");
            let read_end = unsafe { OwnedFd::from_raw_fd(fds[0]) };
            let write_end = unsafe { OwnedFd::from_raw_fd(fds[1]) };
            drop(read_end);
            set_fd_nonblocking(write_end.as_raw_fd()).expect("make test fd nonblocking");
            let fd_target = std::fs::read_link(format!("/proc/self/fd/{}", write_end.as_raw_fd()))
                .expect("resolve test fd target");

            let lifecycle = Arc::new(Mutex::new(()));
            let lifecycle_guard = lifecycle.lock().await;
            get_pty_process_map().lock().await.insert(
                pid,
                ManagedPtyProcess {
                    async_fd: AsyncFd::new(write_end).expect("monitor test fd"),
                    lifecycle: lifecycle.clone(),
                    io: Arc::new(PtyIoState {
                        write_lock: Mutex::new(()),
                        syscall_lock: StdMutex::new(()),
                        closed: AtomicBool::new(false),
                        cancelled: Notify::new(),
                    }),
                    child_pid: Pid::from_raw(-1),
                    cmd: String::new(),
                    exit_status: None,
                    output_eof: false,
                },
            );
            assert_eq!(fd_target_count(&fd_target), 1);
            let reader = tokio::spawn(read_pty_now(pid, 1));

            for _ in 0..100 {
                if fd_target_count(&fd_target) == 2 {
                    break;
                }
                tokio::task::yield_now().await;
            }
            assert_eq!(
                fd_target_count(&fd_target),
                2,
                "read did not duplicate the PTY descriptor"
            );
            assert!(get_pty_process_map().lock().await.remove(&pid).is_some());
            drop(lifecycle_guard);

            let result = tokio::time::timeout(std::time::Duration::from_secs(1), reader)
                .await
                .expect("read should finish after PTY removal")
                .expect("read task should join")
                .expect("read should report removed PTY");
            assert!(result.exited);
            assert_eq!(
                fd_target_count(&fd_target),
                0,
                "iteration {iteration} leaked an fd"
            );
        }
    }

    #[tokio::test]
    async fn pty_write_completes_large_data_under_backpressure() {
        let _test_lock = test_process_map_lock().await;
        let start = start_pty(Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/sleep".into()),
            ),
            (
                Value::String("args".into()),
                Value::Array(vec![Value::String("30".into())]),
            ),
        ]))
        .await
        .expect("start backpressure PTY");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        let read_fd = install_full_pipe_pty(pid).await;

        // Remove the synthetic fill without sleeping; the writer starts with
        // no available capacity and must therefore exercise partial writes.
        let mut scratch = [0u8; 8192];
        loop {
            let read = unsafe {
                libc::read(
                    read_fd.as_raw_fd(),
                    scratch.as_mut_ptr() as *mut libc::c_void,
                    scratch.len(),
                )
            };
            if read < 0 {
                assert_eq!(
                    std::io::Error::last_os_error().raw_os_error(),
                    Some(libc::EAGAIN)
                );
                break;
            }
        }

        let reader_fd = AsyncFd::new(read_fd).expect("monitor backpressure reader");
        let data = vec![b'z'; 256 * 1024];
        let expected = data.clone();
        let reader_expected = expected.clone();
        let reader = tokio::spawn(async move {
            let mut output = Vec::with_capacity(reader_expected.len());
            while output.len() < reader_expected.len() {
                let mut guard = reader_fd
                    .readable()
                    .await
                    .expect("backpressure reader readiness");
                match guard.try_io(|inner| {
                    let mut chunk = [0u8; 8192];
                    let read = unsafe {
                        libc::read(
                            inner.get_ref().as_raw_fd(),
                            chunk.as_mut_ptr() as *mut libc::c_void,
                            chunk.len(),
                        )
                    };
                    if read >= 0 {
                        Ok(chunk[..read as usize].to_vec())
                    } else {
                        Err(std::io::Error::last_os_error())
                    }
                }) {
                    Ok(Ok(chunk)) => output.extend_from_slice(&chunk),
                    Ok(Err(error)) if error.kind() == ErrorKind::Interrupted => {}
                    Ok(Err(error)) => panic!("backpressure reader: {error}"),
                    Err(_) => {}
                }
            }
            output
        });
        let writer = tokio::spawn(write_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (Value::String("data".into()), Value::Binary(data)),
        ])));

        let result = tokio::time::timeout(std::time::Duration::from_secs(2), writer)
            .await
            .expect("large PTY write should complete")
            .expect("large PTY writer should join")
            .expect("large PTY write should succeed");
        assert_eq!(
            map_get(&result, "written").and_then(Value::as_u64),
            Some(262144)
        );
        let output = tokio::time::timeout(std::time::Duration::from_secs(2), reader)
            .await
            .expect("backpressure reader should complete")
            .expect("backpressure reader should join");
        assert_eq!(output, expected);
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close backpressure PTY");
    }

    #[tokio::test]
    async fn pty_blocked_write_does_not_hold_registry_for_kill_or_close() {
        let _test_lock = test_process_map_lock().await;
        let temp = tempfile::tempdir().expect("temporary PTY directory");

        for close in [false, true] {
            let marker = temp
                .path()
                .join(if close { "close-ready" } else { "kill-ready" });
            let pid = start_signal_ignoring_pty(&marker).await;
            let os_pid = pty_os_pid(pid).await;
            // Keep the read end open so the full synthetic write end remains
            // at EAGAIN while write_pty waits for WRITABLE readiness.
            let _pipe_reader = install_full_pipe_pty(pid).await;
            let mut blocked_write = tokio::spawn(write_pty(Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (Value::String("data".into()), Value::Binary(vec![b'x'])),
            ])));
            let write_wait =
                tokio::time::timeout(std::time::Duration::from_millis(100), &mut blocked_write)
                    .await;

            let lifecycle_result = if close {
                tokio::time::timeout(
                    std::time::Duration::from_secs(1),
                    close_pty(Value::Map(vec![(
                        Value::String("pid".into()),
                        Value::Integer(pid.into()),
                    )])),
                )
                .await
            } else {
                tokio::time::timeout(
                    std::time::Duration::from_secs(1),
                    kill_pty(Value::Map(vec![
                        (Value::String("pid".into()), Value::Integer(pid.into())),
                        (
                            Value::String("signal".into()),
                            Value::Integer((libc::SIGKILL as i64).into()),
                        ),
                    ])),
                )
                .await
            };

            let writer_result =
                tokio::time::timeout(std::time::Duration::from_secs(1), &mut blocked_write).await;
            // Remove the drain-preserving entry left by kill.  This is also
            // harmless after close, which is deliberately idempotent.
            let close_result = tokio::time::timeout(
                std::time::Duration::from_secs(1),
                close_pty(Value::Map(vec![(
                    Value::String("pid".into()),
                    Value::Integer(pid.into()),
                )])),
            )
            .await;

            assert!(write_wait.is_err(), "PTY write should wait for readiness");
            lifecycle_result
                .expect("PTY lifecycle operation should not wait for write")
                .expect("PTY lifecycle operation should succeed");
            match writer_result.expect("cancelled writer should join") {
                Ok(Err(error)) => assert_eq!(error.code, RpcError::PROCESS_ERROR),
                Err(_) => {}
                Ok(Ok(_)) => panic!("writer should be cancelled"),
            }
            close_result
                .expect("PTY cleanup should be bounded")
                .expect("PTY cleanup should succeed");
            assert_reaped(os_pid);
            assert!(!get_pty_process_map().lock().await.contains_key(&pid));
        }
    }

    #[tokio::test]
    async fn read_pty_rejects_oversized_max_bytes() {
        let error = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(1.into())),
            (
                Value::String("max_bytes".into()),
                Value::Integer(((MAX_PROCESS_READ_BYTES + 1) as u64).into()),
            ),
        ]))
        .await
        .expect_err("oversized PTY read should be rejected");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn start_pty_applies_env_without_mutating_process_env() {
        let _test_lock = test_process_map_lock().await;
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
