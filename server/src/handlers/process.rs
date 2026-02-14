//! Process execution operations

use crate::msgpack_map;
use crate::protocol::{from_value, ProcessResult, RpcError};
use nix::fcntl::{fcntl, FcntlArg, OFlag};
use nix::pty::{openpty, OpenptyResult};
use nix::sys::signal::Signal;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{close, dup2, execvp, fork, setsid, tcgetpgrp, ForkResult, Pid};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::ErrorKind;
use std::os::fd::{AsRawFd, BorrowedFd, RawFd};
use std::process::Stdio;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

use super::HandlerResult;

// ============================================================================
// Process management for async processes
// ============================================================================

use std::sync::OnceLock;

static PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedProcess>>> = OnceLock::new();
static PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

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
    #[allow(dead_code)]
    cmd: String,
}

// ============================================================================
// Synchronous process execution (but async-friendly)
// ============================================================================

/// Run a command and wait for it to complete
pub async fn run(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    let mut child = cmd.spawn().map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to spawn process: {}", e),
        data: None,
    })?;

    // Write stdin if provided (no base64 decoding needed!)
    if let Some(stdin_data) = params.stdin {
        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(&stdin_data).await;
        }
    }

    // Wait for process to complete (async!)
    let output = child.wait_with_output().await.map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to wait for process: {}", e),
        data: None,
    })?;

    // Return binary data directly (no encoding needed!)
    // On Unix, signal-killed processes have code() == None.
    // Convention: return 128 + signal_number (e.g. SIGKILL=9 -> 137).
    let exit_code = output.status.code().unwrap_or_else(|| {
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt;
            output.status.signal().map(|s| 128 + s).unwrap_or(-1)
        }
        #[cfg(not(unix))]
        {
            -1
        }
    });
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
pub async fn start(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    let child = cmd.spawn().map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to spawn process: {}", e),
        data: None,
    })?;

    let pid = get_next_pid().await;

    let managed = ManagedProcess {
        child,
        cmd: params.cmd.clone(),
    };

    get_process_map().lock().await.insert(pid, managed);

    Ok(msgpack_map! {
        "pid" => pid
    })
}

/// Write to an async process's stdin
pub async fn write(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    if let Some(stdin) = managed.child.stdin.as_mut() {
        stdin.write_all(&data).await.map_err(|e| RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Failed to write to stdin: {}", e),
            data: None,
        })?;
    }

    Ok(msgpack_map! {
        "written" => data.len()
    })
}

/// Read from an async process's stdout/stderr
pub async fn read(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let timeout = params.timeout_ms.unwrap_or(0);

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Try to read stdout (with optional blocking timeout)
    let stdout_data = if let Some(stdout) = managed.child.stdout.as_mut() {
        try_read_async_with_timeout(stdout, params.max_bytes, timeout).await
    } else {
        vec![]
    };

    // Try to read stderr (with optional blocking timeout)
    let stderr_data = if let Some(stderr) = managed.child.stderr.as_mut() {
        try_read_async_with_timeout(stderr, params.max_bytes, timeout).await
    } else {
        vec![]
    };

    // Check if process has exited
    let exit_status = managed.child.try_wait().ok().flatten();

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

    Ok(msgpack_map! {
        "stdout" => stdout_val,
        "stderr" => stderr_val,
        "exited" => exit_status.is_some(),
        "exit_code" => exit_status.map(|s| {
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                s.code().unwrap_or_else(|| s.signal().map(|sig| 128 + sig).unwrap_or(-1))
            }
            #[cfg(not(unix))]
            {
                s.code().unwrap_or(-1)
            }
        }).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

/// Try to read from an async reader with configurable timeout
async fn try_read_async_with_timeout<R: tokio::io::AsyncRead + Unpin>(
    reader: &mut R,
    max_bytes: usize,
    timeout_ms: u64,
) -> Vec<u8> {
    let mut buf = vec![0u8; max_bytes];
    let timeout = if timeout_ms == 0 { 1 } else { timeout_ms };

    match tokio::time::timeout(
        std::time::Duration::from_millis(timeout),
        reader.read(&mut buf),
    )
    .await
    {
        Ok(Ok(0)) => vec![], // EOF
        Ok(Ok(n)) => {
            buf.truncate(n);
            buf
        }
        Ok(Err(e)) if e.kind() == ErrorKind::WouldBlock => vec![],
        Ok(Err(_)) => vec![],
        Err(_) => vec![], // Timeout - no data available
    }
}

/// Close the stdin of an async process (signals EOF)
pub async fn close_stdin(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Drop stdin to close it
    managed.child.stdin.take();

    Ok(Value::Boolean(true))
}

/// Kill an async process
pub async fn kill(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Get the actual OS PID
    let os_pid = managed.child.id().ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: "Process has no PID (already exited?)".to_string(),
        data: None,
    })?;

    // Send the signal
    let result = unsafe { libc::kill(os_pid as i32, params.signal) };

    if result != 0 {
        return Err(RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Failed to send signal: {}", std::io::Error::last_os_error()),
            data: None,
        });
    }

    // If SIGKILL, remove from process map
    if params.signal == libc::SIGKILL {
        processes.remove(&params.pid);
    }

    Ok(Value::Boolean(true))
}

/// List all managed async processes
pub async fn list(_params: &Value) -> HandlerResult {
    let mut processes = get_process_map().lock().await;

    let list: Vec<Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            let exited = managed.child.try_wait().ok().flatten();
            msgpack_map! {
                "pid" => *pid,
                "os_pid" => managed.child.id().map(|id| Value::Integer((id as i64).into())).unwrap_or(Value::Nil),
                "cmd" => managed.cmd.clone(),
                "exited" => exited.is_some(),
                "exit_code" => exited.map(|s| {
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        s.code().unwrap_or_else(|| s.signal().map(|sig| 128 + sig).unwrap_or(-1))
                    }
                    #[cfg(not(unix))]
                    {
                        s.code().unwrap_or(-1)
                    }
                }).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
            }
        })
        .collect();

    Ok(Value::Array(list))
}

// ============================================================================
// PTY (Pseudo-Terminal) Process Management
// ============================================================================

use std::os::unix::io::{FromRawFd, OwnedFd};
use tokio::io::unix::AsyncFd;
use tokio::io::Interest;

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

fn set_fd_nonblocking(fd: RawFd) -> Result<(), nix::Error> {
    let flags = fcntl(fd, FcntlArg::F_GETFL)?;
    let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
    fcntl(fd, FcntlArg::F_SETFL(new_flags))?;
    Ok(())
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
    let OpenptyResult { master, slave } = openpty(None, None).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to open PTY: {}", e),
        data: None,
    })?;

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
            return Err(RpcError {
                code: RpcError::PROCESS_ERROR,
                message: format!(
                    "Failed to get tty name: {}",
                    std::io::Error::from_raw_os_error(ret)
                ),
                data: None,
            });
        }
        let nul_pos = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
        String::from_utf8_lossy(&buf[..nul_pos]).into_owned()
    };

    set_window_size(master.as_raw_fd(), params.rows, params.cols).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to set window size: {}", e),
        data: None,
    })?;

    let cmd_cstring = CString::new(params.cmd.clone()).map_err(|e| RpcError {
        code: RpcError::INVALID_PARAMS,
        message: format!("Invalid command: {}", e),
        data: None,
    })?;

    let mut args_cstrings: Vec<CString> = vec![cmd_cstring.clone()];
    for arg in &params.args {
        args_cstrings.push(CString::new(arg.clone()).map_err(|e| RpcError {
            code: RpcError::INVALID_PARAMS,
            message: format!("Invalid argument: {}", e),
            data: None,
        })?);
    }

    match unsafe { fork() } {
        Ok(ForkResult::Child) => {
            let _ = close(master.as_raw_fd());
            let _ = setsid();
            unsafe {
                libc::ioctl(slave.as_raw_fd(), libc::TIOCSCTTY as _, 0);
            }
            let _ = dup2(slave.as_raw_fd(), 0);
            let _ = dup2(slave.as_raw_fd(), 1);
            let _ = dup2(slave.as_raw_fd(), 2);
            if slave.as_raw_fd() > 2 {
                let _ = close(slave.as_raw_fd());
            }
            if let Some(cwd) = &params.cwd {
                let _ = std::env::set_current_dir(super::expand_tilde(cwd));
            }
            if params.clear_env {
                for (key, _) in std::env::vars() {
                    std::env::remove_var(key);
                }
            }
            if let Some(env) = &params.env {
                for (key, value) in env {
                    std::env::set_var(key, value);
                }
            }
            let _ = execvp(&cmd_cstring, &args_cstrings);
            std::process::exit(127);
        }
        Ok(ForkResult::Parent { child }) => {
            drop(slave);
            let master_fd = master.as_raw_fd();
            std::mem::forget(master);
            Ok(ForkResult2 {
                master_fd,
                child_pid: child,
                tty_name,
            })
        }
        Err(e) => Err(RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Failed to fork: {}", e),
            data: None,
        }),
    }
}

/// Start a process with a PTY (pseudo-terminal)
pub async fn start_pty(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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
        .map_err(|e| RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Task join error: {}", e),
            data: None,
        })??;

    set_fd_nonblocking(fork_result.master_fd).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to set non-blocking: {}", e),
        data: None,
    })?;

    let owned_fd = unsafe { OwnedFd::from_raw_fd(fork_result.master_fd) };
    let async_fd = AsyncFd::new(owned_fd).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to create AsyncFd: {}", e),
        data: None,
    })?;

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
pub async fn resize_pty(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        rows: u16,
        cols: u16,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let processes = get_pty_process_map().lock().await;
    let managed = processes.get(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("PTY process not found: {}", params.pid),
        data: None,
    })?;

    let fd = managed.async_fd.get_ref().as_raw_fd();

    set_window_size(fd, params.rows, params.cols).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to resize PTY: {}", e),
        data: None,
    })?;

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
pub async fn read_pty(params: &Value) -> HandlerResult {
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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
pub async fn write_pty(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    let processes = get_pty_process_map().lock().await;
    let managed = processes.get(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("PTY process not found: {}", params.pid),
        data: None,
    })?;

    let mut guard = managed
        .async_fd
        .ready(Interest::WRITABLE)
        .await
        .map_err(|e| RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Failed to wait for writable: {}", e),
            data: None,
        })?;

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
            return Err(RpcError {
                code: RpcError::PROCESS_ERROR,
                message: format!("Failed to write to PTY: {}", e),
                data: None,
            })
        }
        Err(_would_block) => 0,
    };

    Ok(msgpack_map! {
        "written" => written
    })
}

/// Kill a PTY process
pub async fn kill_pty(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_pty_signal")]
        signal: i32,
    }

    fn default_pty_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;
    let managed = processes.get(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("PTY process not found: {}", params.pid),
        data: None,
    })?;

    let signal = Signal::try_from(params.signal).map_err(|_| RpcError {
        code: RpcError::INVALID_PARAMS,
        message: format!("Invalid signal: {}", params.signal),
        data: None,
    })?;

    nix::sys::signal::kill(managed.child_pid, signal).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to send signal: {}", e),
        data: None,
    })?;

    if params.signal == libc::SIGKILL {
        processes.remove(&params.pid);
    }

    Ok(Value::Boolean(true))
}

/// Close a PTY process and clean up
pub async fn close_pty(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;

    if let Some(managed) = processes.remove(&params.pid) {
        let _ = nix::sys::signal::kill(managed.child_pid, Signal::SIGKILL);
        Ok(Value::Boolean(true))
    } else {
        Err(RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("PTY process not found: {}", params.pid),
            data: None,
        })
    }
}

/// List all PTY processes
pub async fn list_pty(_params: &Value) -> HandlerResult {
    let mut processes = get_pty_process_map().lock().await;

    let list: Vec<Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            let (exited, exit_code) = if managed.exit_status.is_some() {
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
                    _ => (false, None),
                }
            };

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
