//! Process execution operations

use crate::protocol::{OutputEncoding, ProcessResult, RpcError};
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use nix::fcntl::{fcntl, FcntlArg, OFlag};
use nix::pty::{openpty, OpenptyResult};
use nix::sys::signal::Signal;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{close, dup2, execvp, fork, setsid, tcgetpgrp, ForkResult, Pid};
use serde::Deserialize;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::ErrorKind;
use std::os::fd::{AsRawFd, BorrowedFd, RawFd};
use std::process::Stdio;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

type HandlerResult = Result<serde_json::Value, RpcError>;

/// Encode bytes smartly: use raw text if valid UTF-8, otherwise base64.
/// Returns (encoded_string, encoding_type).
fn smart_encode(data: &[u8]) -> (String, OutputEncoding) {
    // Try to interpret as UTF-8
    match std::str::from_utf8(data) {
        Ok(text) => {
            // Valid UTF-8 - use raw text (serde_json will escape as needed)
            (text.to_string(), OutputEncoding::Text)
        }
        Err(_) => {
            // Not valid UTF-8 - use base64
            (BASE64.encode(data), OutputEncoding::Base64)
        }
    }
}

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
pub async fn run(params: &serde_json::Value) -> HandlerResult {
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
        /// Base64-encoded stdin input
        #[serde(default)]
        stdin: Option<String>,
        /// Clear environment before setting env vars
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(cwd);
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

    // Write stdin if provided
    if let Some(stdin_data) = params.stdin {
        let decoded = BASE64
            .decode(&stdin_data)
            .map_err(|e| RpcError::invalid_params(format!("Invalid base64 stdin: {}", e)))?;

        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(&decoded).await;
        }
    }

    // Wait for process to complete (async!)
    let output = child.wait_with_output().await.map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to wait for process: {}", e),
        data: None,
    })?;

    // Smart encode: use text if valid UTF-8, base64 otherwise
    let (stdout, stdout_encoding) = smart_encode(&output.stdout);
    let (stderr, stderr_encoding) = smart_encode(&output.stderr);

    let result = ProcessResult {
        exit_code: output.status.code().unwrap_or(-1),
        stdout,
        stderr,
        stdout_encoding,
        stderr_encoding,
    };

    Ok(serde_json::to_value(result).unwrap())
}

// ============================================================================
// Asynchronous process management
// ============================================================================

/// Start an async process
pub async fn start(params: &serde_json::Value) -> HandlerResult {
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

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(cwd);
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

    Ok(serde_json::json!({
        "pid": pid
    }))
}

/// Write to an async process's stdin
pub async fn write(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Base64-encoded data to write
        data: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let decoded = BASE64
        .decode(&params.data)
        .map_err(|e| RpcError::invalid_params(format!("Invalid base64: {}", e)))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    if let Some(stdin) = managed.child.stdin.as_mut() {
        stdin.write_all(&decoded).await.map_err(|e| RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Failed to write to stdin: {}", e),
            data: None,
        })?;
    }

    Ok(serde_json::json!({
        "written": decoded.len()
    }))
}

/// Read from an async process's stdout/stderr
pub async fn read(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Maximum bytes to read
        #[serde(default = "default_max_read")]
        max_bytes: usize,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Try to read stdout (non-blocking with timeout)
    let stdout_data = if let Some(stdout) = managed.child.stdout.as_mut() {
        try_read_async(stdout, params.max_bytes).await
    } else {
        vec![]
    };

    // Try to read stderr (non-blocking with timeout)
    let stderr_data = if let Some(stderr) = managed.child.stderr.as_mut() {
        try_read_async(stderr, params.max_bytes).await
    } else {
        vec![]
    };

    // Check if process has exited
    let exit_status = managed.child.try_wait().ok().flatten();

    // Smart encode stdout and stderr
    let (stdout_val, stdout_enc) = if stdout_data.is_empty() {
        (serde_json::Value::Null, None)
    } else {
        let (s, enc) = smart_encode(&stdout_data);
        (serde_json::Value::String(s), Some(enc))
    };

    let (stderr_val, stderr_enc) = if stderr_data.is_empty() {
        (serde_json::Value::Null, None)
    } else {
        let (s, enc) = smart_encode(&stderr_data);
        (serde_json::Value::String(s), Some(enc))
    };

    Ok(serde_json::json!({
        "stdout": stdout_val,
        "stderr": stderr_val,
        "stdout_encoding": stdout_enc,
        "stderr_encoding": stderr_enc,
        "exited": exit_status.is_some(),
        "exit_code": exit_status.and_then(|s| s.code())
    }))
}

/// Try to read from an async reader with a very short timeout
async fn try_read_async<R: tokio::io::AsyncRead + Unpin>(
    reader: &mut R,
    max_bytes: usize,
) -> Vec<u8> {
    let mut buf = vec![0u8; max_bytes];

    // Use a very short timeout to make this effectively non-blocking
    match tokio::time::timeout(std::time::Duration::from_millis(1), reader.read(&mut buf)).await {
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
pub async fn close_stdin(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_process_map().lock().await;
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Drop stdin to close it
    managed.child.stdin.take();

    Ok(serde_json::json!(true))
}

/// Kill an async process
pub async fn kill(params: &serde_json::Value) -> HandlerResult {
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

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    Ok(serde_json::json!(true))
}

/// List all managed async processes
pub async fn list(_params: &serde_json::Value) -> HandlerResult {
    let mut processes = get_process_map().lock().await;

    let list: Vec<serde_json::Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            let exited = managed.child.try_wait().ok().flatten();
            serde_json::json!({
                "pid": pid,
                "os_pid": managed.child.id(),
                "cmd": managed.cmd,
                "exited": exited.is_some(),
                "exit_code": exited.and_then(|s| s.code())
            })
        })
        .collect();

    Ok(serde_json::to_value(list).unwrap())
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
    let counter = PTY_PID_COUNTER.get_or_init(|| Mutex::new(10000)); // Start at 10000 to distinguish from regular PIDs
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

struct ManagedPtyProcess {
    /// The master file descriptor for the PTY (wrapped for async I/O)
    async_fd: AsyncFd<OwnedFd>,
    /// The child process PID
    child_pid: Pid,
    /// Command that was run
    cmd: String,
    /// Cached exit status (if process has exited)
    exit_status: Option<i32>,
}

/// Set a file descriptor to non-blocking mode using nix
fn set_fd_nonblocking(fd: RawFd) -> Result<(), nix::Error> {
    let flags = fcntl(fd, FcntlArg::F_GETFL)?;
    let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
    fcntl(fd, FcntlArg::F_SETFL(new_flags))?;
    Ok(())
}

/// Set terminal window size
fn set_window_size(fd: RawFd, rows: u16, cols: u16) -> Result<(), std::io::Error> {
    let ws = libc::winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    let result = unsafe { libc::ioctl(fd, libc::TIOCSWINSZ, &ws) };
    if result < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

/// Parameters for starting a PTY process (used across thread boundary)
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

/// Result of fork operation
struct ForkResult2 {
    master_fd: RawFd,
    child_pid: Pid,
    tty_name: String,
}

/// Perform the fork/exec in a blocking context
fn do_fork_exec(params: PtyStartParams) -> Result<ForkResult2, RpcError> {
    // Open a PTY pair
    let OpenptyResult { master, slave } = openpty(None, None).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to open PTY: {}", e),
        data: None,
    })?;

    // Get the tty name from the slave fd before forking
    let tty_name = {
        let mut buf = vec![0u8; 256];
        let ret =
            unsafe { libc::ttyname_r(slave.as_raw_fd(), buf.as_mut_ptr() as *mut i8, buf.len()) };
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

    // Set initial window size
    set_window_size(master.as_raw_fd(), params.rows, params.cols).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to set window size: {}", e),
        data: None,
    })?;

    // Prepare command and args for execvp
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

    // Fork
    match unsafe { fork() } {
        Ok(ForkResult::Child) => {
            // Child process
            // Close master side
            let _ = close(master.as_raw_fd());

            // Create new session and set controlling terminal
            let _ = setsid();

            // Set the slave as controlling terminal
            unsafe {
                libc::ioctl(slave.as_raw_fd(), libc::TIOCSCTTY, 0);
            }

            // Duplicate slave to stdin, stdout, stderr
            let _ = dup2(slave.as_raw_fd(), 0);
            let _ = dup2(slave.as_raw_fd(), 1);
            let _ = dup2(slave.as_raw_fd(), 2);

            // Close the original slave fd if it's not one of 0, 1, 2
            if slave.as_raw_fd() > 2 {
                let _ = close(slave.as_raw_fd());
            }

            // Change directory if specified
            if let Some(cwd) = &params.cwd {
                let _ = std::env::set_current_dir(cwd);
            }

            // Set environment variables
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

            // Execute the command
            let _ = execvp(&cmd_cstring, &args_cstrings);

            // If execvp returns, it failed
            std::process::exit(127);
        }
        Ok(ForkResult::Parent { child }) => {
            // Parent process
            // Close slave side - we don't need it
            drop(slave);

            let master_fd = master.as_raw_fd();

            // Prevent the OwnedFd from closing the fd when dropped
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
pub async fn start_pty(params: &serde_json::Value) -> HandlerResult {
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
        /// Terminal rows (default 24)
        #[serde(default = "default_rows")]
        rows: u16,
        /// Terminal columns (default 80)
        #[serde(default = "default_cols")]
        cols: u16,
    }

    fn default_rows() -> u16 {
        24
    }
    fn default_cols() -> u16 {
        80
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let start_params = PtyStartParams {
        cmd: params.cmd.clone(),
        args: params.args,
        cwd: params.cwd,
        env: params.env,
        clear_env: params.clear_env,
        rows: params.rows,
        cols: params.cols,
    };

    // Run fork/exec in a blocking task to avoid blocking the async runtime
    let fork_result = tokio::task::spawn_blocking(move || do_fork_exec(start_params))
        .await
        .map_err(|e| RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("Task join error: {}", e),
            data: None,
        })??;

    // Set non-blocking mode for async I/O
    set_fd_nonblocking(fork_result.master_fd).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to set non-blocking: {}", e),
        data: None,
    })?;

    // Wrap the fd in OwnedFd and AsyncFd for async I/O
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

    Ok(serde_json::json!({
        "pid": our_pid,
        "os_pid": fork_result.child_pid.as_raw(),
        "tty_name": fork_result.tty_name
    }))
}

/// Resize a PTY terminal
pub async fn resize_pty(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        rows: u16,
        cols: u16,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    // Get the foreground process group and send SIGWINCH to it
    // This ensures the signal reaches the currently active process (e.g., bash at prompt)
    // rather than just the shell's process group
    // SAFETY: fd is valid for the duration of this call as we hold the lock on the process map
    match tcgetpgrp(unsafe { BorrowedFd::borrow_raw(fd) }) {
        Ok(fg_pgrp) => {
            // Send to the foreground process group (negative PID = process group)
            let _ = nix::sys::signal::kill(Pid::from_raw(-fg_pgrp.as_raw()), Signal::SIGWINCH);
        }
        Err(_) => {
            // Fallback: send to the original child's process group
            let _ = nix::sys::signal::kill(
                Pid::from_raw(-managed.child_pid.as_raw()),
                Signal::SIGWINCH,
            );
        }
    }

    Ok(serde_json::json!(true))
}

/// Read from a PTY process with optional blocking
pub async fn read_pty(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_max_read")]
        max_bytes: usize,
        /// Timeout in milliseconds to wait for data. If 0 or not specified, returns immediately.
        #[serde(default)]
        timeout_ms: Option<u64>,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let timeout = params.timeout_ms.unwrap_or(0);
    let mut buf = vec![0u8; params.max_bytes];

    // Try to read, optionally waiting for data
    let read_result = {
        let mut processes = get_pty_process_map().lock().await;
        let managed = match processes.get_mut(&params.pid) {
            Some(m) => m,
            None => {
                return Ok(serde_json::json!({
                    "output": serde_json::Value::Null,
                    "output_encoding": serde_json::Value::Null,
                    "exited": true,
                    "exit_code": serde_json::Value::Null
                }));
            }
        };

        // First try non-blocking read
        let fd = managed.async_fd.get_ref().as_raw_fd();
        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };

        if n > 0 {
            // Got data immediately
            buf.truncate(n as usize);
            Some((buf.clone(), false, None))
        } else if timeout == 0 {
            // No data and no timeout requested
            Some((vec![], false, None))
        } else {
            // No data - we need to wait, but first check exit status
            let (exited, exit_code) = check_exit_status(managed);
            if exited {
                Some((vec![], true, exit_code))
            } else {
                None // Signal that we need to wait
            }
        }
    };

    // If we got a result, return it
    if let Some((output, exited, exit_code)) = read_result {
        // Check exit status if not already exited
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

        let (output_val, output_enc) = if output.is_empty() {
            (serde_json::Value::Null, None)
        } else {
            let (s, enc) = smart_encode(&output);
            (serde_json::Value::String(s), Some(enc))
        };

        return Ok(serde_json::json!({
            "output": output_val,
            "output_encoding": output_enc,
            "exited": exited,
            "exit_code": exit_code
        }));
    }

    // Need to wait for data - use async wait with timeout
    let wait_result = tokio::time::timeout(
        std::time::Duration::from_millis(timeout),
        wait_for_pty_readable(params.pid),
    )
    .await;

    // After waiting, try to read again
    let mut processes = get_pty_process_map().lock().await;
    let managed = match processes.get_mut(&params.pid) {
        Some(m) => m,
        None => {
            return Ok(serde_json::json!({
                "output": serde_json::Value::Null,
                "output_encoding": serde_json::Value::Null,
                "exited": true,
                "exit_code": serde_json::Value::Null
            }));
        }
    };

    let output = if wait_result.is_ok() {
        // Wait succeeded, try to read
        let fd = managed.async_fd.get_ref().as_raw_fd();
        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        if n > 0 {
            buf.truncate(n as usize);
            buf
        } else {
            vec![]
        }
    } else {
        // Timeout - return empty
        vec![]
    };

    let (exited, exit_code) = check_exit_status(managed);

    let (output_val, output_enc) = if output.is_empty() {
        (serde_json::Value::Null, None)
    } else {
        let (s, enc) = smart_encode(&output);
        (serde_json::Value::String(s), Some(enc))
    };

    Ok(serde_json::json!({
        "output": output_val,
        "output_encoding": output_enc,
        "exited": exited,
        "exit_code": exit_code
    }))
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
    // Get the raw fd without holding the lock long
    let fd = {
        let processes = get_pty_process_map().lock().await;
        match processes.get(&pid) {
            Some(m) => m.async_fd.get_ref().as_raw_fd(),
            None => return false,
        }
    };

    // Loop polling until data is available (outer timeout will cancel us)
    loop {
        let ready = tokio::task::spawn_blocking(move || {
            let mut pollfd = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            // Poll with 100ms timeout
            let ret = unsafe { libc::poll(&mut pollfd, 1, 100) };
            ret > 0 && (pollfd.revents & libc::POLLIN) != 0
        })
        .await
        .unwrap_or(false);

        if ready {
            return true;
        }

        // Check if process still exists before looping
        let processes = get_pty_process_map().lock().await;
        if !processes.contains_key(&pid) {
            return false;
        }
        // Small yield to avoid busy spinning
        tokio::task::yield_now().await;
    }
}

/// Write to a PTY process (async)
pub async fn write_pty(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Base64-encoded data to write
        data: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let decoded = BASE64
        .decode(&params.data)
        .map_err(|e| RpcError::invalid_params(format!("Invalid base64: {}", e)))?;

    let processes = get_pty_process_map().lock().await;
    let managed = processes.get(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("PTY process not found: {}", params.pid),
        data: None,
    })?;

    // Wait for writable and write
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
                decoded.as_ptr() as *const libc::c_void,
                decoded.len(),
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

    Ok(serde_json::json!({
        "written": written
    }))
}

/// Kill a PTY process
pub async fn kill_pty(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_pty_signal")]
        signal: i32,
    }

    fn default_pty_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;
    let managed = processes.get(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("PTY process not found: {}", params.pid),
        data: None,
    })?;

    // Convert signal number to Signal enum
    let signal = Signal::try_from(params.signal).map_err(|_| RpcError {
        code: RpcError::INVALID_PARAMS,
        message: format!("Invalid signal: {}", params.signal),
        data: None,
    })?;

    // Send signal to the process
    nix::sys::signal::kill(managed.child_pid, signal).map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to send signal: {}", e),
        data: None,
    })?;

    // If SIGKILL, also close and remove
    if params.signal == libc::SIGKILL {
        processes.remove(&params.pid);
        // AsyncFd and OwnedFd will be dropped, closing the fd
    }

    Ok(serde_json::json!(true))
}

/// Close a PTY process and clean up
pub async fn close_pty(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut processes = get_pty_process_map().lock().await;

    if let Some(managed) = processes.remove(&params.pid) {
        // Kill the process if still running
        let _ = nix::sys::signal::kill(managed.child_pid, Signal::SIGKILL);
        // AsyncFd and OwnedFd will be dropped, closing the fd
        Ok(serde_json::json!(true))
    } else {
        Err(RpcError {
            code: RpcError::PROCESS_ERROR,
            message: format!("PTY process not found: {}", params.pid),
            data: None,
        })
    }
}

/// List all PTY processes
pub async fn list_pty(_params: &serde_json::Value) -> HandlerResult {
    let mut processes = get_pty_process_map().lock().await;

    let list: Vec<serde_json::Value> = processes
        .iter_mut()
        .map(|(pid, managed)| {
            // Check if process has exited
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

            serde_json::json!({
                "pid": pid,
                "os_pid": managed.child_pid.as_raw(),
                "cmd": managed.cmd,
                "exited": exited,
                "exit_code": exit_code
            })
        })
        .collect();

    Ok(serde_json::to_value(list).unwrap())
}
