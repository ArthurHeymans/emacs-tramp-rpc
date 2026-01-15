//! Process execution operations

use crate::protocol::{ProcessResult, RpcError};
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use serde::Deserialize;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::process::{Child, Command, Stdio};
use std::sync::Mutex;

type HandlerResult = Result<serde_json::Value, RpcError>;

// ============================================================================
// Process management for async processes
// ============================================================================

use std::sync::OnceLock;

static PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedProcess>>> = OnceLock::new();
static PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

fn get_process_map() -> &'static Mutex<HashMap<u32, ManagedProcess>> {
    PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

fn get_next_pid() -> u32 {
    let counter = PID_COUNTER.get_or_init(|| Mutex::new(1));
    let mut pid = counter.lock().unwrap();
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
// Synchronous process execution
// ============================================================================

/// Run a command and wait for it to complete
pub fn run(params: &serde_json::Value) -> HandlerResult {
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
            let _ = stdin.write_all(&decoded);
        }
    }

    // Wait for process to complete
    let output = child.wait_with_output().map_err(|e| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to wait for process: {}", e),
        data: None,
    })?;

    let result = ProcessResult {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: BASE64.encode(&output.stdout),
        stderr: BASE64.encode(&output.stderr),
    };

    Ok(serde_json::to_value(result).unwrap())
}

// ============================================================================
// Asynchronous process management
// ============================================================================

/// Start an async process
pub fn start(params: &serde_json::Value) -> HandlerResult {
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

    let pid = get_next_pid();

    let managed = ManagedProcess {
        child,
        cmd: params.cmd.clone(),
    };

    get_process_map().lock().unwrap().insert(pid, managed);

    Ok(serde_json::json!({
        "pid": pid
    }))
}

/// Write to an async process's stdin
pub fn write(params: &serde_json::Value) -> HandlerResult {
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

    let mut processes = get_process_map().lock().unwrap();
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    if let Some(stdin) = managed.child.stdin.as_mut() {
        stdin.write_all(&decoded).map_err(|e| RpcError {
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
pub fn read(params: &serde_json::Value) -> HandlerResult {
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

    let mut processes = get_process_map().lock().unwrap();
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Try to read stdout
    let mut stdout_buf = vec![0u8; params.max_bytes];
    let stdout_read = if let Some(stdout) = managed.child.stdout.as_mut() {
        // Use non-blocking read if possible
        match stdout.read(&mut stdout_buf) {
            Ok(n) => {
                stdout_buf.truncate(n);
                Some(BASE64.encode(&stdout_buf))
            }
            Err(_) => None,
        }
    } else {
        None
    };

    // Try to read stderr
    let mut stderr_buf = vec![0u8; params.max_bytes];
    let stderr_read = if let Some(stderr) = managed.child.stderr.as_mut() {
        match stderr.read(&mut stderr_buf) {
            Ok(n) => {
                stderr_buf.truncate(n);
                Some(BASE64.encode(&stderr_buf))
            }
            Err(_) => None,
        }
    } else {
        None
    };

    // Check if process has exited
    let exit_status = managed.child.try_wait().ok().flatten();

    Ok(serde_json::json!({
        "stdout": stdout_read,
        "stderr": stderr_read,
        "exited": exit_status.is_some(),
        "exit_code": exit_status.and_then(|s| s.code())
    }))
}

/// Kill an async process
pub fn kill(params: &serde_json::Value) -> HandlerResult {
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

    let mut processes = get_process_map().lock().unwrap();
    let managed = processes.get_mut(&params.pid).ok_or_else(|| RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Process not found: {}", params.pid),
        data: None,
    })?;

    // Get the actual OS PID
    let os_pid = managed.child.id();

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
pub fn list(_params: &serde_json::Value) -> HandlerResult {
    let mut processes = get_process_map().lock().unwrap();

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
