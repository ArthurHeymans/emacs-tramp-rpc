//! Process execution operations

use crate::protocol::{OutputEncoding, ProcessResult, RpcError};
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
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
