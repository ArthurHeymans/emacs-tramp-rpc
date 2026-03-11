//! TRAMP-RPC Server
//!
//! A MessagePack-RPC server for TRAMP remote file access.
//! Communicates over stdin/stdout using length-prefixed MessagePack messages.
//!
//! Protocol framing:
//!   <4-byte big-endian length><msgpack payload>
//!
//! Uses tokio for async concurrent request processing - multiple requests
//! can be processed in parallel while waiting on I/O.

mod handlers;
mod protocol;
mod watcher;

use protocol::{Request, Response, RpcError};
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt, BufWriter};
use tokio::sync::Mutex;
use tokio::task::JoinSet;

/// Shared handle to the stdout writer, used by both response writing
/// and the watcher's notification sending.
pub type WriterHandle = Arc<Mutex<BufWriter<tokio::io::Stdout>>>;

#[tokio::main]
async fn main() {
    let mut stdin = tokio::io::stdin();
    let stdout: WriterHandle = Arc::new(Mutex::new(BufWriter::new(tokio::io::stdout())));

    // Initialize the filesystem watcher for cache invalidation notifications.
    // If this fails (e.g. inotify not available), we continue without watching.
    // NOTE: Do NOT use eprintln! here or anywhere in the server -- SSH forwards
    // the remote process's stderr over the same pipe to Emacs, where it gets
    // mixed with the binary msgpack protocol on stdout and corrupts framing.
    if let Ok(manager) = watcher::WatchManager::new(Arc::clone(&stdout)) {
        watcher::init(manager);
    }

    let mut tasks: JoinSet<()> = JoinSet::new();

    // Process requests concurrently
    loop {
        // Read 4-byte length prefix (big-endian)
        let mut len_buf = [0u8; 4];
        if stdin.read_exact(&mut len_buf).await.is_err() {
            break; // EOF or error
        }
        let len = u32::from_be_bytes(len_buf) as usize;

        // Sanity check - reject obviously invalid lengths
        if len > 100 * 1024 * 1024 {
            // 100MB max message size - drain the payload to keep framing in sync
            // (cannot use eprintln! as SSH merges stderr with stdout)
            let mut discard = vec![0u8; 8192];
            let mut remaining = len;
            while remaining > 0 {
                let to_read = remaining.min(discard.len());
                if stdin.read_exact(&mut discard[..to_read]).await.is_err() {
                    break;
                }
                remaining -= to_read;
            }
            continue;
        }

        // Read payload
        let mut payload = vec![0u8; len];
        if stdin.read_exact(&mut payload).await.is_err() {
            break; // EOF or error
        }

        // Clone writer for this task
        let writer = Arc::clone(&stdout);

        // Spawn a task for each request - allows concurrent processing
        tasks.spawn(async move {
            let response = process_request(&payload).await;

            // Serialize response with MessagePack
            if let Ok(msgpack_bytes) = rmp_serde::to_vec_named(&response) {
                let mut writer = writer.lock().await;
                // Write length prefix
                let len_bytes = (msgpack_bytes.len() as u32).to_be_bytes();
                let _ = writer.write_all(&len_bytes).await;
                // Write payload
                let _ = writer.write_all(&msgpack_bytes).await;
                let _ = writer.flush().await;
            }
        });
    }

    // Wait for all pending tasks to complete before exiting
    while tasks.join_next().await.is_some() {}
}

async fn process_request(payload: &[u8]) -> Response {
    // Parse the request from MessagePack
    let request: Request = match rmp_serde::from_slice(payload) {
        Ok(r) => r,
        Err(e) => {
            return Response::error(None, RpcError::parse_error(e.to_string()));
        }
    };

    // Validate RPC version
    if request.version != "2.0" {
        return Response::error(
            Some(request.id),
            RpcError::invalid_request("Invalid RPC version"),
        );
    }

    // Dispatch to handler
    handlers::dispatch(request).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmpv::Value;

    fn make_request(method: &str, params: Value) -> Vec<u8> {
        let request = rmpv::Value::Map(vec![
            (Value::String("version".into()), Value::String("2.0".into())),
            (Value::String("id".into()), Value::Integer(1.into())),
            (Value::String("method".into()), Value::String(method.into())),
            (Value::String("params".into()), params),
        ]);
        rmp_serde::to_vec_named(&request).unwrap()
    }

    #[tokio::test]
    async fn test_parse_request() {
        let params = Value::Map(vec![(
            Value::String("path".into()),
            Value::String("/tmp".into()),
        )]);
        let payload = make_request("file.stat", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none());
    }

    #[tokio::test]
    async fn test_invalid_msgpack() {
        let response = process_request(b"not msgpack").await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::PARSE_ERROR);
    }

    #[tokio::test]
    async fn test_method_not_found() {
        let params = Value::Map(vec![]);
        let payload = make_request("nonexistent.method", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::METHOD_NOT_FOUND);
    }

    /// Test that process.run returns 128+signal for signal-killed processes.
    /// This is required by Emacs `process-file' (tramp-test28-process-file).
    #[tokio::test]
    async fn test_process_run_signal_exit_code() {
        // SIGINT (signal 2) -> expect exit code 130
        let params = Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/sh".into()),
            ),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("kill -2 $$".into()),
                ]),
            ),
            (
                Value::String("cwd".into()),
                Value::String("/tmp".into()),
            ),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(exit_code, 130, "SIGINT should produce exit code 128+2=130");
    }

    /// Test that process.run returns 128+signal for SIGKILL.
    #[tokio::test]
    async fn test_process_run_sigkill_exit_code() {
        // SIGKILL (signal 9) -> expect exit code 137
        let params = Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/sh".into()),
            ),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("kill -9 $$".into()),
                ]),
            ),
            (
                Value::String("cwd".into()),
                Value::String("/tmp".into()),
            ),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(
            exit_code, 137,
            "SIGKILL should produce exit code 128+9=137"
        );
    }

    /// Test that process.run returns the correct exit code for normal exit.
    #[tokio::test]
    async fn test_process_run_normal_exit_code() {
        let params = Value::Map(vec![
            (
                Value::String("cmd".into()),
                Value::String("/bin/sh".into()),
            ),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("exit 42".into()),
                ]),
            ),
            (
                Value::String("cwd".into()),
                Value::String("/tmp".into()),
            ),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(exit_code, 42, "exit 42 should produce exit code 42");
    }

    /// Test exit_code_from_status with raw ExitStatus values.
    #[cfg(unix)]
    #[test]
    fn test_exit_code_from_status_signals() {
        use std::os::unix::process::ExitStatusExt;
        use std::process::ExitStatus;

        // Normal exit with code 0
        let status = ExitStatus::from_raw(0 << 8); // WEXITSTATUS=0, WIFEXITED=true
        assert_eq!(protocol::exit_code_from_status(status), 0);

        // Normal exit with code 42
        let status = ExitStatus::from_raw(42 << 8);
        assert_eq!(protocol::exit_code_from_status(status), 42);

        // Signal 2 (SIGINT): raw status = 2 (low byte = signal, no core dump)
        let status = ExitStatus::from_raw(2);
        assert_eq!(
            protocol::exit_code_from_status(status),
            130,
            "SIGINT raw status should give 128+2=130"
        );

        // Signal 9 (SIGKILL): raw status = 9
        let status = ExitStatus::from_raw(9);
        assert_eq!(
            protocol::exit_code_from_status(status),
            137,
            "SIGKILL raw status should give 128+9=137"
        );

        // Signal 15 (SIGTERM): raw status = 15
        let status = ExitStatus::from_raw(15);
        assert_eq!(
            protocol::exit_code_from_status(status),
            143,
            "SIGTERM raw status should give 128+15=143"
        );

        // Signal 2 with core dump: raw status = 2 | 0x80 = 130
        let status = ExitStatus::from_raw(2 | 0x80);
        assert_eq!(
            protocol::exit_code_from_status(status),
            130,
            "SIGINT with core dump should still give 128+2=130"
        );
    }
}
