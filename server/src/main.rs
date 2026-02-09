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
            // 100MB max message size - silently skip
            // (cannot use eprintln! as SSH merges stderr with stdout)
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
    handlers::dispatch(&request).await
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
        let payload = make_request("file.exists", params);
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
}
