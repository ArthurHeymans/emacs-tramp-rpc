//! TRAMP-RPC Server
//!
//! A JSON-RPC 2.0 server for TRAMP remote file access.
//! Communicates over stdin/stdout using newline-delimited JSON.
//!
//! Uses tokio for async concurrent request processing - multiple requests
//! can be processed in parallel while waiting on I/O.

mod handlers;
mod protocol;

use protocol::{Request, Response, RpcError};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader, BufWriter};
use tokio::sync::Mutex;
use tokio::task::JoinSet;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let reader = BufReader::new(stdin);
    let writer = Arc::new(Mutex::new(BufWriter::new(stdout)));

    let mut lines = reader.lines();
    let mut tasks: JoinSet<()> = JoinSet::new();

    // Process requests concurrently
    while let Ok(Some(line)) = lines.next_line().await {
        // Skip empty lines
        if line.trim().is_empty() {
            continue;
        }

        // Clone writer for this task
        let writer = Arc::clone(&writer);

        // Spawn a task for each request - allows concurrent processing
        tasks.spawn(async move {
            let response = process_request(&line).await;

            // Serialize and send response
            if let Ok(json) = serde_json::to_string(&response) {
                let mut writer = writer.lock().await;
                let _ = writer.write_all(json.as_bytes()).await;
                let _ = writer.write_all(b"\n").await;
                let _ = writer.flush().await;
            }
        });
    }

    // Wait for all pending tasks to complete before exiting
    while tasks.join_next().await.is_some() {}
}

async fn process_request(line: &str) -> Response {
    // Parse the request
    let request: Request = match serde_json::from_str(line) {
        Ok(r) => r,
        Err(e) => {
            return Response::error(None, RpcError::parse_error(e.to_string()));
        }
    };

    // Validate JSON-RPC version
    if request.jsonrpc != "2.0" {
        return Response::error(
            Some(request.id),
            RpcError::invalid_request("Invalid JSON-RPC version"),
        );
    }

    // Dispatch to handler
    handlers::dispatch(&request).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_parse_request() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"file.exists","params":{"path":"/tmp"}}"#;
        let response = process_request(json).await;
        assert!(response.error.is_none());
    }

    #[tokio::test]
    async fn test_invalid_json() {
        let response = process_request("not json").await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::PARSE_ERROR);
    }

    #[tokio::test]
    async fn test_method_not_found() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"nonexistent.method","params":{}}"#;
        let response = process_request(json).await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::METHOD_NOT_FOUND);
    }
}
