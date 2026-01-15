//! TRAMP-RPC Server
//!
//! A JSON-RPC 2.0 server for TRAMP remote file access.
//! Communicates over stdin/stdout using newline-delimited JSON.

mod handlers;
mod protocol;

use protocol::{Request, Response, RpcError};
use std::io::{self, BufRead, Write};

fn main() {
    // Disable buffering on stdout for immediate responses
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    // Process requests line by line
    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                continue;
            }
        };

        // Skip empty lines
        if line.trim().is_empty() {
            continue;
        }

        let response = process_request(&line);

        // Serialize and send response
        match serde_json::to_string(&response) {
            Ok(json) => {
                if writeln!(stdout, "{}", json).is_err() {
                    break; // Stdout closed, exit
                }
                let _ = stdout.flush();
            }
            Err(e) => {
                eprintln!("Error serializing response: {}", e);
            }
        }
    }
}

fn process_request(line: &str) -> Response {
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
    handlers::dispatch(&request)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_request() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"file.exists","params":{"path":"/tmp"}}"#;
        let response = process_request(json);
        assert!(response.error.is_none());
    }

    #[test]
    fn test_invalid_json() {
        let response = process_request("not json");
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::PARSE_ERROR);
    }

    #[test]
    fn test_method_not_found() {
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"nonexistent.method","params":{}}"#;
        let response = process_request(json);
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::METHOD_NOT_FOUND);
    }
}
