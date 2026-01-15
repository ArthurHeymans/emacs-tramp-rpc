//! Request handlers for TRAMP-RPC operations

pub mod dir;
pub mod file;
pub mod io;
pub mod process;

use crate::protocol::{Request, RequestId, Response, RpcError};

/// Dispatch a request to the appropriate handler
pub async fn dispatch(request: &Request) -> Response {
    // Handle batch separately (it needs special handling and can't recurse)
    if request.method == "batch" {
        let result = batch_execute(&request.params).await;
        return match result {
            Ok(value) => Response::success(request.id.clone(), value),
            Err(error) => Response::error(Some(request.id.clone()), error),
        };
    }

    // All other methods go through dispatch_inner
    dispatch_inner(request).await
}

type HandlerResult = Result<serde_json::Value, RpcError>;

/// Get system information
fn system_info() -> HandlerResult {
    use std::env;

    let info = serde_json::json!({
        "version": env!("CARGO_PKG_VERSION"),
        "os": std::env::consts::OS,
        "arch": std::env::consts::ARCH,
        "hostname": hostname(),
        "uid": unsafe { libc::getuid() },
        "gid": unsafe { libc::getgid() },
        "home": env::var("HOME").ok(),
        "user": env::var("USER").ok(),
    });

    Ok(info)
}

fn hostname() -> String {
    let mut buf = [0u8; 256];
    unsafe {
        if libc::gethostname(buf.as_mut_ptr() as *mut libc::c_char, buf.len()) == 0 {
            let len = buf.iter().position(|&c| c == 0).unwrap_or(buf.len());
            String::from_utf8_lossy(&buf[..len]).to_string()
        } else {
            "unknown".to_string()
        }
    }
}

/// Get environment variable
fn system_getenv(params: &serde_json::Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        name: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    Ok(serde_json::json!(std::env::var(&params.name).ok()))
}

/// Expand path with tilde and environment variables
fn system_expand_path(params: &serde_json::Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let expanded = expand_tilde(&params.path);
    Ok(serde_json::json!(expanded))
}

/// Expand ~ to home directory
fn expand_tilde(path: &str) -> String {
    if path.starts_with("~/") {
        if let Ok(home) = std::env::var("HOME") {
            return format!("{}{}", home, &path[1..]);
        }
    } else if path == "~" {
        if let Ok(home) = std::env::var("HOME") {
            return home;
        }
    }
    path.to_string()
}

/// Execute multiple RPC requests in a single batch
/// This saves round-trip time when multiple independent operations are needed.
///
/// Request format:
/// ```json
/// {
///   "requests": [
///     {"method": "file.exists", "params": {"path": "/foo"}},
///     {"method": "file.stat", "params": {"path": "/bar"}},
///     {"method": "process.run", "params": {"cmd": "git", "args": ["status"]}}
///   ]
/// }
/// ```
///
/// Response format:
/// ```json
/// {
///   "results": [
///     {"result": true},
///     {"result": {...file attrs...}},
///     {"error": {"code": -32001, "message": "..."}}
///   ]
/// }
/// ```
async fn batch_execute(params: &serde_json::Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct BatchParams {
        requests: Vec<BatchRequest>,
    }

    #[derive(serde::Deserialize)]
    struct BatchRequest {
        method: String,
        #[serde(default)]
        params: serde_json::Value,
    }

    let batch_params: BatchParams = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Execute all requests concurrently using tokio::join_all
    let futures: Vec<_> = batch_params
        .requests
        .into_iter()
        .map(|req| async move {
            // Create a fake Request to reuse dispatch logic
            let fake_request = Request {
                jsonrpc: "2.0".to_string(),
                id: RequestId::Number(0), // Dummy ID, not used in batch
                method: req.method,
                params: req.params,
            };

            // Get the result by calling the handler directly (not full dispatch)
            let response = dispatch_inner(&fake_request).await;

            // Convert Response to a result object
            match (response.result, response.error) {
                (Some(result), None) => serde_json::json!({"result": result}),
                (None, Some(error)) => serde_json::json!({
                    "error": {
                        "code": error.code,
                        "message": error.message
                    }
                }),
                _ => serde_json::json!({"result": null}),
            }
        })
        .collect();

    // Run all batch requests concurrently
    let results = futures::future::join_all(futures).await;

    Ok(serde_json::json!({ "results": results }))
}

/// Inner dispatch that handles the actual method routing
/// Used by both single requests and batch requests
async fn dispatch_inner(request: &Request) -> Response {
    let result = match request.method.as_str() {
        // File metadata operations
        "file.stat" => file::stat(&request.params).await,
        "file.stat_batch" => file::stat_batch(&request.params).await,
        "file.exists" => file::exists(&request.params).await,
        "file.readable" => file::readable(&request.params).await,
        "file.writable" => file::writable(&request.params).await,
        "file.executable" => file::executable(&request.params).await,
        "file.truename" => file::truename(&request.params).await,
        "file.newer_than" => file::newer_than(&request.params).await,

        // Directory operations
        "dir.list" => dir::list(&request.params).await,
        "dir.create" => dir::create(&request.params).await,
        "dir.remove" => dir::remove(&request.params).await,
        "dir.completions" => dir::completions(&request.params).await,

        // File I/O operations
        "file.read" => io::read(&request.params).await,
        "file.write" => io::write(&request.params).await,
        "file.copy" => io::copy(&request.params).await,
        "file.rename" => io::rename(&request.params).await,
        "file.delete" => io::delete(&request.params).await,
        "file.set_modes" => io::set_modes(&request.params).await,
        "file.set_times" => io::set_times(&request.params).await,
        "file.make_symlink" => io::make_symlink(&request.params).await,

        // Process operations
        "process.run" => process::run(&request.params).await,
        "process.start" => process::start(&request.params).await,
        "process.write" => process::write(&request.params).await,
        "process.read" => process::read(&request.params).await,
        "process.close_stdin" => process::close_stdin(&request.params).await,
        "process.kill" => process::kill(&request.params).await,
        "process.list" => process::list(&request.params).await,

        // System info
        "system.info" => system_info(),
        "system.getenv" => system_getenv(&request.params),
        "system.expand_path" => system_expand_path(&request.params),

        // Note: "batch" is NOT allowed in batch (no recursion)
        _ => Err(RpcError::method_not_found(&request.method)),
    };

    match result {
        Ok(value) => Response::success(request.id.clone(), value),
        Err(error) => Response::error(Some(request.id.clone()), error),
    }
}
