//! Request handlers for TRAMP-RPC operations

pub mod commands;
pub mod dir;
pub mod file;
pub mod io;
pub mod process;

use crate::msgpack_map;
use crate::protocol::{from_value, Request, RequestId, Response, RpcError};
use rmpv::Value;

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

pub type HandlerResult = Result<Value, RpcError>;

/// Get system information
fn system_info() -> HandlerResult {
    use std::env;

    Ok(msgpack_map! {
        "version" => env!("CARGO_PKG_VERSION"),
        "os" => std::env::consts::OS,
        "arch" => std::env::consts::ARCH,
        "hostname" => hostname(),
        "uid" => unsafe { libc::getuid() },
        "gid" => unsafe { libc::getgid() },
        "home" => env::var("HOME").ok().into_value(),
        "user" => env::var("USER").ok().into_value()
    })
}

use crate::protocol::IntoValue;

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
fn system_getenv(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        name: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    Ok(std::env::var(&params.name).ok().into_value())
}

/// Expand path with tilde and environment variables
fn system_expand_path(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let expanded = expand_tilde(&params.path);
    Ok(expanded.into_value())
}

/// Get filesystem information (like df)
fn system_statvfs(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    use std::ffi::CString;
    let expanded = expand_tilde(&params.path);
    let path_cstr =
        CString::new(expanded.as_str()).map_err(|_| RpcError::invalid_params("Invalid path"))?;

    let mut stat: libc::statvfs = unsafe { std::mem::zeroed() };
    let result = unsafe { libc::statvfs(path_cstr.as_ptr(), &mut stat) };

    if result != 0 {
        return Err(RpcError::io_error(std::io::Error::last_os_error()));
    }

    // Return values in bytes (multiply by block size)
    // Allow unnecessary casts for cross-platform compatibility (types differ between Linux/macOS)
    #[allow(clippy::unnecessary_cast)]
    let block_size = stat.f_frsize as u64;
    #[allow(clippy::unnecessary_cast)]
    let total = stat.f_blocks as u64 * block_size;
    #[allow(clippy::unnecessary_cast)]
    let free = stat.f_bfree as u64 * block_size;
    #[allow(clippy::unnecessary_cast)]
    let available = stat.f_bavail as u64 * block_size;

    Ok(msgpack_map! {
        "total" => total,
        "free" => free,
        "available" => available,
        "block_size" => block_size
    })
}

/// Get groups for the current user
fn system_groups() -> HandlerResult {
    // Get supplementary groups (use reasonable max buffer)
    let ngroups: libc::c_int = 64;
    let mut groups: Vec<libc::gid_t> = vec![0; ngroups as usize];

    let actual_count = unsafe { libc::getgroups(ngroups, groups.as_mut_ptr()) };

    if actual_count < 0 {
        return Err(RpcError::io_error(std::io::Error::last_os_error()));
    }

    groups.truncate(actual_count as usize);

    // Convert to group info with names
    let group_info: Vec<Value> = groups
        .iter()
        .map(|&gid| {
            let gname = get_group_name(gid);
            msgpack_map! {
                "gid" => gid,
                "name" => gname.into_value()
            }
        })
        .collect();

    Ok(Value::Array(group_info))
}

/// Get group name from gid (delegates to file.rs's mutex-protected, cached version)
fn get_group_name(gid: libc::gid_t) -> Option<String> {
    file::get_group_name(gid)
}

/// Expand ~ to home directory
pub(crate) fn expand_tilde(path: &str) -> String {
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
async fn batch_execute(params: &Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct BatchParams {
        requests: Vec<BatchRequest>,
    }

    fn default_params() -> Value {
        Value::Nil
    }

    #[derive(serde::Deserialize)]
    struct BatchRequest {
        method: String,
        #[serde(default = "default_params")]
        params: Value,
    }

    let batch_params: BatchParams =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Execute all requests concurrently using tokio::join_all
    let futures: Vec<_> = batch_params
        .requests
        .into_iter()
        .map(|req| async move {
            // Create a fake Request to reuse dispatch logic
            let fake_request = Request {
                version: "2.0".to_string(),
                id: RequestId::Number(0), // Dummy ID, not used in batch
                method: req.method,
                params: req.params,
            };

            // Get the result by calling the handler directly (not full dispatch)
            let response = dispatch_inner(&fake_request).await;

            // Convert Response to a result object
            match (response.result, response.error) {
                (Some(result), None) => msgpack_map! { "result" => result },
                (None, Some(error)) => msgpack_map! {
                    "error" => msgpack_map! {
                        "code" => error.code,
                        "message" => error.message
                    }
                },
                _ => msgpack_map! { "result" => Value::Nil },
            }
        })
        .collect();

    // Run all batch requests concurrently
    let results = futures::future::join_all(futures).await;

    Ok(msgpack_map! { "results" => Value::Array(results) })
}

/// Inner dispatch that handles the actual method routing
/// Used by both single requests and batch requests
async fn dispatch_inner(request: &Request) -> Response {
    let result = match request.method.as_str() {
        // File metadata operations
        "file.stat" => file::stat(&request.params).await,
        "file.stat_batch" => file::stat_batch(&request.params).await,
        "file.executable" => file::executable(&request.params).await,
        "file.truename" => file::truename(&request.params).await,

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
        "file.make_hardlink" => io::make_hardlink(&request.params).await,
        "file.chown" => io::chown(&request.params).await,

        // Process operations
        "process.run" => process::run(&request.params).await,
        "process.start" => process::start(&request.params).await,
        "process.write" => process::write(&request.params).await,
        "process.read" => process::read(&request.params).await,
        "process.close_stdin" => process::close_stdin(&request.params).await,
        "process.kill" => process::kill(&request.params).await,
        "process.list" => process::list(&request.params).await,

        // PTY (pseudo-terminal) process operations
        "process.start_pty" => process::start_pty(&request.params).await,
        "process.read_pty" => process::read_pty(&request.params).await,
        "process.write_pty" => process::write_pty(&request.params).await,
        "process.resize_pty" => process::resize_pty(&request.params).await,
        "process.kill_pty" => process::kill_pty(&request.params).await,
        "process.close_pty" => process::close_pty(&request.params).await,
        "process.list_pty" => process::list_pty(&request.params).await,

        // System info
        "system.info" => system_info(),
        "system.getenv" => system_getenv(&request.params),
        "system.expand_path" => system_expand_path(&request.params),
        "system.statvfs" => system_statvfs(&request.params),
        "system.groups" => system_groups(),

        // Parallel command execution and ancestor scanning
        "commands.run_parallel" => commands::run_parallel(&request.params).await,
        "ancestors.scan" => commands::ancestors_scan(&request.params).await,

        // Filesystem watch operations (for cache invalidation)
        "watch.add" => crate::watcher::handle_add(&request.params),
        "watch.remove" => crate::watcher::handle_remove(&request.params),
        "watch.list" => crate::watcher::handle_list(&request.params),

        // Note: "batch" is NOT allowed in batch (no recursion)
        _ => Err(RpcError::method_not_found(&request.method)),
    };

    match result {
        Ok(value) => Response::success(request.id.clone(), value),
        Err(error) => Response::error(Some(request.id.clone()), error),
    }
}
