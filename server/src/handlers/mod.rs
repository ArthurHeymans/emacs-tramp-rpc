//! Request handlers for TRAMP-RPC operations

pub mod file;
pub mod dir;
pub mod io;
pub mod process;

use crate::protocol::{Request, Response, RpcError};

/// Dispatch a request to the appropriate handler
pub fn dispatch(request: &Request) -> Response {
    let result = match request.method.as_str() {
        // File metadata operations
        "file.stat" => file::stat(&request.params),
        "file.stat_batch" => file::stat_batch(&request.params),
        "file.exists" => file::exists(&request.params),
        "file.readable" => file::readable(&request.params),
        "file.writable" => file::writable(&request.params),
        "file.executable" => file::executable(&request.params),
        "file.truename" => file::truename(&request.params),
        "file.newer_than" => file::newer_than(&request.params),

        // Directory operations
        "dir.list" => dir::list(&request.params),
        "dir.create" => dir::create(&request.params),
        "dir.remove" => dir::remove(&request.params),
        "dir.completions" => dir::completions(&request.params),

        // File I/O operations
        "file.read" => io::read(&request.params),
        "file.write" => io::write(&request.params),
        "file.copy" => io::copy(&request.params),
        "file.rename" => io::rename(&request.params),
        "file.delete" => io::delete(&request.params),
        "file.set_modes" => io::set_modes(&request.params),
        "file.set_times" => io::set_times(&request.params),
        "file.make_symlink" => io::make_symlink(&request.params),

        // Process operations
        "process.run" => process::run(&request.params),
        "process.start" => process::start(&request.params),
        "process.write" => process::write(&request.params),
        "process.read" => process::read(&request.params),
        "process.kill" => process::kill(&request.params),
        "process.list" => process::list(&request.params),

        // System info
        "system.info" => system_info(),
        "system.getenv" => system_getenv(&request.params),
        "system.expand_path" => system_expand_path(&request.params),

        _ => Err(RpcError::method_not_found(&request.method)),
    };

    match result {
        Ok(value) => Response::success(request.id.clone(), value),
        Err(error) => Response::error(Some(request.id.clone()), error),
    }
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
