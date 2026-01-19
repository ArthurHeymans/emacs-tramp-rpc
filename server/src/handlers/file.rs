//! File metadata operations

use crate::protocol::{FileAttributes, FileType, RpcError, StatResult};
use serde::Deserialize;
use std::collections::HashMap;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::sync::Mutex;
use std::time::UNIX_EPOCH;
use tokio::fs;

type HandlerResult = Result<serde_json::Value, RpcError>;

/// Get file attributes
pub async fn stat(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// Encoding for path: "text" (default) or "base64" for non-UTF8 paths
        path_encoding: Option<String>,
        /// If true, don't follow symlinks
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;
    let attrs = get_file_attributes(&path, params.lstat).await?;
    Ok(serde_json::to_value(attrs).unwrap())
}

/// Batch stat operation - returns results for multiple paths concurrently
pub async fn stat_batch(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        paths: Vec<String>,
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Run all stat operations concurrently
    let futures: Vec<_> = params
        .paths
        .iter()
        .map(|path| {
            let path = path.clone();
            let lstat = params.lstat;
            async move {
                match get_file_attributes(Path::new(&path), lstat).await {
                    Ok(attrs) => StatResult::Ok(attrs),
                    Err(e) => StatResult::Err {
                        error: e.message.clone(),
                    },
                }
            }
        })
        .collect();

    let results = futures::future::join_all(futures).await;
    Ok(serde_json::to_value(results).unwrap())
}

/// Check if file exists
pub async fn exists(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        path_encoding: Option<String>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;
    // tokio::fs doesn't have exists(), so we try metadata
    let exists = fs::metadata(&path).await.is_ok();
    Ok(serde_json::json!(exists))
}

/// Check if file is readable
pub async fn readable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        path_encoding: Option<String>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;
    // Use access() syscall to check read permission
    let readable = tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        // Create null-terminated path
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);
        unsafe { libc::access(path_cstr.as_ptr() as *const libc::c_char, libc::R_OK) == 0 }
    })
    .await
    .unwrap_or(false);

    Ok(serde_json::json!(readable))
}

/// Check if file is writable
pub async fn writable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        path_encoding: Option<String>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;
    let writable = tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);
        unsafe { libc::access(path_cstr.as_ptr() as *const libc::c_char, libc::W_OK) == 0 }
    })
    .await
    .unwrap_or(false);

    Ok(serde_json::json!(writable))
}

/// Check if file is executable
pub async fn executable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        path_encoding: Option<String>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;
    let executable = tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);
        unsafe { libc::access(path_cstr.as_ptr() as *const libc::c_char, libc::X_OK) == 0 }
    })
    .await
    .unwrap_or(false);

    Ok(serde_json::json!(executable))
}

/// Get the true name of a file (resolve symlinks)
pub async fn truename(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        path_encoding: Option<String>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = decode_path(&params.path, params.path_encoding.as_deref())?;

    // Use tokio's async canonicalize
    let canonical = fs::canonicalize(&path)
        .await
        .map_err(|e| map_io_error(e, &params.path))?;

    // Check if the path is valid UTF-8
    if let Some(s) = canonical.to_str() {
        // Valid UTF-8 path
        Ok(serde_json::json!({
            "path": s,
            "path_encoding": "text"
        }))
    } else {
        // Non-UTF8 path - encode as base64
        use std::os::unix::ffi::OsStrExt;
        let bytes = canonical.as_os_str().as_bytes();
        Ok(serde_json::json!({
            "path": BASE64.encode(bytes),
            "path_encoding": "base64"
        }))
    }
}

/// Check if file1 is newer than file2
pub async fn newer_than(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        file1: String,
        file2: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Get both metadata concurrently
    let (meta1, meta2) = tokio::join!(fs::metadata(&params.file1), fs::metadata(&params.file2));

    let result = match (meta1, meta2) {
        (Ok(m1), Ok(m2)) => {
            let t1 = m1.modified().unwrap_or(UNIX_EPOCH);
            let t2 = m2.modified().unwrap_or(UNIX_EPOCH);
            t1 > t2
        }
        (Ok(_), Err(_)) => true, // file1 exists, file2 doesn't
        (Err(_), _) => false,    // file1 doesn't exist
    };

    Ok(serde_json::json!(result))
}

// ============================================================================
// Helper functions
// ============================================================================

pub async fn get_file_attributes(path: &Path, lstat: bool) -> Result<FileAttributes, RpcError> {
    let metadata = if lstat {
        fs::symlink_metadata(path).await
    } else {
        fs::metadata(path).await
    }
    .map_err(|e| map_io_error(e, &path.to_string_lossy()))?;

    let file_type = get_file_type(&metadata);

    let link_target = if file_type == FileType::Symlink {
        fs::read_link(path)
            .await
            .ok()
            .map(|p| p.to_string_lossy().to_string())
    } else {
        None
    };

    let uid = metadata.uid();
    let gid = metadata.gid();

    Ok(FileAttributes {
        file_type,
        nlinks: metadata.nlink(),
        uid,
        gid,
        uname: get_user_name(uid),
        gname: get_group_name(gid),
        atime: metadata.atime(),
        mtime: metadata.mtime(),
        ctime: metadata.ctime(),
        size: metadata.len(),
        mode: metadata.mode(),
        inode: metadata.ino(),
        dev: metadata.dev(),
        link_target,
    })
}

fn get_file_type(metadata: &std::fs::Metadata) -> FileType {
    let ft = metadata.file_type();

    if ft.is_file() {
        FileType::File
    } else if ft.is_dir() {
        FileType::Directory
    } else if ft.is_symlink() {
        FileType::Symlink
    } else if ft.is_char_device() {
        FileType::CharDevice
    } else if ft.is_block_device() {
        FileType::BlockDevice
    } else if ft.is_fifo() {
        FileType::Fifo
    } else if ft.is_socket() {
        FileType::Socket
    } else {
        FileType::Unknown
    }
}

static USER_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, String>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Get user name from uid
fn get_user_name(uid: u32) -> Option<String> {
    let mut cache = USER_NAMES.lock().unwrap();

    if let Some(result) = cache.get(&uid) {
        Some(result.clone())
    } else {
        unsafe {
            let passwd = libc::getpwuid(uid);
            if passwd.is_null() {
                return None;
            }
            let name = std::ffi::CStr::from_ptr((*passwd).pw_name);

            name.to_str().ok().map(|s| {
                let result = s.to_string();
                cache.insert(uid, result.clone());
                result
            })
        }
    }
}

static GROUP_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, String>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Get group name from gid
fn get_group_name(gid: u32) -> Option<String> {
    let mut cache = GROUP_NAMES.lock().unwrap();

    if let Some(result) = cache.get(&gid) {
        Some(result.clone())
    } else {
        unsafe {
            let group = libc::getgrgid(gid);
            if group.is_null() {
                return None;
            }
            let name = std::ffi::CStr::from_ptr((*group).gr_name);

            name.to_str().ok().map(|s| {
                let result = s.to_string();
                cache.insert(gid, result.clone());
                result
            })
        }
    }
}

pub fn map_io_error(err: std::io::Error, path: &str) -> RpcError {
    use std::io::ErrorKind;

    match err.kind() {
        ErrorKind::NotFound => RpcError::file_not_found(path),
        ErrorKind::PermissionDenied => RpcError::permission_denied(path),
        _ => RpcError::io_error(err),
    }
}

use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

/// Decode a path that may be base64-encoded (for non-UTF8 filenames).
/// If path_encoding is "base64", decode the path from base64.
/// Otherwise, use the path as-is (UTF-8 string).
pub fn decode_path(path: &str, path_encoding: Option<&str>) -> Result<PathBuf, RpcError> {
    match path_encoding {
        Some("base64") => {
            let bytes = BASE64
                .decode(path)
                .map_err(|e| RpcError::invalid_params(format!("Invalid base64 path: {}", e)))?;
            Ok(PathBuf::from(OsStr::from_bytes(&bytes)))
        }
        _ => Ok(PathBuf::from(path)),
    }
}
