//! File metadata operations

use crate::protocol::{from_value, FileAttributes, FileType, RpcError, StatResult};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::sync::Mutex;
use std::time::UNIX_EPOCH;
use tokio::fs;

use super::HandlerResult;

/// Get file attributes
pub async fn stat(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Path as string (UTF-8) or bytes (non-UTF8)
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// If true, don't follow symlinks
        #[serde(default)]
        lstat: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let attrs = get_file_attributes(&path, params.lstat).await?;
    Ok(attrs.to_value())
}

/// Batch stat operation - returns results for multiple paths concurrently
pub async fn stat_batch(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        paths: Vec<String>,
        #[serde(default)]
        lstat: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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
    // Convert to array of map values with named fields
    let values: Vec<Value> = results.iter().map(|r| r.to_value()).collect();
    Ok(Value::Array(values))
}

/// Check if file exists
pub async fn exists(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    // tokio::fs doesn't have exists(), so we try metadata
    let exists = fs::metadata(&path).await.is_ok();
    Ok(Value::Boolean(exists))
}

/// Check if file is readable
pub async fn readable(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
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

    Ok(Value::Boolean(readable))
}

/// Check if file is writable
pub async fn writable(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let writable = tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);
        unsafe { libc::access(path_cstr.as_ptr() as *const libc::c_char, libc::W_OK) == 0 }
    })
    .await
    .unwrap_or(false);

    Ok(Value::Boolean(writable))
}

/// Check if file is executable
pub async fn executable(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let executable = tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);
        unsafe { libc::access(path_cstr.as_ptr() as *const libc::c_char, libc::X_OK) == 0 }
    })
    .await
    .unwrap_or(false);

    Ok(Value::Boolean(executable))
}

/// Get the true name of a file (resolve symlinks)
pub async fn truename(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    // Use tokio's async canonicalize
    let canonical = fs::canonicalize(&path)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    // Return path as binary (MessagePack handles encoding)
    use std::os::unix::ffi::OsStrExt;
    let bytes = canonical.as_os_str().as_bytes();
    Ok(Value::Binary(bytes.to_vec()))
}

/// Check if file1 is newer than file2
pub async fn newer_than(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        file1: String,
        file2: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    Ok(Value::Boolean(result))
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
pub fn get_user_name(uid: u32) -> Option<String> {
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
pub fn get_group_name(gid: u32) -> Option<String> {
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

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

/// Convert raw bytes to a PathBuf
pub fn bytes_to_path(bytes: &[u8]) -> PathBuf {
    PathBuf::from(OsStr::from_bytes(bytes))
}

use crate::protocol::path_or_bytes;
