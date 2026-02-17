//! File metadata operations

use crate::protocol::{from_value, FileAttributes, FileType, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::sync::Mutex;
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
    match get_file_attributes(&path, params.lstat).await {
        Ok(attrs) => Ok(attrs.to_value()),
        Err(e) if e.code == RpcError::FILE_NOT_FOUND => Ok(Value::Nil),
        Err(e) => Err(e),
    }
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

/// Get user name from uid using thread-safe getpwuid_r
pub fn get_user_name(uid: u32) -> Option<String> {
    let mut cache = USER_NAMES.lock().unwrap_or_else(|e| e.into_inner());

    if let Some(result) = cache.get(&uid) {
        Some(result.clone())
    } else {
        // Use getpwuid_r (reentrant) instead of getpwuid for thread safety
        let mut buf = vec![0u8; 1024];
        let mut pwd: libc::passwd = unsafe { std::mem::zeroed() };
        let mut result_ptr: *mut libc::passwd = std::ptr::null_mut();

        let ret = unsafe {
            libc::getpwuid_r(
                uid,
                &mut pwd,
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut result_ptr,
            )
        };

        if ret != 0 || result_ptr.is_null() {
            return None;
        }

        let name = unsafe { std::ffi::CStr::from_ptr(pwd.pw_name) };
        name.to_str().ok().map(|s| {
            let result = s.to_string();
            cache.insert(uid, result.clone());
            result
        })
    }
}

static GROUP_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, String>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Get group name from gid using thread-safe getgrgid_r
pub fn get_group_name(gid: u32) -> Option<String> {
    let mut cache = GROUP_NAMES.lock().unwrap_or_else(|e| e.into_inner());

    if let Some(result) = cache.get(&gid) {
        Some(result.clone())
    } else {
        // Use getgrgid_r (reentrant) instead of getgrgid for thread safety
        let mut buf = vec![0u8; 1024];
        let mut grp: libc::group = unsafe { std::mem::zeroed() };
        let mut result_ptr: *mut libc::group = std::ptr::null_mut();

        let ret = unsafe {
            libc::getgrgid_r(
                gid,
                &mut grp,
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut result_ptr,
            )
        };

        if ret != 0 || result_ptr.is_null() {
            return None;
        }

        let name = unsafe { std::ffi::CStr::from_ptr(grp.gr_name) };
        name.to_str().ok().map(|s| {
            let result = s.to_string();
            cache.insert(gid, result.clone());
            result
        })
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
    let path = PathBuf::from(OsStr::from_bytes(bytes));
    expand_tilde_path(&path)
}

/// Expand ~ to home directory in a PathBuf.
/// Delegates to the canonical string-based `expand_tilde` in mod.rs.
fn expand_tilde_path(path: &Path) -> PathBuf {
    let s = path.to_string_lossy();
    let expanded = super::expand_tilde(&s);
    if expanded.as_str() != s.as_ref() {
        PathBuf::from(expanded)
    } else {
        path.to_path_buf()
    }
}

use crate::protocol::path_or_bytes;
