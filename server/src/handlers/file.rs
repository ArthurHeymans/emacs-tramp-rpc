//! File metadata operations

use crate::protocol::{FileAttributes, FileType, RpcError, StatResult};
use serde::Deserialize;
use std::fs;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::time::UNIX_EPOCH;

type HandlerResult = Result<serde_json::Value, RpcError>;

/// Get file attributes
pub fn stat(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// If true, don't follow symlinks
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let attrs = get_file_attributes(&params.path, params.lstat)?;
    Ok(serde_json::to_value(attrs).unwrap())
}

/// Batch stat operation - returns results for multiple paths
pub fn stat_batch(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        paths: Vec<String>,
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let results: Vec<StatResult> = params
        .paths
        .iter()
        .map(|path| match get_file_attributes(path, params.lstat) {
            Ok(attrs) => StatResult::Ok(attrs),
            Err(e) => StatResult::Err {
                error: e.message.clone(),
            },
        })
        .collect();

    Ok(serde_json::to_value(results).unwrap())
}

/// Check if file exists
pub fn exists(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let exists = Path::new(&params.path).exists();
    Ok(serde_json::json!(exists))
}

/// Check if file is readable
pub fn readable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Use access() syscall to check read permission
    let readable = unsafe {
        libc::access(
            std::ffi::CString::new(params.path.as_str())
                .unwrap()
                .as_ptr(),
            libc::R_OK,
        ) == 0
    };

    Ok(serde_json::json!(readable))
}

/// Check if file is writable
pub fn writable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let writable = unsafe {
        libc::access(
            std::ffi::CString::new(params.path.as_str())
                .unwrap()
                .as_ptr(),
            libc::W_OK,
        ) == 0
    };

    Ok(serde_json::json!(writable))
}

/// Check if file is executable
pub fn executable(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let executable = unsafe {
        libc::access(
            std::ffi::CString::new(params.path.as_str())
                .unwrap()
                .as_ptr(),
            libc::X_OK,
        ) == 0
    };

    Ok(serde_json::json!(executable))
}

/// Get the true name of a file (resolve symlinks)
pub fn truename(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = Path::new(&params.path);

    // First canonicalize to resolve symlinks and get absolute path
    let canonical = fs::canonicalize(path).map_err(|e| map_io_error(e, &params.path))?;

    Ok(serde_json::json!(canonical.to_string_lossy()))
}

/// Check if file1 is newer than file2
pub fn newer_than(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        file1: String,
        file2: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let meta1 = fs::metadata(&params.file1);
    let meta2 = fs::metadata(&params.file2);

    let result = match (meta1, meta2) {
        (Ok(m1), Ok(m2)) => {
            let t1 = m1.modified().unwrap_or(UNIX_EPOCH);
            let t2 = m2.modified().unwrap_or(UNIX_EPOCH);
            t1 > t2
        }
        (Ok(_), Err(_)) => true,  // file1 exists, file2 doesn't
        (Err(_), _) => false,     // file1 doesn't exist
    };

    Ok(serde_json::json!(result))
}

// ============================================================================
// Helper functions
// ============================================================================

pub fn get_file_attributes(path: &str, lstat: bool) -> Result<FileAttributes, RpcError> {
    let path_obj = Path::new(path);

    let metadata = if lstat {
        fs::symlink_metadata(path_obj)
    } else {
        fs::metadata(path_obj)
    }
    .map_err(|e| map_io_error(e, path))?;

    let file_type = get_file_type(&metadata);

    let link_target = if file_type == FileType::Symlink {
        fs::read_link(path_obj).ok().map(|p| p.to_string_lossy().to_string())
    } else {
        None
    };

    Ok(FileAttributes {
        file_type,
        nlinks: metadata.nlink(),
        uid: metadata.uid(),
        gid: metadata.gid(),
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

fn get_file_type(metadata: &fs::Metadata) -> FileType {
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

pub fn map_io_error(err: std::io::Error, path: &str) -> RpcError {
    use std::io::ErrorKind;

    match err.kind() {
        ErrorKind::NotFound => RpcError::file_not_found(path),
        ErrorKind::PermissionDenied => RpcError::permission_denied(path),
        _ => RpcError::io_error(err),
    }
}
