//! File I/O operations

use crate::protocol::RpcError;
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use serde::Deserialize;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

use super::file::map_io_error;

type HandlerResult = Result<serde_json::Value, RpcError>;

/// Read file contents
pub fn read(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// Byte offset to start reading from
        #[serde(default)]
        offset: Option<u64>,
        /// Maximum number of bytes to read (default: entire file)
        #[serde(default)]
        length: Option<usize>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut file = File::open(&params.path).map_err(|e| map_io_error(e, &params.path))?;

    // Seek to offset if specified
    if let Some(offset) = params.offset {
        file.seek(SeekFrom::Start(offset))
            .map_err(|e| map_io_error(e, &params.path))?;
    }

    // Read the content
    let content = if let Some(length) = params.length {
        let mut buf = vec![0u8; length];
        let bytes_read = file
            .read(&mut buf)
            .map_err(|e| map_io_error(e, &params.path))?;
        buf.truncate(bytes_read);
        buf
    } else {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)
            .map_err(|e| map_io_error(e, &params.path))?;
        buf
    };

    // Return base64-encoded content
    let encoded = BASE64.encode(&content);

    Ok(serde_json::json!({
        "content": encoded,
        "size": content.len()
    }))
}

/// Write file contents
pub fn write(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// Base64-encoded content to write
        content: String,
        /// File mode (permissions) - only applied to new files
        #[serde(default)]
        mode: Option<u32>,
        /// Append to file instead of overwriting
        #[serde(default)]
        append: bool,
        /// Byte offset to start writing at (only if not appending)
        #[serde(default)]
        offset: Option<u64>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Decode the content
    let content = BASE64
        .decode(&params.content)
        .map_err(|e| RpcError::invalid_params(format!("Invalid base64: {}", e)))?;

    // Open the file with appropriate options
    let mut options = OpenOptions::new();

    if params.append {
        options.append(true).create(true);
    } else if params.offset.is_some() {
        options.write(true);
    } else {
        options.write(true).create(true).truncate(true);
    }

    let mut file = options
        .open(&params.path)
        .map_err(|e| map_io_error(e, &params.path))?;

    // Seek to offset if specified
    if let Some(offset) = params.offset {
        file.seek(SeekFrom::Start(offset))
            .map_err(|e| map_io_error(e, &params.path))?;
    }

    // Write the content
    file.write_all(&content)
        .map_err(|e| map_io_error(e, &params.path))?;

    // Set permissions if specified
    if let Some(mode) = params.mode {
        let perms = fs::Permissions::from_mode(mode);
        fs::set_permissions(&params.path, perms).map_err(|e| map_io_error(e, &params.path))?;
    }

    Ok(serde_json::json!({
        "written": content.len()
    }))
}

/// Copy a file
pub fn copy(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        src: String,
        dest: String,
        /// Preserve file attributes
        #[serde(default)]
        preserve: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Copy the file
    let bytes_copied =
        fs::copy(&params.src, &params.dest).map_err(|e| map_io_error(e, &params.src))?;

    // Preserve attributes if requested
    if params.preserve {
        if let Ok(metadata) = fs::metadata(&params.src) {
            // Preserve permissions
            let _ = fs::set_permissions(&params.dest, metadata.permissions());

            // Preserve timestamps
            #[cfg(unix)]
            {
                use std::os::unix::fs::MetadataExt;
                let _ = set_file_times(&params.dest, metadata.atime(), metadata.mtime());
            }
        }
    }

    Ok(serde_json::json!({
        "copied": bytes_copied
    }))
}

/// Rename/move a file
pub fn rename(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        src: String,
        dest: String,
        /// Overwrite destination if it exists
        #[serde(default)]
        overwrite: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Check if destination exists and overwrite is false
    if !params.overwrite && Path::new(&params.dest).exists() {
        return Err(RpcError {
            code: RpcError::IO_ERROR,
            message: format!("Destination already exists: {}", params.dest),
            data: None,
        });
    }

    fs::rename(&params.src, &params.dest).map_err(|e| map_io_error(e, &params.src))?;

    Ok(serde_json::json!(true))
}

/// Delete a file
pub fn delete(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// If true, don't error if file doesn't exist
        #[serde(default)]
        force: bool,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    match fs::remove_file(&params.path) {
        Ok(()) => Ok(serde_json::json!(true)),
        Err(e) if params.force && e.kind() == std::io::ErrorKind::NotFound => {
            Ok(serde_json::json!(false))
        }
        Err(e) => Err(map_io_error(e, &params.path)),
    }
}

/// Set file permissions
pub fn set_modes(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        mode: u32,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let perms = fs::Permissions::from_mode(params.mode);
    fs::set_permissions(&params.path, perms).map_err(|e| map_io_error(e, &params.path))?;

    Ok(serde_json::json!(true))
}

/// Set file timestamps
pub fn set_times(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: String,
        /// Modification time (seconds since epoch)
        mtime: i64,
        /// Access time (seconds since epoch, defaults to mtime)
        #[serde(default)]
        atime: Option<i64>,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let atime = params.atime.unwrap_or(params.mtime);

    set_file_times(&params.path, atime, params.mtime)?;

    Ok(serde_json::json!(true))
}

/// Create a symbolic link
pub fn make_symlink(params: &serde_json::Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        target: String,
        link_path: String,
    }

    let params: Params = serde_json::from_value(params.clone())
        .map_err(|e| RpcError::invalid_params(e.to_string()))?;

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(&params.target, &params.link_path)
            .map_err(|e| map_io_error(e, &params.link_path))?;
    }

    Ok(serde_json::json!(true))
}

// ============================================================================
// Helper functions
// ============================================================================

#[cfg(unix)]
fn set_file_times(path: &str, atime: i64, mtime: i64) -> Result<(), RpcError> {
    use std::ffi::CString;

    let path_cstr = CString::new(path).map_err(|_| RpcError::invalid_params("Invalid path"))?;

    let times = [
        libc::timespec {
            tv_sec: atime,
            tv_nsec: 0,
        },
        libc::timespec {
            tv_sec: mtime,
            tv_nsec: 0,
        },
    ];

    let result = unsafe { libc::utimensat(libc::AT_FDCWD, path_cstr.as_ptr(), times.as_ptr(), 0) };

    if result != 0 {
        return Err(RpcError::io_error(std::io::Error::last_os_error()));
    }

    Ok(())
}
