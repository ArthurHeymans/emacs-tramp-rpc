//! File I/O operations

use crate::msgpack_map;
use crate::protocol::{from_value, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::io::SeekFrom;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use tokio::fs::{self, File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};

use super::file::{bytes_to_path, map_io_error};
use super::HandlerResult;

use crate::protocol::path_or_bytes;

/// Read file contents
pub async fn read(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Byte offset to start reading from
        #[serde(default)]
        offset: Option<u64>,
        /// Maximum number of bytes to read (default: entire file)
        #[serde(default)]
        length: Option<usize>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    let mut file = File::open(&path)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    // Seek to offset if specified
    if let Some(offset) = params.offset {
        file.seek(SeekFrom::Start(offset))
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
    }

    // Read the content
    let content = if let Some(length) = params.length {
        let mut buf = vec![0u8; length];
        let bytes_read = file
            .read(&mut buf)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
        buf.truncate(bytes_read);
        buf
    } else {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
        buf
    };

    // Return binary content directly (no base64!)
    let size = content.len();
    Ok(msgpack_map! {
        "content" => Value::Binary(content),
        "size" => size
    })
}

/// Write file contents
pub async fn write(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Content to write as binary
        #[serde(with = "serde_bytes")]
        content: Vec<u8>,
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

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    // Content is already binary, no decoding needed!
    let content = params.content;

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
        .open(&path)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    // Seek to offset if specified
    if let Some(offset) = params.offset {
        file.seek(SeekFrom::Start(offset))
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
    }

    // Write the content
    file.write_all(&content)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    // Set permissions if specified
    if let Some(mode) = params.mode {
        let perms = std::fs::Permissions::from_mode(mode);
        fs::set_permissions(&path, perms)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
    }

    Ok(msgpack_map! {
        "written" => content.len()
    })
}

/// Copy a file
pub async fn copy(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
        /// Preserve file attributes
        #[serde(default)]
        preserve: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src_path = bytes_to_path(&params.src);
    let mut dest_path = bytes_to_path(&params.dest).to_path_buf();

    // If destination is a directory, append the source filename
    if dest_path.is_dir() {
        if let Some(filename) = src_path.file_name() {
            dest_path.push(filename);
        }
    }

    let src_str = src_path.to_string_lossy().to_string();

    let src_metadata = fs::metadata(&src_path)
        .await
        .map_err(|e| map_io_error(e, &src_str))?;

    let bytes_copied = if src_metadata.is_dir() {
        // Recursive directory copy
        copy_dir_recursive(&src_path, &dest_path, params.preserve)
            .await
            .map_err(|e| map_io_error(e, &src_str))?
    } else {
        // Copy regular file (or symlink target)
        let n = fs::copy(&src_path, &dest_path)
            .await
            .map_err(|e| map_io_error(e, &src_str))?;

        // Preserve attributes if requested
        if params.preserve {
            let _ = fs::set_permissions(&dest_path, src_metadata.permissions()).await;
            #[cfg(unix)]
            {
                use std::os::unix::fs::MetadataExt;
                let atime = src_metadata.atime();
                let mtime = src_metadata.mtime();
                let dest = dest_path.to_string_lossy().to_string();
                let _ =
                    tokio::task::spawn_blocking(move || set_file_times_sync(&dest, atime, mtime))
                        .await;
            }
        }
        n
    };

    Ok(msgpack_map! {
        "copied" => bytes_copied
    })
}

/// Recursively copy a directory and its contents.
async fn copy_dir_recursive(src: &Path, dest: &Path, preserve: bool) -> std::io::Result<u64> {
    // Create destination directory
    fs::create_dir_all(dest).await?;

    if preserve {
        // Copy permissions from source dir
        let src_meta = fs::metadata(src).await?;
        let _ = fs::set_permissions(dest, src_meta.permissions()).await;
    }

    let mut total: u64 = 0;
    let mut entries = fs::read_dir(src).await?;

    while let Some(entry) = entries.next_entry().await? {
        let entry_path = entry.path();
        let dest_child = dest.join(entry.file_name());
        let file_type = entry.file_type().await?;

        if file_type.is_dir() {
            total += Box::pin(copy_dir_recursive(&entry_path, &dest_child, preserve)).await?;
        } else if file_type.is_symlink() {
            // Preserve symlinks as symlinks
            let link_target = fs::read_link(&entry_path).await?;
            tokio::fs::symlink(&link_target, &dest_child).await?;
        } else {
            let n = fs::copy(&entry_path, &dest_child).await?;
            total += n;

            if preserve {
                if let Ok(meta) = fs::metadata(&entry_path).await {
                    let _ = fs::set_permissions(&dest_child, meta.permissions()).await;
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::MetadataExt;
                        let atime = meta.atime();
                        let mtime = meta.mtime();
                        let dest_str = dest_child.to_string_lossy().to_string();
                        let _ = tokio::task::spawn_blocking(move || {
                            set_file_times_sync(&dest_str, atime, mtime)
                        })
                        .await;
                    }
                }
            }
        }
    }

    Ok(total)
}

/// Rename/move a file
pub async fn rename(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
        /// Overwrite destination if it exists
        #[serde(default)]
        overwrite: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = bytes_to_path(&params.src);
    let dest = bytes_to_path(&params.dest);
    let dest_str = dest.to_string_lossy().to_string();
    let src_str = src.to_string_lossy().to_string();

    // Check if destination exists and overwrite is false
    if !params.overwrite && dest.exists() {
        return Err(RpcError {
            code: RpcError::IO_ERROR,
            message: format!("Destination already exists: {}", dest_str),
            data: None,
        });
    }

    fs::rename(&src, &dest)
        .await
        .map_err(|e| map_io_error(e, &src_str))?;

    Ok(Value::Boolean(true))
}

/// Delete a file
pub async fn delete(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// If true, don't error if file doesn't exist
        #[serde(default)]
        force: bool,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    match fs::remove_file(&path).await {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(e) if params.force && e.kind() == std::io::ErrorKind::NotFound => {
            Ok(Value::Boolean(false))
        }
        Err(e) => Err(map_io_error(e, &path_str)),
    }
}

/// Set file permissions
pub async fn set_modes(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        mode: u32,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().to_string();

    let perms = std::fs::Permissions::from_mode(params.mode);
    fs::set_permissions(&path, perms)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}

/// Set file timestamps
pub async fn set_times(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Modification time (seconds since epoch)
        mtime: i64,
        /// Access time (seconds since epoch, defaults to mtime)
        #[serde(default)]
        atime: Option<i64>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let atime = params.atime.unwrap_or(params.mtime);
    let mtime = params.mtime;

    // Use spawn_blocking for the libc syscall
    tokio::task::spawn_blocking(move || set_file_times_sync_path(&path, atime, mtime))
        .await
        .map_err(|e| RpcError::internal_error(e.to_string()))??;

    Ok(Value::Boolean(true))
}

/// Create a symbolic link
pub async fn make_symlink(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        target: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        link_path: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let target = bytes_to_path(&params.target);
    let link_path = bytes_to_path(&params.link_path);
    let link_path_str = link_path.to_string_lossy().to_string();

    #[cfg(unix)]
    {
        // Try creating the symlink; if it already exists, remove it and retry
        // (matching `ln -sf` behavior needed by tramp lock files).
        match fs::symlink(&target, &link_path).await {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                fs::remove_file(&link_path)
                    .await
                    .map_err(|e| map_io_error(e, &link_path_str))?;
                fs::symlink(&target, &link_path)
                    .await
                    .map_err(|e| map_io_error(e, &link_path_str))?;
            }
            Err(e) => return Err(map_io_error(e, &link_path_str)),
        }
    }

    Ok(Value::Boolean(true))
}

/// Create a hard link
pub async fn make_hardlink(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// The existing file to link to
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        /// The new hard link path
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = bytes_to_path(&params.src);
    let dest = bytes_to_path(&params.dest);
    let dest_str = dest.to_string_lossy().to_string();

    fs::hard_link(&src, &dest)
        .await
        .map_err(|e| map_io_error(e, &dest_str))?;

    Ok(Value::Boolean(true))
}

/// Change file ownership (chown)
pub async fn chown(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// New user ID (-1 to leave unchanged)
        uid: i32,
        /// New group ID (-1 to leave unchanged)
        gid: i32,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let uid = params.uid;
    let gid = params.gid;

    // Use spawn_blocking for the libc syscall
    tokio::task::spawn_blocking(move || {
        use std::os::unix::ffi::OsStrExt;
        let path_bytes = path.as_os_str().as_bytes();
        let mut path_cstr = path_bytes.to_vec();
        path_cstr.push(0);

        let result = unsafe {
            libc::chown(
                path_cstr.as_ptr() as *const libc::c_char,
                uid as libc::uid_t,
                gid as libc::gid_t,
            )
        };

        if result != 0 {
            return Err(RpcError::io_error(std::io::Error::last_os_error()));
        }
        Ok(())
    })
    .await
    .map_err(|e| RpcError::internal_error(e.to_string()))??;

    Ok(Value::Boolean(true))
}

// ============================================================================
// Helper functions
// ============================================================================

#[cfg(unix)]
fn set_file_times_sync(path: &str, atime: i64, mtime: i64) -> Result<(), RpcError> {
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

#[cfg(unix)]
fn set_file_times_sync_path(
    path: &std::path::Path,
    atime: i64,
    mtime: i64,
) -> Result<(), RpcError> {
    use std::os::unix::ffi::OsStrExt;

    let path_bytes = path.as_os_str().as_bytes();
    let mut path_cstr = path_bytes.to_vec();
    path_cstr.push(0); // Null terminate

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

    let result = unsafe {
        libc::utimensat(
            libc::AT_FDCWD,
            path_cstr.as_ptr() as *const libc::c_char,
            times.as_ptr(),
            0,
        )
    };

    if result != 0 {
        return Err(RpcError::io_error(std::io::Error::last_os_error()));
    }

    Ok(())
}
