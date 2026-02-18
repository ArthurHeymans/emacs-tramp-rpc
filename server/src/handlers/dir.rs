//! Directory operations
//!
//! Optimized to use:
//! - `d_type` from readdir to get file type without extra syscalls
//! - `fstatat` with directory fd for efficient attribute collection
//! - Synchronous blocking task to avoid per-entry async overhead

use crate::protocol::{from_value, DirEntry, FileAttributes, FileType, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use tokio::fs;

use super::file::{bytes_to_path, map_io_error};
use super::HandlerResult;

use crate::protocol::path_or_bytes;

/// Extract time and mode fields from libc::stat in a cross-platform way
/// Returns (atime, mtime, ctime, mode)
/// - On Linux: st_mode is u32, time fields are i64
/// - On macOS: st_mode is u16, time fields are i64
#[inline]
fn extract_stat_fields(stat_buf: &libc::stat) -> (i64, i64, i64, u32) {
    #[cfg(target_os = "macos")]
    let mode = stat_buf.st_mode as u32;
    #[cfg(not(target_os = "macos"))]
    let mode = stat_buf.st_mode;

    (
        stat_buf.st_atime,
        stat_buf.st_mtime,
        stat_buf.st_ctime,
        mode,
    )
}

/// Get FileAttributes using fstatat relative to directory fd
fn get_file_attributes_at(
    dir_fd: libc::c_int,
    name: &[u8],
    follow_symlinks: bool,
) -> Option<FileAttributes> {
    let mut stat_buf: libc::stat = unsafe { std::mem::zeroed() };

    // Create null-terminated name
    let mut name_cstr = name.to_vec();
    name_cstr.push(0);

    let flags = if follow_symlinks {
        0
    } else {
        libc::AT_SYMLINK_NOFOLLOW
    };

    let result = unsafe {
        libc::fstatat(
            dir_fd,
            name_cstr.as_ptr() as *const libc::c_char,
            &mut stat_buf,
            flags,
        )
    };

    if result != 0 {
        return None;
    }

    // Determine file type from stat mode
    let file_type = match stat_buf.st_mode & libc::S_IFMT {
        libc::S_IFREG => FileType::File,
        libc::S_IFDIR => FileType::Directory,
        libc::S_IFLNK => FileType::Symlink,
        libc::S_IFCHR => FileType::CharDevice,
        libc::S_IFBLK => FileType::BlockDevice,
        libc::S_IFIFO => FileType::Fifo,
        libc::S_IFSOCK => FileType::Socket,
        _ => FileType::Unknown,
    };

    // Get link target if symlink
    let link_target = if file_type == FileType::Symlink {
        let full_name = if name == b"." || name == b".." {
            // For . and .., we need the full path for readlink
            None
        } else {
            // Use readlinkat with the dir fd
            let mut buf = vec![0u8; 4096];
            let len = unsafe {
                libc::readlinkat(
                    dir_fd,
                    name_cstr.as_ptr() as *const libc::c_char,
                    buf.as_mut_ptr() as *mut libc::c_char,
                    buf.len(),
                )
            };
            if len >= 0 {
                buf.truncate(len as usize);
                Some(String::from_utf8_lossy(&buf).to_string())
            } else {
                None
            }
        };
        full_name
    } else {
        None
    };

    let uid = stat_buf.st_uid;
    let gid = stat_buf.st_gid;
    let (atime, mtime, ctime, mode) = extract_stat_fields(&stat_buf);

    Some(FileAttributes {
        file_type,
        nlinks: stat_buf.st_nlink as u64,
        uid,
        gid,
        uname: super::file::get_user_name(uid),
        gname: super::file::get_group_name(gid),
        atime,
        mtime,
        ctime,
        size: stat_buf.st_size as u64,
        mode,
        inode: stat_buf.st_ino as u64,
        dev: stat_buf.st_dev as u64,
        link_target,
    })
}

/// List directory contents using optimized synchronous I/O with d_type and fstatat
pub async fn list(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Include file attributes for each entry
        #[serde(default)]
        include_attrs: bool,
        /// Include hidden files (starting with .)
        #[serde(default = "default_true")]
        include_hidden: bool,
    }

    fn default_true() -> bool {
        true
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();
    let include_attrs = params.include_attrs;
    let include_hidden = params.include_hidden;

    // Do all I/O in a single blocking task for efficiency
    let results =
        tokio::task::spawn_blocking(move || list_dir_sync(&path, include_attrs, include_hidden))
            .await
            .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
            .map_err(|e| map_io_error(e, &path_str))?;

    // Convert to array of map values with named fields
    let values: Vec<Value> = results.iter().map(|e| e.to_value()).collect();
    Ok(Value::Array(values))
}

/// Synchronous directory listing with d_type and fstatat optimizations
fn list_dir_sync(
    path: &Path,
    include_attrs: bool,
    include_hidden: bool,
) -> Result<Vec<DirEntry>, std::io::Error> {
    // Open directory fd for fstatat
    let dir_fd = if include_attrs {
        let mut path_cstr = path.as_os_str().as_bytes().to_vec();
        path_cstr.push(0);
        let fd = unsafe {
            libc::open(
                path_cstr.as_ptr() as *const libc::c_char,
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error());
        }
        Some(fd)
    } else {
        None
    };

    // Ensure we close the fd on all exit paths
    struct DirFdGuard(Option<libc::c_int>);
    impl Drop for DirFdGuard {
        fn drop(&mut self) {
            if let Some(fd) = self.0 {
                unsafe {
                    libc::close(fd);
                }
            }
        }
    }
    let _guard = DirFdGuard(dir_fd);

    let mut results: Vec<DirEntry> = Vec::new();

    // Add . and .. entries
    if include_hidden {
        results.push(DirEntry {
            name: b".".to_vec(),
            file_type: FileType::Directory,
            attrs: dir_fd.and_then(|fd| get_file_attributes_at(fd, b".", true)),
        });

        results.push(DirEntry {
            name: b"..".to_vec(),
            file_type: FileType::Directory,
            attrs: dir_fd.and_then(|fd| get_file_attributes_at(fd, b"..", true)),
        });
    }

    // Use std::fs::read_dir which exposes d_type on Linux via DirEntry::file_type()
    let read_dir = std::fs::read_dir(path)?;

    for entry_result in read_dir {
        let entry = entry_result?;
        let name_bytes = entry.file_name().as_bytes().to_vec();

        // Skip hidden files if not requested
        let is_hidden = name_bytes.first() == Some(&b'.');
        if !include_hidden && is_hidden {
            continue;
        }

        // Get file type - std::fs::DirEntry::file_type() uses d_type on Linux
        // (no extra syscall needed unless d_type is DT_UNKNOWN)
        let file_type = match entry.file_type() {
            Ok(ft) => file_type_from_metadata_ft(&ft),
            Err(_) => FileType::Unknown,
        };

        let attrs = if include_attrs {
            // Use lstat (follow_symlinks=false) so symlinks show as symlinks
            // with their link_target resolved, matching Emacs expectations
            dir_fd.and_then(|fd| get_file_attributes_at(fd, &name_bytes, false))
        } else {
            None
        };

        results.push(DirEntry {
            name: name_bytes,
            file_type,
            attrs,
        });
    }

    // Sort by name
    results.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(results)
}

/// Convert std::fs::FileType to our FileType
fn file_type_from_metadata_ft(ft: &std::fs::FileType) -> FileType {
    use std::os::unix::fs::FileTypeExt;
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

/// Create a directory
pub async fn create(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Create parent directories if they don't exist
        #[serde(default)]
        parents: bool,
        /// Directory mode (permissions)
        #[serde(default = "default_mode")]
        mode: u32,
    }

    fn default_mode() -> u32 {
        0o755
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    let result = if params.parents {
        fs::create_dir_all(&path).await
    } else {
        fs::create_dir(&path).await
    };

    result.map_err(|e| map_io_error(e, &path_str))?;

    // Set permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(params.mode);
        fs::set_permissions(&path, perms)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
    }

    Ok(Value::Boolean(true))
}

/// Remove a directory
pub async fn remove(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Remove recursively
        #[serde(default)]
        recursive: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    let result = if params.recursive {
        fs::remove_dir_all(&path).await
    } else {
        fs::remove_dir(&path).await
    };

    result.map_err(|e| map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}
