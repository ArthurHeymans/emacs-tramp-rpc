//! File I/O operations

use crate::msgpack_map;
use crate::protocol::{RpcError, from_value};
use flate2::Compression;
use flate2::write::ZlibEncoder;
use rmpv::Value;
use serde::Deserialize;
use std::io::{SeekFrom, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use tokio::fs::{self, File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};

use super::HandlerResult;
use super::file::{bytes_to_path, map_io_error};

use crate::protocol::path_or_bytes;

const MAX_READ_BYTES: usize = 16 * 1024 * 1024;

/// Read file contents
pub async fn read(params: Value) -> HandlerResult {
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
        /// When true, zlib-compress payload bytes before sending.
        #[serde(default)]
        compress: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.length.is_some_and(|length| length > MAX_READ_BYTES) {
        return Err(RpcError::invalid_params(format!(
            "length exceeds maximum read size of {MAX_READ_BYTES} bytes"
        )));
    }

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    let mut file = File::open(&path)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    // Seek to offset if specified
    if let Some(offset) = params.offset {
        file.seek(SeekFrom::Start(offset))
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
    }

    // Read the content.
    let content = if let Some(length) = params.length {
        // Read up to LENGTH bytes in a single pass. `take` keeps reads bounded.
        let mut buf = Vec::with_capacity(length);
        let mut reader = file.take(length as u64);
        reader
            .read_to_end(&mut buf)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
        buf
    } else {
        let metadata = file
            .metadata()
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
        read_default_bounded(
            &mut file,
            metadata.len().saturating_sub(params.offset.unwrap_or(0)),
            &path_str,
        )
        .await?
    };

    // Return binary content directly (no base64!). Compression is opt-in.
    let size = content.len();
    if params.compress {
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::fast());
        encoder
            .write_all(&content)
            .map_err(|e| RpcError::internal_error(format!("zlib write failed: {e}")))?;
        let compressed = encoder
            .finish()
            .map_err(|e| RpcError::internal_error(format!("zlib finish failed: {e}")))?;
        Ok(msgpack_map! {
            "content" => Value::Binary(compressed),
            "size" => size,
            "compressed" => true,
            "compression" => "zlib"
        })
    } else {
        Ok(msgpack_map! {
            "content" => Value::Binary(content),
            "size" => size,
            "compressed" => false,
            "compression" => Value::Nil
        })
    }
}

async fn read_default_bounded(
    file: &mut File,
    expected_len: u64,
    path: &str,
) -> Result<Vec<u8>, RpcError> {
    if expected_len > MAX_READ_BYTES as u64 {
        return Err(RpcError::invalid_params(format!(
            "file exceeds maximum read size of {MAX_READ_BYTES} bytes"
        )));
    }

    let mut buf = Vec::with_capacity(expected_len as usize);
    file.take(MAX_READ_BYTES as u64 + 1)
        .read_to_end(&mut buf)
        .await
        .map_err(|e| map_io_error(e, path))?;
    if buf.len() > MAX_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "file exceeds maximum read size of {MAX_READ_BYTES} bytes"
        )));
    }
    Ok(buf)
}

/// Write file contents
pub async fn write(params: Value) -> HandlerResult {
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

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    // Content is already binary, no decoding needed!
    let content = params.content;

    // Open the file with appropriate options
    let mut options = OpenOptions::new();

    if params.append {
        options.append(true).create(true);
    } else if params.offset.is_some() {
        // Seeking past EOF then writing is the sparse, zero-filled behavior
        // Emacs expects for an integer `write-region' offset.
        options.write(true).create(true);
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
    // Do not report success until Tokio has completed the pending file writes.
    file.flush().await.map_err(|e| map_io_error(e, &path_str))?;

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

/// Copy a file or directory.
pub async fn copy(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
        /// Public protocol alias for preserving both permissions and timestamps.
        #[serde(default)]
        preserve: bool,
        /// Preserve permission bits without forcing timestamp preservation.
        #[serde(default)]
        preserve_permissions: bool,
        /// Preserve access and modification times.
        #[serde(default)]
        preserve_times: bool,
        /// Overwrite existing destination entries where possible.
        #[serde(default)]
        overwrite: bool,
        /// Treat `dest` as the exact destination path, even when it names an
        /// existing directory.  The default keeps the historical `file.copy`
        /// behavior of copying into an existing destination directory.
        #[serde(default)]
        exact_dest: bool,
        /// Allow recursive directory copies to merge into pre-existing child
        /// directories.  This defaults to true as part of the public `file.copy`
        /// protocol; callers that emulate Emacs `copy-directory` with nil
        /// PARENTS set it to false.
        #[serde(default = "default_true")]
        merge_existing_directories: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let options = CopyOptions {
        preserve_permissions: params.preserve || params.preserve_permissions,
        preserve_times: params.preserve || params.preserve_times,
        overwrite: params.overwrite,
        merge_existing_directories: params.merge_existing_directories,
    };

    let src_path = bytes_to_path(&params.src);
    let mut dest_path = bytes_to_path(&params.dest);

    // If destination is a directory, append the source filename
    if !params.exact_dest
        && fs::metadata(&dest_path)
            .await
            .map(|meta| meta.is_dir())
            .unwrap_or(false)
        && let Some(filename) = src_path.file_name()
    {
        dest_path.push(filename);
    }

    let src_str = src_path.to_string_lossy().into_owned();

    let src_metadata = fs::metadata(&src_path)
        .await
        .map_err(|e| map_io_error(e, &src_str))?;

    let bytes_copied = if src_metadata.is_dir() {
        reject_recursive_self_copy(&src_path, &dest_path)
            .await
            .map_err(|e| map_io_error(e, &src_str))?;
        // Recursive directory copy
        copy_dir_recursive(&src_path, &dest_path, options, true)
            .await
            .map_err(|e| map_io_error(e, &src_str))?
    } else {
        // Copy regular file (or symlink target)
        prepare_regular_destination(&dest_path, options.overwrite)
            .await
            .map_err(|e| map_io_error(e, &src_str))?;
        let n = fs::copy(&src_path, &dest_path)
            .await
            .map_err(|e| map_io_error(e, &src_str))?;

        apply_copied_metadata(&src_metadata, &dest_path, options)
            .await
            .map_err(|e| map_io_error(e, &src_str))?;
        n
    };

    Ok(msgpack_map! {
        "copied" => bytes_copied
    })
}

#[derive(Clone, Copy)]
struct CopyOptions {
    preserve_permissions: bool,
    preserve_times: bool,
    overwrite: bool,
    merge_existing_directories: bool,
}

fn default_true() -> bool {
    true
}

/// Recursively copy a directory and its contents.
async fn copy_dir_recursive(
    src: &Path,
    dest: &Path,
    options: CopyOptions,
    allow_existing_dest_dir: bool,
) -> std::io::Result<u64> {
    ensure_directory_destination(dest, allow_existing_dest_dir).await?;

    let mut total: u64 = 0;
    let mut entries = fs::read_dir(src).await?;

    while let Some(entry) = entries.next_entry().await? {
        let entry_path = entry.path();
        let dest_child = dest.join(entry.file_name());
        let file_type = entry.file_type().await?;

        if file_type.is_dir() {
            total += Box::pin(copy_dir_recursive(
                &entry_path,
                &dest_child,
                options,
                options.merge_existing_directories,
            ))
            .await?;
        } else if file_type.is_symlink() {
            // Preserve symlinks as symlinks
            let link_target = fs::read_link(&entry_path).await?;
            prepare_symlink_destination(&dest_child, options.overwrite).await?;
            tokio::fs::symlink(&link_target, &dest_child).await?;
        } else {
            prepare_regular_destination(&dest_child, options.overwrite).await?;
            let n = fs::copy(&entry_path, &dest_child).await?;
            total += n;

            let meta = fs::metadata(&entry_path).await?;
            apply_copied_metadata(&meta, &dest_child, options).await?;
        }
    }

    let src_meta = fs::metadata(src).await?;
    apply_copied_metadata(&src_meta, dest, options).await?;

    Ok(total)
}

async fn ensure_directory_destination(path: &Path, allow_existing: bool) -> std::io::Result<()> {
    match fs::symlink_metadata(path).await {
        Ok(_) => {
            if allow_existing
                && fs::metadata(path)
                    .await
                    .map(|meta| meta.is_dir())
                    .unwrap_or(false)
            {
                Ok(())
            } else {
                Err(already_exists(path))
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => fs::create_dir_all(path).await,
        Err(e) => Err(e),
    }
}

async fn reject_recursive_self_copy(src: &Path, dest: &Path) -> std::io::Result<()> {
    let src = fs::canonicalize(src).await?;
    let dest = canonicalize_existing_ancestor(dest).await?;

    if dest.starts_with(&src) {
        Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!(
                "Cannot copy directory {} into its descendant {}",
                src.display(),
                dest.display()
            ),
        ))
    } else {
        Ok(())
    }
}

async fn canonicalize_existing_ancestor(path: &Path) -> std::io::Result<PathBuf> {
    let mut ancestor = path.to_path_buf();
    let mut suffix = PathBuf::new();

    loop {
        match fs::symlink_metadata(&ancestor).await {
            Ok(_) => return Ok(fs::canonicalize(&ancestor).await?.join(suffix)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                let Some(name) = ancestor.file_name() else {
                    return Err(e);
                };
                suffix = Path::new(name).join(suffix);
                let Some(parent) = ancestor.parent() else {
                    return Err(e);
                };
                ancestor = parent.to_path_buf();
            }
            Err(e) => return Err(e),
        }
    }
}

async fn prepare_regular_destination(path: &Path, overwrite: bool) -> std::io::Result<()> {
    if overwrite {
        return Ok(());
    }

    match fs::symlink_metadata(path).await {
        Ok(_) => Err(already_exists(path)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e),
    }
}

async fn prepare_symlink_destination(path: &Path, overwrite: bool) -> std::io::Result<()> {
    match fs::symlink_metadata(path).await {
        Ok(meta) => {
            if !overwrite {
                return Err(already_exists(path));
            }
            if meta.is_dir() && !meta.file_type().is_symlink() {
                return Err(already_exists(path));
            }
            remove_path_for_overwrite(path).await
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e),
    }
}

fn already_exists(path: &Path) -> std::io::Error {
    std::io::Error::new(
        std::io::ErrorKind::AlreadyExists,
        format!("Destination already exists: {}", path.display()),
    )
}

async fn apply_copied_metadata(
    src_meta: &std::fs::Metadata,
    dest: &Path,
    options: CopyOptions,
) -> std::io::Result<()> {
    if options.preserve_permissions {
        fs::set_permissions(dest, src_meta.permissions()).await?;
    }

    if options.preserve_times {
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;

            let atime = src_meta.atime();
            let atime_nsec = src_meta.atime_nsec();
            let mtime = src_meta.mtime();
            let mtime_nsec = src_meta.mtime_nsec();
            let dest = dest.to_path_buf();
            tokio::task::spawn_blocking(move || {
                set_file_times_sync_path_io(&dest, atime, atime_nsec, mtime, mtime_nsec, false)
            })
            .await
            .map_err(std::io::Error::other)??;
        }
    }

    Ok(())
}

async fn remove_path_for_overwrite(path: &Path) -> std::io::Result<()> {
    let meta = fs::symlink_metadata(path).await?;
    if meta.is_dir() && !meta.file_type().is_symlink() {
        fs::remove_dir_all(path).await
    } else {
        fs::remove_file(path).await
    }
}

/// Rename/move a file
pub async fn rename(params: Value) -> HandlerResult {
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

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = bytes_to_path(&params.src);
    let dest = bytes_to_path(&params.dest);
    let dest_str = dest.to_string_lossy().into_owned();
    let src_str = src.to_string_lossy().into_owned();

    // `exists` follows links and misses dangling destinations.  lstat instead
    // preserves no-overwrite semantics for every directory entry type.
    if !params.overwrite {
        match fs::symlink_metadata(&dest).await {
            Ok(_) => return Err(map_io_error(already_exists(&dest), &dest_str)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => return Err(map_io_error(e, &dest_str)),
        }
    }

    fs::rename(&src, &dest)
        .await
        .map_err(|e| map_io_error(e, &src_str))?;

    Ok(Value::Boolean(true))
}

/// Delete a file
pub async fn delete(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// If true, don't error if file doesn't exist
        #[serde(default)]
        force: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    match fs::remove_file(&path).await {
        Ok(()) => Ok(Value::Boolean(true)),
        Err(e) if params.force && e.kind() == std::io::ErrorKind::NotFound => {
            Ok(Value::Boolean(false))
        }
        Err(e) => Err(map_io_error(e, &path_str)),
    }
}

/// Set file permissions
pub async fn set_modes(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        mode: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();

    let perms = std::fs::Permissions::from_mode(params.mode);
    fs::set_permissions(&path, perms)
        .await
        .map_err(|e| map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}

/// Set file timestamps
pub async fn set_times(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// Modification time (seconds since epoch)
        mtime: i64,
        /// Access time (seconds since epoch, defaults to mtime)
        #[serde(default)]
        atime: Option<i64>,
        /// If true, update a symlink itself instead of following it.
        #[serde(default)]
        nofollow: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();
    let atime = params.atime.unwrap_or(params.mtime);
    let mtime = params.mtime;
    let nofollow = params.nofollow;

    // Use spawn_blocking for the libc syscall
    tokio::task::spawn_blocking(move || {
        set_file_times_sync_path_io(&path, atime, 0, mtime, 0, nofollow)
    })
    .await
    .map_err(|e| RpcError::internal_error(e.to_string()))?
    .map_err(|e| map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}

/// Create a symbolic link
pub async fn make_symlink(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        target: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        link_path: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let target = bytes_to_path(&params.target);
    let link_path = bytes_to_path(&params.link_path);
    let link_path_str = link_path.to_string_lossy().into_owned();

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
pub async fn make_hardlink(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// The existing file to link to
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        /// The new hard link path
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = bytes_to_path(&params.src);
    let dest = bytes_to_path(&params.dest);
    let dest_str = dest.to_string_lossy().into_owned();

    fs::hard_link(&src, &dest)
        .await
        .map_err(|e| map_io_error(e, &dest_str))?;

    Ok(Value::Boolean(true))
}

/// Change file ownership (chown)
pub async fn chown(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// New user ID (-1 to leave unchanged)
        uid: i32,
        /// New group ID (-1 to leave unchanged)
        gid: i32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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
fn set_file_times_sync_path_io(
    path: &Path,
    atime: i64,
    atime_nsec: i64,
    mtime: i64,
    mtime_nsec: i64,
    nofollow: bool,
) -> std::io::Result<()> {
    use std::os::unix::ffi::OsStrExt;

    let path_bytes = path.as_os_str().as_bytes();
    let mut path_cstr = path_bytes.to_vec();
    path_cstr.push(0); // Null terminate

    let times = [
        libc::timespec {
            tv_sec: atime as _,
            tv_nsec: atime_nsec as _,
        },
        libc::timespec {
            tv_sec: mtime as _,
            tv_nsec: mtime_nsec as _,
        },
    ];

    let result = unsafe {
        libc::utimensat(
            libc::AT_FDCWD,
            path_cstr.as_ptr() as *const libc::c_char,
            times.as_ptr(),
            if nofollow {
                libc::AT_SYMLINK_NOFOLLOW
            } else {
                0
            },
        )
    };

    if result != 0 {
        return Err(std::io::Error::last_os_error());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmpv::Value;
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::MetadataExt;

    fn path_value(path: &Path) -> Value {
        Value::Binary(path.as_os_str().as_bytes().to_vec())
    }

    #[tokio::test]
    async fn write_offset_preserves_suffix() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"abcdef").await.unwrap();

        write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(b"XY".to_vec()),
            "offset" => 2u64,
        })
        .await
        .expect("write at offset");

        assert_eq!(fs::read(path).await.unwrap(), b"abXYef");
    }

    #[tokio::test]
    async fn write_offset_creates_zero_filled_file() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");

        write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(b"XY".to_vec()),
            "offset" => 4u64,
        })
        .await
        .expect("write at offset creates file");

        assert_eq!(fs::read(path).await.unwrap(), b"\0\0\0\0XY");
    }

    #[tokio::test]
    async fn rename_no_overwrite_rejects_dangling_symlink() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"source").await.unwrap();
        tokio::fs::symlink("missing-target", &dest).await.unwrap();

        let error = rename(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
        })
        .await
        .expect_err("dangling symlink is an existing destination");

        assert_eq!(error.code, RpcError::IO_ERROR);
        let errno = error
            .data
            .as_ref()
            .and_then(Value::as_map)
            .and_then(|data| {
                data.iter()
                    .find(|(key, _)| key.as_str() == Some("os_errno"))
            })
            .and_then(|(_, value)| value.as_i64());
        assert_eq!(errno, Some(i64::from(libc::EEXIST)));
        assert!(fs::symlink_metadata(&dest).await.is_ok());
        assert!(fs::metadata(&src).await.is_ok());
    }

    #[tokio::test]
    async fn read_size_limit_rejects_oversized_request() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"x").await.unwrap();

        let error = read(msgpack_map! {
            "path" => path_value(&path),
            "length" => (MAX_READ_BYTES + 1) as u64,
        })
        .await
        .expect_err("oversized read must fail before allocating");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn default_read_rejects_file_that_grows_after_metadata() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"x").await.unwrap();
        let mut file = File::open(&path).await.unwrap();
        let expected_len = file.metadata().await.unwrap().len();
        fs::write(&path, vec![b'x'; MAX_READ_BYTES + 1])
            .await
            .unwrap();

        let error = read_default_bounded(&mut file, expected_len, &path.to_string_lossy())
            .await
            .expect_err("grown file must remain bounded");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn copy_directory_overwrites_existing_entries() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");

        fs::create_dir_all(src.join("subdir")).await.unwrap();
        fs::write(src.join("file.txt"), b"new file").await.unwrap();
        fs::write(src.join("subdir/nested.txt"), b"new nested")
            .await
            .unwrap();
        tokio::fs::symlink("file.txt", src.join("link.txt"))
            .await
            .unwrap();

        let copied_dest = dest.join("src");
        fs::create_dir_all(copied_dest.join("subdir"))
            .await
            .unwrap();
        fs::write(copied_dest.join("file.txt"), b"old file")
            .await
            .unwrap();
        fs::write(copied_dest.join("subdir/nested.txt"), b"old nested")
            .await
            .unwrap();
        fs::write(copied_dest.join("link.txt"), b"old link placeholder")
            .await
            .unwrap();

        copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "overwrite" => true,
        })
        .await
        .expect("copy should succeed with overwrite");

        assert_eq!(
            fs::read(copied_dest.join("file.txt")).await.unwrap(),
            b"new file"
        );
        assert_eq!(
            fs::read(copied_dest.join("subdir/nested.txt"))
                .await
                .unwrap(),
            b"new nested"
        );
        assert_eq!(
            fs::read_link(copied_dest.join("link.txt")).await.unwrap(),
            Path::new("file.txt")
        );
    }

    #[tokio::test]
    async fn copy_directory_without_overwrite_rejects_existing_symlink_dest() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");

        fs::create_dir_all(&src).await.unwrap();
        let copied_dest = dest.join("src");
        fs::create_dir_all(&copied_dest).await.unwrap();
        tokio::fs::symlink("target", src.join("link.txt"))
            .await
            .unwrap();
        fs::write(copied_dest.join("link.txt"), b"already here")
            .await
            .unwrap();

        let err = copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
        })
        .await
        .expect_err("copy should fail without overwrite");

        assert!(err.message.contains("File exists") || err.message.contains("exists"));
    }

    #[tokio::test]
    async fn copy_directory_exact_dest_merges_existing_directory() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest").join("src");

        fs::create_dir_all(&src).await.unwrap();
        fs::write(src.join("file.txt"), b"new").await.unwrap();

        fs::create_dir_all(&dest).await.unwrap();
        fs::write(dest.join("file.txt"), b"old").await.unwrap();

        copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "overwrite" => true,
            "exact_dest" => true,
        })
        .await
        .expect("copy should merge into exact destination");

        assert_eq!(fs::read(dest.join("file.txt")).await.unwrap(), b"new");
        assert!(
            !dest.join("src").exists(),
            "exact_dest must not append basename"
        );
    }

    #[tokio::test]
    async fn copy_directory_preserves_file_and_directory_times() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let child = src.join("child");
        let file = child.join("file.txt");
        let dest = tmp.path().join("dest");

        fs::create_dir_all(&child).await.unwrap();
        fs::write(&file, b"payload").await.unwrap();

        let src_time = 1_600_000_001;
        let child_time = 1_600_000_002;
        let file_time = 1_600_000_003;
        let src_nsec = 101_000_001;
        let child_nsec = 102_000_002;
        let file_nsec = 103_000_003;
        set_file_times_sync_path_io(&file, file_time, file_nsec, file_time, file_nsec, false)
            .unwrap();
        set_file_times_sync_path_io(
            &child, child_time, child_nsec, child_time, child_nsec, false,
        )
        .unwrap();
        set_file_times_sync_path_io(&src, src_time, src_nsec, src_time, src_nsec, false).unwrap();

        copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "exact_dest" => true,
            "preserve_times" => true,
        })
        .await
        .expect("copy should preserve times");

        let dest_meta = fs::metadata(&dest).await.unwrap();
        assert_eq!(dest_meta.mtime(), src_time);
        assert_eq!(dest_meta.mtime_nsec(), src_nsec);
        let child_meta = fs::metadata(dest.join("child")).await.unwrap();
        assert_eq!(child_meta.mtime(), child_time);
        assert_eq!(child_meta.mtime_nsec(), child_nsec);
        let file_meta = fs::metadata(dest.join("child/file.txt")).await.unwrap();
        assert_eq!(file_meta.mtime(), file_time);
        assert_eq!(file_meta.mtime_nsec(), file_nsec);
    }

    #[tokio::test]
    async fn copy_directory_can_reject_existing_child_directories() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");

        fs::create_dir_all(src.join("child")).await.unwrap();
        fs::write(src.join("child/new.txt"), b"new").await.unwrap();

        fs::create_dir_all(dest.join("child")).await.unwrap();
        fs::write(dest.join("child/old.txt"), b"old").await.unwrap();

        let err = copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "overwrite" => true,
            "exact_dest" => true,
            "merge_existing_directories" => false,
        })
        .await
        .expect_err("copy should reject an existing child directory");

        assert!(err.message.contains("exists"));
        let errno = err
            .data
            .as_ref()
            .and_then(|data| data.as_map())
            .and_then(|data| data.iter().find(|(k, _)| k.as_str() == Some("os_errno")))
            .and_then(|(_, v)| v.as_i64())
            .expect("os_errno");
        assert_eq!(errno, i64::from(libc::EEXIST));
        assert_eq!(fs::read(dest.join("child/old.txt")).await.unwrap(), b"old");
    }

    #[tokio::test]
    async fn copy_directory_rejects_recursive_self_copy() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = src.join("nested-copy");

        fs::create_dir_all(&src).await.unwrap();
        fs::write(src.join("file.txt"), b"payload").await.unwrap();

        let err = copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "overwrite" => true,
            "exact_dest" => true,
        })
        .await
        .expect_err("copying a directory into itself must be rejected");

        assert!(err.message.contains("Cannot copy directory"));
    }

    #[tokio::test]
    async fn copy_regular_file_without_overwrite_rejects_existing_file() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src.txt");
        let dest = tmp.path().join("dest.txt");

        fs::write(&src, b"new").await.unwrap();
        fs::write(&dest, b"old").await.unwrap();

        let err = copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
        })
        .await
        .expect_err("copy should fail without overwrite");

        assert!(err.message.contains("exists"));
        assert_eq!(fs::read(&dest).await.unwrap(), b"old");
    }

    #[tokio::test]
    async fn copy_directory_sets_permissions_after_children() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");

        fs::create_dir_all(src.join("child")).await.unwrap();
        fs::write(src.join("child/file.txt"), b"payload")
            .await
            .unwrap();

        let readonly = std::fs::Permissions::from_mode(0o555);
        fs::set_permissions(src.join("child"), readonly.clone())
            .await
            .unwrap();
        fs::set_permissions(&src, readonly).await.unwrap();

        copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "preserve_permissions" => true,
        })
        .await
        .expect("copy should not chmod destination before copying children");

        let copied = dest;
        assert_eq!(
            fs::read(copied.join("child/file.txt")).await.unwrap(),
            b"payload"
        );
        assert_eq!(
            fs::metadata(&copied).await.unwrap().permissions().mode() & 0o777,
            0o555
        );
        assert_eq!(
            fs::metadata(copied.join("child"))
                .await
                .unwrap()
                .permissions()
                .mode()
                & 0o777,
            0o555
        );

        let writable = std::fs::Permissions::from_mode(0o755);
        fs::set_permissions(&src, writable.clone()).await.unwrap();
        fs::set_permissions(src.join("child"), writable.clone())
            .await
            .unwrap();
        fs::set_permissions(&copied, writable.clone())
            .await
            .unwrap();
        fs::set_permissions(copied.join("child"), writable)
            .await
            .unwrap();
    }
}
