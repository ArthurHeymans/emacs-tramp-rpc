//! File I/O operations

use crate::msgpack_map;
use crate::protocol::{from_value, RpcError};
use flate2::write::ZlibEncoder;
use flate2::Compression;
use rmpv::Value;
use serde::Deserialize;
use std::io::{SeekFrom, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use tokio::fs::{self, File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};

use super::file::{bytes_to_path, map_io_error};
use super::HandlerResult;

use crate::protocol::path_or_bytes;

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

    // Read the content
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
        // Pre-size from metadata to avoid repeated reallocations on large reads.
        let mut buf = Vec::new();
        if let Ok(metadata) = file.metadata().await {
            let mut expected_len = metadata.len() as usize;
            if let Some(offset) = params.offset {
                expected_len = expected_len.saturating_sub(offset as usize);
            }
            buf.reserve(expected_len);
        }
        file.read_to_end(&mut buf)
            .await
            .map_err(|e| map_io_error(e, &path_str))?;
        buf
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
            "size" => size
        })
    }
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
pub async fn copy(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        src: Vec<u8>,
        #[serde(with = "path_or_bytes")]
        dest: Vec<u8>,
        /// Preserve file attributes (backward-compatible alias for both
        /// permissions and timestamps).
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
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let options = CopyOptions {
        preserve_permissions: params.preserve || params.preserve_permissions,
        preserve_times: params.preserve || params.preserve_times,
        overwrite: params.overwrite,
    };

    let src_path = bytes_to_path(&params.src);
    let mut dest_path = bytes_to_path(&params.dest);

    // If destination is a directory, append the source filename
    if !params.exact_dest && dest_path.is_dir() {
        if let Some(filename) = src_path.file_name() {
            dest_path.push(filename);
        }
    }

    let src_str = src_path.to_string_lossy().into_owned();

    let src_metadata = fs::metadata(&src_path)
        .await
        .map_err(|e| map_io_error(e, &src_str))?;

    let bytes_copied = if src_metadata.is_dir() {
        // Recursive directory copy
        copy_dir_recursive(&src_path, &dest_path, options)
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
}

/// Recursively copy a directory and its contents.
async fn copy_dir_recursive(src: &Path, dest: &Path, options: CopyOptions) -> std::io::Result<u64> {
    ensure_directory_destination(dest).await?;

    let mut total: u64 = 0;
    let mut entries = fs::read_dir(src).await?;

    while let Some(entry) = entries.next_entry().await? {
        let entry_path = entry.path();
        let dest_child = dest.join(entry.file_name());
        let file_type = entry.file_type().await?;

        if file_type.is_dir() {
            total += Box::pin(copy_dir_recursive(&entry_path, &dest_child, options)).await?;
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

async fn ensure_directory_destination(path: &Path) -> std::io::Result<()> {
    match fs::symlink_metadata(path).await {
        Ok(_) => {
            if fs::metadata(path)
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
            let mtime = src_meta.mtime();
            let dest = dest.to_path_buf();
            tokio::task::spawn_blocking(move || set_file_times_sync_path_io(&dest, atime, mtime))
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
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path);
    let path_str = path.to_string_lossy().into_owned();
    let atime = params.atime.unwrap_or(params.mtime);
    let mtime = params.mtime;

    // Use spawn_blocking for the libc syscall
    tokio::task::spawn_blocking(move || set_file_times_sync_path_io(&path, atime, mtime))
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
fn set_file_times_sync_path_io(path: &Path, atime: i64, mtime: i64) -> std::io::Result<()> {
    use std::os::unix::ffi::OsStrExt;

    let path_bytes = path.as_os_str().as_bytes();
    let mut path_cstr = path_bytes.to_vec();
    path_cstr.push(0); // Null terminate

    let times = [
        libc::timespec {
            tv_sec: atime as _,
            tv_nsec: 0,
        },
        libc::timespec {
            tv_sec: mtime as _,
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
        return Err(std::io::Error::last_os_error());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmpv::Value;
    use std::os::unix::ffi::OsStrExt;

    fn path_value(path: &Path) -> Value {
        Value::Binary(path.as_os_str().as_bytes().to_vec())
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
