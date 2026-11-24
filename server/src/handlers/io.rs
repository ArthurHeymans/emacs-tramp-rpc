// SPDX-License-Identifier: GPL-3.0-or-later

//! File I/O operations

use crate::msgpack_map;
use crate::protocol::{RpcError, RpcPath, from_value};
use flate2::Compression;
use flate2::write::ZlibEncoder;
use flate2::{Decompress, FlushDecompress, Status};
use rmpv::Value;
use serde::Deserialize;
use std::io::{Read, Seek, SeekFrom, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use tokio::fs;

use super::HandlerResult;
use super::file::{file_attributes_from_metadata, map_io_error};

pub(crate) const MAX_FILE_READ_CHUNK_BYTES: usize = 16 * 1024 * 1024;

/// Read file contents
pub async fn read(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// Byte offset to start reading from
        #[serde(default)]
        offset: Option<u64>,
        /// Maximum number of bytes to read (default: entire file)
        #[serde(default)]
        length: Option<usize>,
        /// When true, zlib-compress payload bytes before sending (if smaller).
        #[serde(default)]
        compress: bool,
        /// When true, include `stat` (fstat attributes), `writable` (effective
        /// W_OK check or nil if unsupported), and `is_symlink` in the response.
        #[serde(default)]
        include_meta: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params
        .length
        .is_some_and(|length| length > MAX_FILE_READ_CHUNK_BYTES)
    {
        return Err(RpcError::invalid_params(format!(
            "length exceeds maximum read size of {MAX_FILE_READ_CHUNK_BYTES} bytes"
        )));
    }

    super::run_blocking("File read", move || {
        let path = params.path.resolve_sync();
        let path_str = path.to_string_lossy().into_owned();

        let mut file = std::fs::File::open(&path).map_err(|e| map_io_error(e, &path_str))?;

        // fstat on the open fd: lets chunked clients learn the total size from
        // the first chunk response instead of spending a separate file.stat RPC.
        let metadata = file.metadata().map_err(|e| map_io_error(e, &path_str))?;
        if metadata.is_dir() {
            return Err(map_io_error(
                std::io::Error::from_raw_os_error(libc::EISDIR),
                &path_str,
            ));
        }
        let file_size = metadata.len();

        let offset = params.offset.unwrap_or(0);
        if let Some(offset) = params.offset {
            file.seek(SeekFrom::Start(offset))
                .map_err(|e| map_io_error(e, &path_str))?;
        }

        let remaining = file_size.saturating_sub(offset);
        let content = if let Some(length) = params.length {
            // Cap initial capacity by remaining file size so a large requested
            // chunk length on a small file does not eagerly allocate 16 MiB.
            let initial_cap = length.min(remaining as usize);
            let mut buf = Vec::with_capacity(initial_cap);
            file.take(length as u64)
                .read_to_end(&mut buf)
                .map_err(|e| map_io_error(e, &path_str))?;
            buf
        } else {
            read_default_bounded_sync(&mut file, remaining, &path_str)?
        };

        let size = content.len();
        let (payload, compressed, compression) = if params.compress {
            let mut encoder = ZlibEncoder::new(Vec::new(), Compression::fast());
            encoder
                .write_all(&content)
                .map_err(|e| RpcError::internal_error(format!("zlib write failed: {e}")))?;
            let encoded = encoder
                .finish()
                .map_err(|e| RpcError::internal_error(format!("zlib finish failed: {e}")))?;
            if encoded.len() < content.len() {
                (encoded, true, Value::String("zlib".into()))
            } else {
                (content, false, Value::Nil)
            }
        } else {
            (content, false, Value::Nil)
        };

        let mut fields = vec![
            (Value::String("content".into()), Value::Binary(payload)),
            (Value::String("size".into()), Value::from(size)),
            (Value::String("file_size".into()), Value::from(file_size)),
            (
                Value::String("compressed".into()),
                Value::Boolean(compressed),
            ),
            (Value::String("compression".into()), compression),
        ];

        if params.include_meta {
            let is_symlink = std::fs::symlink_metadata(&path)
                .map(|m| m.file_type().is_symlink())
                .unwrap_or(false);
            let stat = file_attributes_from_metadata(&metadata, None).into_value();
            let writable = match crate::access::check(&path, rustix::fs::Access::WRITE_OK) {
                Ok(()) => Value::Boolean(true),
                Err(crate::access::Error::Filesystem(_)) => Value::Boolean(false),
                Err(crate::access::Error::Unsupported(_)) => Value::Nil,
            };
            fields.push((Value::String("stat".into()), stat));
            fields.push((Value::String("writable".into()), writable));
            fields.push((
                Value::String("is_symlink".into()),
                Value::Boolean(is_symlink),
            ));
        }

        Ok(Value::Map(fields))
    })
    .await
}

fn read_default_bounded_sync(
    file: &mut std::fs::File,
    expected_len: u64,
    path: &str,
) -> Result<Vec<u8>, RpcError> {
    if expected_len > MAX_FILE_READ_CHUNK_BYTES as u64 {
        return Err(RpcError::invalid_params(format!(
            "file exceeds maximum read size of {MAX_FILE_READ_CHUNK_BYTES} bytes"
        )));
    }

    let mut buf = Vec::with_capacity(expected_len as usize);
    file.take(MAX_FILE_READ_CHUNK_BYTES as u64 + 1)
        .read_to_end(&mut buf)
        .map_err(|e| map_io_error(e, path))?;
    if buf.len() > MAX_FILE_READ_CHUNK_BYTES {
        return Err(RpcError::invalid_params(format!(
            "file exceeds maximum read size of {MAX_FILE_READ_CHUNK_BYTES} bytes"
        )));
    }
    Ok(buf)
}

/// Decode a zlib stream that must reach its end marker before any file is
/// opened: a truncated or empty payload must not truncate the destination.
fn inflate_complete_zlib(input: &[u8], limit: usize) -> Result<Vec<u8>, RpcError> {
    let mut inflater = Decompress::new(true);
    let mut decoded = Vec::with_capacity(input.len().saturating_mul(4).max(64).min(limit + 1));
    loop {
        if decoded.len() > limit {
            return Err(RpcError::invalid_params(
                "decompressed write payload exceeds maximum frame size",
            ));
        }
        if decoded.len() == decoded.capacity() {
            decoded.reserve((decoded.capacity()).min(limit + 1 - decoded.len()).max(1));
        }
        let before = (inflater.total_in(), inflater.total_out());
        let consumed = inflater.total_in() as usize;
        let status = inflater
            .decompress_vec(&input[consumed..], &mut decoded, FlushDecompress::None)
            .map_err(|e| RpcError::invalid_params(format!("zlib decode failed: {e}")))?;
        match status {
            Status::StreamEnd if decoded.len() <= limit => return Ok(decoded),
            Status::StreamEnd => {}
            Status::Ok | Status::BufError => {
                // Output space remained, yet nothing moved: the input ended
                // before the stream did.
                if (inflater.total_in(), inflater.total_out()) == before
                    && decoded.len() < decoded.capacity()
                {
                    return Err(RpcError::invalid_params(
                        "zlib decode failed: incomplete stream",
                    ));
                }
            }
        }
    }
}

/// Re-apply FILE's own owner after a write, as TRAMP's `write-region` does
/// through `tramp-set-file-uid-gid`.  Ownership does not change, but watchers
/// observe the attribute change that file-notify clients, including Emacs's
/// filenotify tests, expect from `write-region` on a remote file.  An
/// unchanged owner of -1 would not notify, so the actual ids are passed.
/// Setuid and setgid files are skipped because chown clears those bits.
/// Failures are ignored: the write itself has succeeded.
fn reassert_ownership(file: &std::fs::File) {
    let Ok(stat) = rustix::fs::fstat(file) else {
        return;
    };
    if rustix::fs::Mode::from_raw_mode(stat.st_mode)
        .intersects(rustix::fs::Mode::SUID | rustix::fs::Mode::SGID)
    {
        return;
    }
    let _ = rustix::fs::fchown(
        file,
        Some(rustix::fs::Uid::from_raw(stat.st_uid)),
        Some(rustix::fs::Gid::from_raw(stat.st_gid)),
    );
}

/// Write file contents
pub async fn write(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// Content to write as binary
        #[serde(with = "serde_bytes")]
        content: Vec<u8>,
        /// File mode (permissions) applied after writing, including existing files
        #[serde(default)]
        mode: Option<u32>,
        /// Append to file instead of overwriting
        #[serde(default)]
        append: bool,
        /// Byte offset to start writing at (only if not appending)
        #[serde(default)]
        offset: Option<u64>,
        /// Fail with EEXIST if the destination already exists (`O_CREAT | O_EXCL`).
        #[serde(default)]
        mustbenew: bool,
        /// When true, `content` is zlib-compressed and decompressed before writing.
        #[serde(default)]
        compress: bool,
        /// When true, return post-write `fstat` attributes as `"stat"`.
        #[serde(default)]
        return_stat: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    super::run_blocking("File write", move || {
        let path = params.path.resolve_sync();
        let path_str = path.to_string_lossy().into_owned();

        let content = if params.compress {
            inflate_complete_zlib(&params.content, crate::MAX_FRAME_SIZE)?
        } else {
            params.content
        };

        let mut options = std::fs::OpenOptions::new();
        if params.mustbenew {
            options.write(true).create_new(true);
            if params.append {
                options.append(true);
            }
        } else if params.append {
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
            .map_err(|e| map_io_error(e, &path_str))?;

        if let Some(offset) = params.offset {
            file.seek(SeekFrom::Start(offset))
                .map_err(|e| map_io_error(e, &path_str))?;
        }

        file.write_all(&content)
            .map_err(|e| map_io_error(e, &path_str))?;
        file.flush().map_err(|e| map_io_error(e, &path_str))?;
        reassert_ownership(&file);

        // Preserve the existing file.write contract for external clients.  The
        // Emacs client does not pass MODE; callers that do request it explicitly
        // receive a post-write chmod.
        if let Some(mode) = params.mode {
            let perms = std::fs::Permissions::from_mode(mode);
            file.set_permissions(perms)
                .map_err(|e| map_io_error(e, &path_str))?;
        }

        let mut fields = vec![(Value::String("written".into()), Value::from(content.len()))];
        if params.return_stat
            && let Ok(metadata) = file.metadata()
        {
            fields.push((
                Value::String("stat".into()),
                file_attributes_from_metadata(&metadata, None).into_value(),
            ));
        }

        Ok(Value::Map(fields))
    })
    .await
}

/// Copy a file or directory.
pub async fn copy(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        src: RpcPath,
        dest: RpcPath,
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

    super::run_blocking("File copy", move || {
        let src_path = params.src.resolve_sync();
        let mut dest_path = params.dest.resolve_sync();

        // If destination is a directory, append the source filename
        if !params.exact_dest
            && std::fs::metadata(&dest_path)
                .map(|meta| meta.is_dir())
                .unwrap_or(false)
            && let Some(filename) = src_path.file_name()
        {
            dest_path.push(filename);
        }

        let src_str = src_path.to_string_lossy().into_owned();
        let src_metadata = std::fs::metadata(&src_path).map_err(|e| map_io_error(e, &src_str))?;

        let bytes_copied = if src_metadata.is_dir() {
            reject_recursive_self_copy_sync(&src_path, &dest_path)
                .map_err(|e| map_io_error(e, &src_str))?;
            copy_dir_recursive_sync(&src_path, &dest_path, options, true)
                .map_err(|e| map_io_error(e, &src_str))?
        } else {
            // Follow a source symlink, but refuse nonregular files and create
            // no-overwrite destinations exclusively.
            copy_regular_file_sync(&src_path, &dest_path, options)
                .map_err(|e| map_io_error(e, &src_str))?
        };

        Ok(msgpack_map! {
            "copied" => bytes_copied
        })
    })
    .await
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

/// Recursively copy a directory and its contents synchronously within a single
/// blocking worker task.
fn copy_dir_recursive_sync(
    src: &Path,
    dest: &Path,
    options: CopyOptions,
    allow_existing_dest_dir: bool,
) -> std::io::Result<u64> {
    ensure_directory_destination_sync(dest, allow_existing_dest_dir)?;

    let mut total: u64 = 0;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let entry_path = entry.path();
        let dest_child = dest.join(entry.file_name());
        let file_type = entry.file_type()?;

        if file_type.is_dir() {
            total += copy_dir_recursive_sync(
                &entry_path,
                &dest_child,
                options,
                options.merge_existing_directories,
            )?;
        } else if file_type.is_symlink() {
            let link_target = std::fs::read_link(&entry_path)?;
            prepare_symlink_destination_sync(&dest_child, options.overwrite)?;
            std::os::unix::fs::symlink(&link_target, &dest_child)?;
        } else {
            total += copy_regular_file_sync(&entry_path, &dest_child, options)?;
        }
    }

    let src_meta = std::fs::metadata(src)?;
    apply_copied_metadata_sync(&src_meta, dest, options)?;

    Ok(total)
}

fn ensure_directory_destination_sync(path: &Path, allow_existing: bool) -> std::io::Result<()> {
    match std::fs::symlink_metadata(path) {
        Ok(_) => {
            if allow_existing
                && std::fs::metadata(path)
                    .map(|meta| meta.is_dir())
                    .unwrap_or(false)
            {
                Ok(())
            } else {
                Err(already_exists(path))
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => std::fs::create_dir_all(path),
        Err(e) => Err(e),
    }
}

fn reject_recursive_self_copy_sync(src: &Path, dest: &Path) -> std::io::Result<()> {
    let src = std::fs::canonicalize(src)?;
    let dest = canonicalize_existing_ancestor_sync(dest)?;

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

fn canonicalize_existing_ancestor_sync(path: &Path) -> std::io::Result<PathBuf> {
    let mut ancestor = path.to_path_buf();
    let mut suffix = PathBuf::new();

    loop {
        match std::fs::symlink_metadata(&ancestor) {
            Ok(_) => return Ok(std::fs::canonicalize(&ancestor)?.join(suffix)),
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

/// Copy regular files using the same descriptor-owned path for both policies.
/// No-overwrite creates exclusively; overwrite follows an existing symlink,
/// but refuses devices, FIFOs and aliases of the source before truncating.
/// Errors leave partial output: unlinking DEST could delete a concurrent
/// replacement.  Neither data writes nor metadata updates reopen a pathname.
fn copy_regular_file_sync(src: &Path, dest: &Path, options: CopyOptions) -> std::io::Result<u64> {
    use std::os::unix::fs::OpenOptionsExt;

    // Nonblocking open prevents a FIFO with no writer pinning this worker.
    let mut source = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NONBLOCK | libc::O_NOCTTY)
        .open(src)?;
    let metadata = source.metadata()?;
    if !metadata.is_file() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("Not a regular file: {}", src.display()),
        ));
    }
    let mut target = open_copy_destination(dest, &metadata, options.overwrite)?;
    // Apply access permissions before any private source bytes are visible.
    target.set_permissions(std::fs::Permissions::from_mode(
        metadata.permissions().mode() & 0o777,
    ))?;
    let copied = std::io::copy(&mut source, &mut target)?;
    // Writes can clear setuid/setgid bits; restore the full mode afterward.
    apply_copied_metadata_fd(&metadata, &target, options)?;
    Ok(copied)
}

fn open_copy_destination(
    dest: &Path,
    source: &std::fs::Metadata,
    overwrite: bool,
) -> std::io::Result<std::fs::File> {
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt};

    match std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .mode(0o600)
        .open(dest)
    {
        Ok(file) => Ok(file),
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists && overwrite => {
            // Follow existing symlinks, but do not create a dangling link's
            // target.  Open without truncation until fstat verifies ownership.
            let file = std::fs::OpenOptions::new()
                .write(true)
                .custom_flags(libc::O_NONBLOCK | libc::O_NOCTTY)
                .open(dest)?;
            let metadata = file.metadata()?;
            if !metadata.is_file() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Not a regular file: {}", dest.display()),
                ));
            }
            if (source.dev(), source.ino()) == (metadata.dev(), metadata.ino()) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Input and output files are the same",
                ));
            }
            file.set_len(0)?;
            Ok(file)
        }
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
            Err(already_exists(dest))
        }
        Err(error) => Err(error),
    }
}

fn prepare_symlink_destination_sync(path: &Path, overwrite: bool) -> std::io::Result<()> {
    match std::fs::symlink_metadata(path) {
        Ok(meta) => {
            if !overwrite {
                return Err(already_exists(path));
            }
            if meta.is_dir() && !meta.file_type().is_symlink() {
                return Err(already_exists(path));
            }
            remove_path_for_overwrite_sync(path)
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

fn apply_copied_metadata_sync(
    src_meta: &std::fs::Metadata,
    dest: &Path,
    options: CopyOptions,
) -> std::io::Result<()> {
    if options.preserve_permissions {
        std::fs::set_permissions(dest, src_meta.permissions())?;
    }

    if options.preserve_times {
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;

            let atime = src_meta.atime();
            let atime_nsec = src_meta.atime_nsec();
            let mtime = src_meta.mtime();
            let mtime_nsec = src_meta.mtime_nsec();
            set_file_times_sync_path_io(dest, atime, atime_nsec, mtime, mtime_nsec, false)?;
        }
    }

    Ok(())
}

/// Apply copied permissions and times through an open handle.
///
/// Resolving DEST again for `set_permissions'/`utimensat' would let a caller
/// that can write the destination directory swap in a symlink and have the
/// source-controlled metadata applied to its target.  `fchmod'/`futimens' on
/// the handle created by the copy cannot be redirected.
fn apply_copied_metadata_fd(
    src_meta: &std::fs::Metadata,
    file: &std::fs::File,
    options: CopyOptions,
) -> std::io::Result<()> {
    // `fs::copy' always copies the permission bits; mirror that.
    let permissions = src_meta.permissions();
    let (atime, atime_nsec, mtime, mtime_nsec) = {
        use std::os::unix::fs::MetadataExt;
        (
            src_meta.atime(),
            src_meta.atime_nsec(),
            src_meta.mtime(),
            src_meta.mtime_nsec(),
        )
    };
    file.set_permissions(permissions)?;
    if options.preserve_times {
        use rustix::fs::{Timespec, Timestamps};
        let times = Timestamps {
            last_access: Timespec {
                tv_sec: atime,
                tv_nsec: atime_nsec,
            },
            last_modification: Timespec {
                tv_sec: mtime,
                tv_nsec: mtime_nsec,
            },
        };
        rustix::fs::futimens(file, &times).map_err(std::io::Error::from)?;
    }
    Ok(())
}

fn remove_path_for_overwrite_sync(path: &Path) -> std::io::Result<()> {
    let meta = std::fs::symlink_metadata(path)?;
    if meta.is_dir() && !meta.file_type().is_symlink() {
        std::fs::remove_dir_all(path)
    } else {
        std::fs::remove_file(path)
    }
}

/// No-clobber fallback for filesystems without an atomic rename primitive.
///
/// Link non-directories exclusively before removing the source.  If neither
/// rename flags nor hard links are available, fail rather than letting a
/// check-then-rename race overwrite a concurrently created destination.
/// If source removal fails, retain the destination and report the error:
/// rollback could delete the last link or a concurrently replaced destination.
async fn rename_no_overwrite_fallback(src: &Path, dest: &Path) -> std::io::Result<()> {
    rename_no_overwrite_fallback_with_remove(src, dest, fs::remove_file(src)).await
}

async fn rename_no_overwrite_fallback_with_remove(
    src: &Path,
    dest: &Path,
    remove_source: impl std::future::Future<Output = std::io::Result<()>>,
) -> std::io::Result<()> {
    if !fs::symlink_metadata(src).await.map(|meta| meta.is_dir())? {
        match fs::hard_link(src, dest).await {
            Ok(()) => return remove_source.await,
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                return Err(already_exists(dest));
            }
            Err(error)
                if matches!(
                    error.raw_os_error(),
                    Some(libc::EPERM) | Some(libc::EOPNOTSUPP) | Some(libc::ENOSYS)
                ) => {}
            Err(error) => return Err(error),
        }
    }
    match fs::symlink_metadata(dest).await {
        Ok(_) => Err(already_exists(dest)),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "Filesystem does not support atomic non-overwriting rename for this source",
        )),
        Err(error) => Err(error),
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
async fn rename_no_overwrite(src: &Path, dest: &Path) -> std::io::Result<()> {
    use rustix::fs::{CWD, RenameFlags};

    let owned_src = src.to_owned();
    let owned_dest = dest.to_owned();
    let result = tokio::task::spawn_blocking(move || {
        rustix::fs::renameat_with(CWD, &owned_src, CWD, &owned_dest, RenameFlags::NOREPLACE)
    })
    .await
    .map_err(std::io::Error::other)?;

    // Map rustix's Errno into a std io error so both match arms share one type.
    let result = result.map_err(std::io::Error::from);

    match result {
        // rustix maps NOREPLACE to Linux RENAME_NOREPLACE or macOS RENAME_EXCL.
        // Older kernels, unsupported filesystems, or seccomp filters can
        // reject the primitive; try the no-clobber hard-link fallback.
        Err(error)
            if matches!(
                error.raw_os_error(),
                Some(libc::EINVAL)
                    | Some(libc::ENOSYS)
                    | Some(libc::EOPNOTSUPP)
                    | Some(libc::EPERM)
            ) =>
        {
            rename_no_overwrite_fallback(src, dest).await
        }
        result => result,
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
async fn rename_no_overwrite(src: &Path, dest: &Path) -> std::io::Result<()> {
    rename_no_overwrite_fallback(src, dest).await
}

/// Rename/move a file
pub async fn rename(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        src: RpcPath,
        dest: RpcPath,
        /// Overwrite destination if it exists
        #[serde(default)]
        overwrite: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = params.src.resolve().await?;
    let dest = params.dest.resolve().await?;
    let dest_str = dest.to_string_lossy().into_owned();
    let src_str = src.to_string_lossy().into_owned();

    let result = if params.overwrite {
        fs::rename(&src, &dest).await
    } else {
        rename_no_overwrite(&src, &dest).await
    };
    result.map_err(|e| {
        if e.kind() == std::io::ErrorKind::AlreadyExists {
            map_io_error(already_exists(&dest), &dest_str)
        } else {
            // Keep the reported name a real path: it lands in the FILENAME
            // slot of the Lisp `file-error'/`file-missing' signal, where
            // callers may feed it back to `expand-file-name'.
            map_io_error(e, &src_str)
        }
    })?;

    Ok(Value::Boolean(true))
}

/// Delete a file
pub async fn delete(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// If true, don't error if file doesn't exist
        #[serde(default)]
        force: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
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
        path: RpcPath,
        mode: u32,
        /// If true, change a symlink itself instead of following it.  Required
        /// so a client that forgets it cannot silently modify a link target.
        nofollow: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();
    let mode = params.mode;
    let nofollow = params.nofollow;

    super::run_blocking("Set file modes", move || {
        set_file_modes_sync(&path, mode, nofollow).map_err(|e| map_io_error(e, &path_str))
    })
    .await?;

    Ok(Value::Boolean(true))
}

/// Set file timestamps
pub async fn set_times(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// Modification time (seconds since epoch)
        mtime: i64,
        /// Modification time nanoseconds (0..1_000_000_000, default 0)
        #[serde(default)]
        mtime_nsec: i64,
        /// Access time (seconds since epoch, defaults to mtime)
        #[serde(default)]
        atime: Option<i64>,
        /// Access time nanoseconds (defaults to mtime_nsec when atime is omitted, else 0)
        #[serde(default)]
        atime_nsec: Option<i64>,
        /// If true, update a symlink itself instead of following it.
        #[serde(default)]
        nofollow: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();
    let mtime = params.mtime;
    let mtime_nsec = params.mtime_nsec;
    let (atime, atime_nsec) = match params.atime {
        Some(atime) => (atime, params.atime_nsec.unwrap_or(0)),
        None => (mtime, params.atime_nsec.unwrap_or(mtime_nsec)),
    };
    // `utimensat' reads UTIME_NOW and UTIME_OMIT from the nanosecond field;
    // out-of-range values must not reach it as control values.
    for (name, nsec) in [("mtime_nsec", mtime_nsec), ("atime_nsec", atime_nsec)] {
        if !(0..1_000_000_000).contains(&nsec) {
            return Err(RpcError::invalid_params(format!(
                "{name} must be in 0..1000000000, got {nsec}"
            )));
        }
    }
    let nofollow = params.nofollow;

    super::run_blocking("Set file times", move || {
        set_file_times_sync_path_io(&path, atime, atime_nsec, mtime, mtime_nsec, nofollow)
            .map_err(|e| map_io_error(e, &path_str))
    })
    .await?;

    Ok(Value::Boolean(true))
}

/// Create a symbolic link
pub async fn make_symlink(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        target: RpcPath,
        link_path: RpcPath,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let target = params.target.resolve().await?;
    let link_path = params.link_path.resolve().await?;
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
        src: RpcPath,
        /// The new hard link path
        dest: RpcPath,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let src = params.src.resolve().await?;
    let dest = params.dest.resolve().await?;
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
        path: RpcPath,
        /// New user ID (-1 to leave unchanged)
        uid: i32,
        /// New group ID (-1 to leave unchanged)
        gid: i32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
    let uid = chown_id(params.uid, "uid")?;
    let gid = chown_id(params.gid, "gid")?;

    super::run_blocking("File chown", move || {
        use rustix::process::{Gid, Uid};

        let owner = uid.map(Uid::from_raw);
        let group = gid.map(Gid::from_raw);
        rustix::fs::chown(&path, owner, group).map_err(|error| RpcError::io_error(error.into()))
    })
    .await?;

    Ok(Value::Boolean(true))
}

// ============================================================================
// Helper functions
// ============================================================================

fn chown_id(id: i32, name: &str) -> Result<Option<u32>, RpcError> {
    match id {
        -1 => Ok(None),
        0.. => Ok(Some(id as u32)),
        _ => Err(RpcError::invalid_params(format!(
            "{name} must be -1 or non-negative"
        ))),
    }
}

/// Change the permission bits of PATH.
///
/// With NOFOLLOW, delegate to libc's `fchmodat(AT_SYMLINK_NOFOLLOW)`: it changes
/// regular files without following a final symlink, and fails with EOPNOTSUPP
/// for symlinks on Linux, where they have no mode bits.  This matches local
/// Emacs.  rustix's raw-syscall `chmodat` rejects NOFOLLOW for every file on
/// Linux, so it cannot be used here.
#[cfg(unix)]
fn set_file_modes_sync(path: &Path, mode: u32, nofollow: bool) -> std::io::Result<()> {
    use nix::sys::stat::{FchmodatFlags, Mode, fchmodat};

    let flag = if nofollow {
        FchmodatFlags::NoFollowSymlink
    } else {
        FchmodatFlags::FollowSymlink
    };
    let mode = mode & 0o7777;
    let result = fchmodat(
        nix::fcntl::AT_FDCWD,
        path,
        Mode::from_bits_truncate(mode as libc::mode_t),
        flag,
    );
    match result {
        // glibc before 2.32 rejects NOFOLLOW for every file.
        #[cfg(target_os = "linux")]
        Err(nix::errno::Errno::EOPNOTSUPP) if nofollow => chmod_nofollow_via_proc(path, mode),
        result => result.map_err(std::io::Error::from),
    }
}

/// Change PATH's mode without following a final symlink, as newer libcs do.
///
/// The descriptor pins the file that was checked, so replacing PATH with a
/// symlink afterwards cannot redirect the chmod to another file.
#[cfg(target_os = "linux")]
fn chmod_nofollow_via_proc(path: &Path, mode: u32) -> std::io::Result<()> {
    use rustix::fs::{FileType, Mode, OFlags};
    use std::os::fd::AsRawFd;

    let fd = rustix::fs::open(
        path,
        OFlags::PATH | OFlags::NOFOLLOW | OFlags::CLOEXEC,
        Mode::empty(),
    )?;
    if FileType::from_raw_mode(rustix::fs::fstat(&fd)?.st_mode) == FileType::Symlink {
        return Err(std::io::Error::from_raw_os_error(libc::EOPNOTSUPP));
    }
    let proc_path = format!("/proc/self/fd/{}", fd.as_raw_fd());
    rustix::fs::chmod(proc_path.as_str(), Mode::from_raw_mode(mode as _)).map_err(|error| {
        // Without /proc there is no race-free way to apply the mode.
        if error == rustix::io::Errno::NOENT {
            std::io::Error::from_raw_os_error(libc::EOPNOTSUPP)
        } else {
            error.into()
        }
    })
}

#[cfg(unix)]
fn set_file_times_sync_path_io(
    path: &Path,
    atime: i64,
    atime_nsec: i64,
    mtime: i64,
    mtime_nsec: i64,
    nofollow: bool,
) -> std::io::Result<()> {
    use rustix::fs::{AtFlags, CWD, Timespec, Timestamps};

    let times = Timestamps {
        last_access: Timespec {
            tv_sec: atime,
            tv_nsec: atime_nsec,
        },
        last_modification: Timespec {
            tv_sec: mtime,
            tv_nsec: mtime_nsec,
        },
    };
    let flags = if nofollow {
        AtFlags::SYMLINK_NOFOLLOW
    } else {
        AtFlags::empty()
    };
    rustix::fs::utimensat(CWD, path, &times, flags).map_err(std::io::Error::from)
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

    #[test]
    fn chown_id_accepts_only_minus_one_or_non_negative_values() {
        assert_eq!(chown_id(-1, "uid").unwrap(), None);
        assert_eq!(chown_id(0, "uid").unwrap(), Some(0));
        assert_eq!(chown_id(i32::MAX, "gid").unwrap(), Some(i32::MAX as u32));
        assert_eq!(
            chown_id(-2, "uid").unwrap_err().code,
            RpcError::INVALID_PARAMS
        );
    }

    async fn set_modes_at(path: &Path, mode: u32, nofollow: bool) -> HandlerResult {
        set_modes(msgpack_map! {
            "path" => path_value(path),
            "mode" => mode,
            "nofollow" => nofollow,
        })
        .await
    }

    #[tokio::test]
    async fn set_modes_nofollow_changes_regular_files() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"").await.unwrap();

        set_modes_at(&path, 0o600, true)
            .await
            .expect("nofollow chmod of a regular file");

        assert_eq!(fs::metadata(path).await.unwrap().mode() & 0o7777, 0o600);
    }

    #[tokio::test]
    async fn set_modes_nofollow_leaves_symlink_target_alone() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let target = tmp.path().join("target");
        let link = tmp.path().join("link");
        fs::write(&target, b"").await.unwrap();
        fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644))
            .await
            .unwrap();
        fs::symlink(&target, &link).await.unwrap();

        // Linux cannot change a symlink's mode and reports an error; BSD and
        // macOS change the link itself.  Either way the target must not change.
        let _ = set_modes_at(&link, 0o600, true).await;

        assert_eq!(fs::metadata(target).await.unwrap().mode() & 0o7777, 0o644);
    }

    #[tokio::test]
    async fn set_modes_follows_symlinks_by_request() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let target = tmp.path().join("target");
        let link = tmp.path().join("link");
        fs::write(&target, b"").await.unwrap();
        fs::symlink(&target, &link).await.unwrap();

        set_modes_at(&link, 0o600, false)
            .await
            .expect("chmod through symlink");

        assert_eq!(fs::metadata(target).await.unwrap().mode() & 0o7777, 0o600);
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn chmod_nofollow_via_proc_changes_files_but_not_link_targets() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let target = tmp.path().join("target");
        let link = tmp.path().join("link");
        std::fs::write(&target, b"").unwrap();
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644)).unwrap();
        std::os::unix::fs::symlink(&target, &link).unwrap();

        let error = chmod_nofollow_via_proc(&link, 0o600).unwrap_err();
        assert_eq!(error.raw_os_error(), Some(libc::EOPNOTSUPP));
        assert_eq!(std::fs::metadata(&target).unwrap().mode() & 0o7777, 0o644);

        chmod_nofollow_via_proc(&target, 0o600).expect("chmod regular file");
        assert_eq!(std::fs::metadata(&target).unwrap().mode() & 0o7777, 0o600);
    }

    #[tokio::test]
    async fn set_modes_requires_nofollow() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"").await.unwrap();

        let error = set_modes(msgpack_map! {
            "path" => path_value(&path),
            "mode" => 0o600u32,
        })
        .await
        .unwrap_err();

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
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
    async fn write_mode_updates_existing_file_permissions() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("existing");
        fs::write(&path, b"old").await.unwrap();
        fs::set_permissions(&path, std::fs::Permissions::from_mode(0o640))
            .await
            .unwrap();

        write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(b"new".to_vec()),
            "mode" => 0o600u32,
        })
        .await
        .expect("overwrite existing file with explicit mode");

        assert_eq!(fs::metadata(path).await.unwrap().mode() & 0o777, 0o600);
    }

    #[tokio::test]
    async fn rename_no_overwrite_fallback_checks_destination_and_renames() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"source").await.unwrap();
        tokio::fs::symlink("missing-target", &dest).await.unwrap();

        let error = rename_no_overwrite_fallback(&src, &dest)
            .await
            .expect_err("dangling symlink is an existing destination");
        assert_eq!(error.kind(), std::io::ErrorKind::AlreadyExists);
        assert!(fs::symlink_metadata(&dest).await.is_ok());
        assert!(fs::metadata(&src).await.is_ok());

        fs::remove_file(&dest).await.unwrap();
        rename_no_overwrite_fallback(&src, &dest)
            .await
            .expect("missing destination is renamed");
        assert_eq!(fs::read(&dest).await.unwrap(), b"source");
        assert!(fs::metadata(&src).await.is_err());
    }

    #[tokio::test]
    async fn rename_fallback_preserves_destination_when_source_removal_fails() {
        // Root can unlink in a read-only directory, so it cannot exercise this.
        if rustix::process::geteuid().is_root() {
            return;
        }
        let tmp = tempfile::tempdir().expect("create tempdir");
        let source_dir = tmp.path().join("source-dir");
        fs::create_dir(&source_dir).await.unwrap();
        let src = source_dir.join("src");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"source").await.unwrap();
        fs::set_permissions(&source_dir, std::fs::Permissions::from_mode(0o555))
            .await
            .unwrap();
        let result = rename_no_overwrite_fallback(&src, &dest).await;
        fs::set_permissions(&source_dir, std::fs::Permissions::from_mode(0o755))
            .await
            .unwrap();

        assert_eq!(
            result.unwrap_err().kind(),
            std::io::ErrorKind::PermissionDenied
        );
        assert_eq!(fs::read(&src).await.unwrap(), b"source");
        assert_eq!(fs::read(&dest).await.unwrap(), b"source");
    }

    #[tokio::test]
    async fn rename_fallback_preserves_last_link_after_concurrent_source_removal() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"source").await.unwrap();

        let error = rename_no_overwrite_fallback_with_remove(&src, &dest, async {
            // The fallback already linked the destination.  Model another
            // caller removing the source before our own unlink is attempted.
            assert_eq!(
                fs::metadata(&src).await?.ino(),
                fs::metadata(&dest).await?.ino()
            );
            fs::remove_file(&src).await?;
            fs::remove_file(&src).await
        })
        .await
        .expect_err("the source disappeared before our unlink");

        assert_eq!(error.kind(), std::io::ErrorKind::NotFound);
        assert!(!fs::try_exists(&src).await.unwrap());
        assert_eq!(fs::read(&dest).await.unwrap(), b"source");
    }

    #[tokio::test]
    async fn rename_fallback_does_not_remove_concurrently_replaced_destination() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"source").await.unwrap();

        let error = rename_no_overwrite_fallback_with_remove(&src, &dest, async {
            // Replace the just-created link before source removal fails.
            fs::remove_file(&dest).await?;
            fs::write(&dest, b"replacement").await?;
            Err(std::io::Error::from_raw_os_error(libc::EACCES))
        })
        .await
        .expect_err("source removal failed");

        assert_eq!(error.raw_os_error(), Some(libc::EACCES));
        assert_eq!(fs::read(&src).await.unwrap(), b"source");
        assert_eq!(fs::read(&dest).await.unwrap(), b"replacement");
    }

    #[tokio::test]
    async fn rename_directory_without_overwrite() {
        // The fallback must fail closed when it cannot link a directory.
        for fallback in [false, true] {
            let tmp = tempfile::tempdir().expect("create tempdir");
            let src = tmp.path().join("src");
            let dest = tmp.path().join("dest");
            fs::create_dir(&src).await.unwrap();
            fs::write(src.join("child"), b"source").await.unwrap();
            tokio::fs::symlink("missing-target", &dest).await.unwrap();

            let result = if fallback {
                rename_no_overwrite_fallback(&src, &dest).await
            } else {
                rename_no_overwrite(&src, &dest).await
            };
            assert_eq!(
                result.unwrap_err().kind(),
                std::io::ErrorKind::AlreadyExists
            );
            assert!(
                fs::symlink_metadata(&dest)
                    .await
                    .unwrap()
                    .file_type()
                    .is_symlink()
            );
            assert_eq!(fs::read(src.join("child")).await.unwrap(), b"source");

            fs::remove_file(&dest).await.unwrap();
            if fallback {
                let error = rename_no_overwrite_fallback(&src, &dest)
                    .await
                    .expect_err("no atomic directory fallback is available");
                assert_eq!(error.kind(), std::io::ErrorKind::Unsupported);
                assert_eq!(fs::read(src.join("child")).await.unwrap(), b"source");
                assert!(!fs::try_exists(&dest).await.unwrap());
            } else {
                rename_no_overwrite(&src, &dest)
                    .await
                    .expect("rename directory with native no-replace support");
                assert_eq!(fs::read(dest.join("child")).await.unwrap(), b"source");
                assert!(!fs::try_exists(&src).await.unwrap());
            }
        }
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
    async fn read_reports_total_file_size_with_partial_chunk() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"0123456789").await.unwrap();

        let result = read(msgpack_map! {
            "path" => path_value(&path),
            "offset" => 2u64,
            "length" => 4u64,
        })
        .await
        .expect("read partial chunk");

        let field = |name: &str| {
            result
                .as_map()
                .unwrap()
                .iter()
                .find(|(key, _)| key.as_str() == Some(name))
                .map(|(_, value)| value.clone())
        };
        assert_eq!(field("content"), Some(Value::Binary(b"2345".to_vec())));
        assert_eq!(field("size").and_then(|v| v.as_u64()), Some(4));
        assert_eq!(field("file_size").and_then(|v| v.as_u64()), Some(10));
    }

    #[tokio::test]
    async fn read_size_limit_rejects_oversized_request() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"x").await.unwrap();

        let error = read(msgpack_map! {
            "path" => path_value(&path),
            "length" => (MAX_FILE_READ_CHUNK_BYTES + 1) as u64,
        })
        .await
        .expect_err("oversized read must fail before allocating");

        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[test]
    fn default_read_rejects_file_that_grows_after_metadata() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        std::fs::write(&path, b"x").unwrap();
        let mut file = std::fs::File::open(&path).unwrap();
        let expected_len = file.metadata().unwrap().len();
        std::fs::write(&path, vec![b'x'; MAX_FILE_READ_CHUNK_BYTES + 1]).unwrap();

        let error = read_default_bounded_sync(&mut file, expected_len, &path.to_string_lossy())
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
    async fn copy_overwrite_follows_destination_symlink() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src.txt");
        let target = tmp.path().join("target.txt");
        let dest = tmp.path().join("dest-link");
        fs::write(&src, b"new").await.unwrap();
        fs::write(&target, b"old").await.unwrap();
        tokio::fs::symlink(&target, &dest).await.unwrap();

        // Emacs `copy-file' with overwrite follows an existing destination
        // symlink and writes through it; the atomic no-overwrite path must not
        // change that when both files are regular.
        copy(msgpack_map! {
            "src" => path_value(&src),
            "dest" => path_value(&dest),
            "overwrite" => true,
        })
        .await
        .expect("overwrite copy should succeed");

        assert!(
            fs::symlink_metadata(&dest)
                .await
                .unwrap()
                .file_type()
                .is_symlink(),
            "the destination symlink must be preserved"
        );
        assert_eq!(fs::read(&target).await.unwrap(), b"new");
    }

    #[test]
    fn atomic_copy_never_truncates_a_rejected_destination() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let src = tmp.path().join("src.txt");
        let dest = tmp.path().join("dest.txt");
        std::fs::write(&src, b"new").unwrap();
        std::fs::write(&dest, b"old").unwrap();

        let options = CopyOptions {
            preserve_permissions: false,
            preserve_times: false,
            overwrite: false,
            merge_existing_directories: false,
        };
        let error = copy_regular_file_sync(&src, &dest, options)
            .expect_err("an existing destination must be refused");
        assert_eq!(error.kind(), std::io::ErrorKind::AlreadyExists);
        // The refusal must not have truncated the destination first.
        assert_eq!(std::fs::read(&dest).unwrap(), b"old");

        let options = CopyOptions {
            overwrite: true,
            ..options
        };
        assert_eq!(copy_regular_file_sync(&src, &dest, options).unwrap(), 3);
        assert_eq!(std::fs::read(&dest).unwrap(), b"new");

        assert_eq!(std::fs::read_dir(tmp.path()).unwrap().count(), 2);
    }

    #[test]
    fn copy_destination_is_private_before_writing() {
        let tmp = tempfile::tempdir().unwrap();
        let src = tmp.path().join("source");
        std::fs::write(&src, b"secret").unwrap();
        std::fs::set_permissions(&src, std::fs::Permissions::from_mode(0o600)).unwrap();
        for overwrite in [false, true] {
            let dest = tmp.path().join(format!("dest-{overwrite}"));
            let file =
                open_copy_destination(&dest, &std::fs::metadata(&src).unwrap(), overwrite).unwrap();
            assert_eq!(file.metadata().unwrap().mode() & 0o077, 0);
            assert_eq!(file.metadata().unwrap().len(), 0);
        }
    }

    #[tokio::test]
    async fn copy_rejects_nonregular_files_without_blocking() {
        use nix::sys::stat::Mode;
        let tmp = tempfile::tempdir().unwrap();
        let fifo = tmp.path().join("fifo");
        nix::unistd::mkfifo(&fifo, Mode::S_IRUSR | Mode::S_IWUSR).unwrap();
        for source in [fifo.as_path(), Path::new("/dev/null")] {
            for overwrite in [false, true] {
                let dest = tmp.path().join("dest");
                let result = tokio::time::timeout(
                    std::time::Duration::from_secs(2),
                    copy(msgpack_map! {
                        "src" => path_value(source), "dest" => path_value(&dest),
                        "overwrite" => overwrite,
                    }),
                )
                .await
                .expect("nonregular source must not block");
                assert!(result.is_err());
                assert!(!dest.exists());
            }
        }
        let src = tmp.path().join("source");
        fs::write(&src, b"payload").await.unwrap();
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            copy(msgpack_map! {
                "src" => path_value(&src), "dest" => path_value(&fifo), "overwrite" => true,
            }),
        )
        .await
        .expect("FIFO destination must not block");
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn copy_overwrite_rejects_source_aliases_without_truncating() {
        let tmp = tempfile::tempdir().unwrap();
        let src = tmp.path().join("source");
        let hardlink = tmp.path().join("hardlink");
        let symlink = tmp.path().join("symlink");
        fs::write(&src, b"keep me").await.unwrap();
        fs::hard_link(&src, &hardlink).await.unwrap();
        tokio::fs::symlink(&src, &symlink).await.unwrap();
        for dest in [&src, &hardlink, &symlink] {
            let error = copy(msgpack_map! {
                "src" => path_value(&src), "dest" => path_value(dest), "overwrite" => true,
            })
            .await
            .expect_err("source aliases must be refused before truncation");
            assert!(error.message.contains("same"));
            assert_eq!(fs::read(&src).await.unwrap(), b"keep me");
        }
    }

    #[tokio::test]
    async fn copy_overwrite_preserves_private_permissions_and_refuses_dangling_link() {
        let tmp = tempfile::tempdir().unwrap();
        let src = tmp.path().join("source");
        let dest = tmp.path().join("dest");
        fs::write(&src, b"secret").await.unwrap();
        fs::set_permissions(&src, std::fs::Permissions::from_mode(0o600))
            .await
            .unwrap();
        fs::write(&dest, b"public").await.unwrap();
        fs::set_permissions(&dest, std::fs::Permissions::from_mode(0o644))
            .await
            .unwrap();
        copy(msgpack_map! {
            "src" => path_value(&src), "dest" => path_value(&dest), "overwrite" => true,
        })
        .await
        .unwrap();
        assert_eq!(fs::metadata(&dest).await.unwrap().mode() & 0o777, 0o600);
        assert_eq!(fs::read(&dest).await.unwrap(), b"secret");

        let link = tmp.path().join("link");
        let missing = tmp.path().join("missing");
        tokio::fs::symlink(&missing, &link).await.unwrap();
        assert!(
            copy(msgpack_map! {
                "src" => path_value(&src), "dest" => path_value(&link), "overwrite" => true,
            })
            .await
            .is_err()
        );
        assert!(!missing.exists());
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn failed_copy_retains_partial_destination() {
        let tmp = tempfile::tempdir().unwrap();
        let dest = tmp.path().join("dest");
        // /proc/self/mem is regular, but reading the unmapped address zero
        // fails with EIO.  Exercise real copy failure without an injection hook.
        assert!(
            copy(msgpack_map! {
                "src" => "/proc/self/mem", "dest" => path_value(&dest),
            })
            .await
            .is_err()
        );
        assert!(dest.exists(), "copy failure must not unlink by pathname");
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

    #[tokio::test]
    async fn read_include_meta_and_skip_incompressible() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("tiny");
        fs::write(&path, b"hi").await.unwrap();

        let result = read(msgpack_map! {
            "path" => path_value(&path),
            "compress" => true,
            "include_meta" => true,
        })
        .await
        .expect("read with meta and compress");

        let map = result.as_map().unwrap();
        let field = |name: &str| {
            map.iter()
                .find(|(k, _)| k.as_str() == Some(name))
                .map(|(_, v)| v)
        };
        // "hi" (2 bytes) expands under zlib, so server must send it uncompressed.
        assert_eq!(field("compressed").and_then(Value::as_bool), Some(false));
        assert_eq!(field("content").and_then(Value::as_slice), Some(&b"hi"[..]));
        assert_eq!(field("is_symlink").and_then(Value::as_bool), Some(false));
        assert_eq!(field("writable").and_then(Value::as_bool), Some(true));
        let stat = field("stat").and_then(Value::as_map).expect("stat map");
        let size = stat
            .iter()
            .find(|(k, _)| k.as_str() == Some("size"))
            .and_then(|(_, v)| v.as_u64());
        assert_eq!(size, Some(2));
    }

    #[test]
    fn inflate_complete_zlib_enforces_decoded_size_limit() {
        let payload = b"0123456789".repeat(100);
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::fast());
        encoder.write_all(&payload).unwrap();
        let compressed = encoder.finish().unwrap();

        assert_eq!(
            inflate_complete_zlib(&compressed, payload.len()).unwrap(),
            payload
        );
        let error = inflate_complete_zlib(&compressed, payload.len() - 1).unwrap_err();
        assert!(error.message.contains("exceeds maximum frame size"));
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn write_reports_attribute_change_without_changing_owner() {
        use rustix::fs::inotify;
        use std::os::unix::fs::MetadataExt;

        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"before").await.unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o640)).unwrap();
        let before = std::fs::metadata(&path).unwrap();
        let watch = inotify::init(inotify::CreateFlags::NONBLOCK | inotify::CreateFlags::CLOEXEC)
            .expect("inotify");
        inotify::add_watch(&watch, &path, inotify::WatchFlags::ATTRIB).expect("watch file");

        write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(b"after".to_vec()),
        })
        .await
        .expect("overwrite");

        let mut buf = [std::mem::MaybeUninit::<u8>::uninit(); 1024];
        let mut reader = inotify::Reader::new(&watch, &mut buf);
        let event = reader.next().expect("write raises an inotify event");
        assert!(event.events().contains(inotify::ReadFlags::ATTRIB));
        let after = std::fs::metadata(&path).unwrap();
        assert_eq!((after.uid(), after.gid()), (before.uid(), before.gid()));
        assert_eq!(after.permissions().mode() & 0o7777, 0o640);
        assert_eq!(fs::read(&path).await.unwrap(), b"after");
    }

    #[tokio::test]
    async fn write_rejects_incomplete_zlib_stream_without_truncating() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("file");
        fs::write(&path, b"original").await.unwrap();

        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::fast());
        encoder.write_all(&b"replacement ".repeat(50)).unwrap();
        let complete = encoder.finish().unwrap();
        for content in [Vec::new(), complete[..complete.len() / 2].to_vec()] {
            let error = write(msgpack_map! {
                "path" => path_value(&path),
                "content" => Value::Binary(content),
                "compress" => true,
            })
            .await
            .expect_err("an incomplete zlib stream must be rejected");
            assert_eq!(error.code, RpcError::INVALID_PARAMS);
            assert_eq!(fs::read(&path).await.unwrap(), b"original");
        }

        // A complete stream that decodes to zero bytes still truncates.
        let empty = ZlibEncoder::new(Vec::new(), Compression::fast())
            .finish()
            .unwrap();
        write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(empty),
            "compress" => true,
        })
        .await
        .expect("write an empty decoded payload");
        assert_eq!(fs::read(&path).await.unwrap(), b"");
    }

    #[tokio::test]
    async fn write_mustbenew_compress_and_return_stat() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("newfile");

        let payload = b"hello world ".repeat(100);
        let mut encoder = ZlibEncoder::new(Vec::new(), Compression::fast());
        encoder.write_all(&payload).unwrap();
        let compressed = encoder.finish().unwrap();

        let res = write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(compressed.clone()),
            "mustbenew" => true,
            "compress" => true,
            "return_stat" => true,
        })
        .await
        .expect("mustbenew compressed write");

        assert_eq!(fs::read(&path).await.unwrap(), payload);
        let map = res.as_map().unwrap();
        let written = map
            .iter()
            .find(|(k, _)| k.as_str() == Some("written"))
            .and_then(|(_, v)| v.as_u64());
        assert_eq!(written, Some(payload.len() as u64));
        let stat = map
            .iter()
            .find(|(k, _)| k.as_str() == Some("stat"))
            .and_then(|(_, v)| v.as_map())
            .expect("return_stat map");
        let size = stat
            .iter()
            .find(|(k, _)| k.as_str() == Some("size"))
            .and_then(|(_, v)| v.as_u64());
        assert_eq!(size, Some(payload.len() as u64));

        // Second mustbenew write must fail with EEXIST / already_exists.
        let err = write(msgpack_map! {
            "path" => path_value(&path),
            "content" => Value::Binary(b"other".to_vec()),
            "mustbenew" => true,
        })
        .await
        .expect_err("mustbenew must fail on existing file");
        let kind = err
            .data
            .as_ref()
            .and_then(Value::as_map)
            .and_then(|m| m.iter().find(|(k, _)| k.as_str() == Some("error_kind")))
            .and_then(|(_, v)| v.as_str());
        assert_eq!(kind, Some("already_exists"));
        assert_eq!(fs::read(&path).await.unwrap(), payload);
    }

    #[tokio::test]
    async fn set_times_preserves_subsecond_nanoseconds() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("ts");
        fs::write(&path, b"x").await.unwrap();

        set_times(msgpack_map! {
            "path" => path_value(&path),
            "mtime" => 1_700_000_000i64,
            "mtime_nsec" => 123_456_789i64,
        })
        .await
        .expect("set_times with nsec");

        let meta = fs::metadata(&path).await.unwrap();
        assert_eq!(meta.mtime(), 1_700_000_000);
        // Allow coarse filesystem timer granularity (some FS truncate to ms/us),
        // but on Linux tmpfs/ext4 nanosecond precision is exact.
        assert!(meta.mtime_nsec() > 0);
    }

    #[tokio::test]
    async fn set_times_rejects_out_of_range_nanoseconds() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("ts");
        fs::write(&path, b"x").await.unwrap();
        set_times(msgpack_map! {"path" => path_value(&path), "mtime" => 1_000i64})
            .await
            .expect("set a known time");

        // 1073741823 is UTIME_NOW on Linux.
        for (key, nsec) in [
            ("mtime_nsec", 1_073_741_823i64),
            ("atime_nsec", 1_000_000_000i64),
            ("mtime_nsec", -1i64),
        ] {
            let error = set_times(msgpack_map! {
                "path" => path_value(&path),
                "mtime" => 1_700_000_000i64,
                key => nsec,
            })
            .await
            .expect_err("out-of-range nanoseconds must be rejected");
            assert_eq!(error.code, RpcError::INVALID_PARAMS);
        }
        assert_eq!(fs::metadata(&path).await.unwrap().mtime(), 1_000);
    }

    #[tokio::test]
    async fn read_zero_length_directory_fails_with_eisdir() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let err = read(msgpack_map! {
            "path" => path_value(tmp.path()),
            "offset" => 0u64,
            "length" => 0u64,
        })
        .await
        .expect_err("zero-length read of a directory must fail");
        let errno = err
            .data
            .as_ref()
            .and_then(Value::as_map)
            .and_then(|m| m.iter().find(|(k, _)| k.as_str() == Some("os_errno")))
            .and_then(|(_, v)| v.as_i64());
        assert_eq!(errno, Some(i64::from(libc::EISDIR)));
    }
}
