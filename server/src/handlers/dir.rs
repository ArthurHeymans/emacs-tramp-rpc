// SPDX-License-Identifier: GPL-3.0-or-later

//! Directory operations
//!
//! Optimized to use:
//! - Single directory fd via `rustix::fs::Dir::read_from` (no double open)
//! - `d_type` from readdir to get file type without extra syscalls
//! - `fstatat` with directory fd for efficient attribute collection
//! - Single-pass batch NSS resolution for distinct uid/gids
//! - Synchronous blocking task to avoid per-entry async overhead

use crate::protocol::{DirEntry, FileAttributes, FileType, RpcError, RpcPath, from_value};
use rmpv::Value;
use rustix::fs::{AtFlags, Dir, FileType as RustixFileType, Mode, OFlags, Stat};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::os::fd::{AsFd, BorrowedFd};
use std::os::unix::fs::{DirBuilderExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;

use super::HandlerResult;
use super::file::map_io_error;

/// Upper bound on one `dir.list` payload.
const MAX_DIR_LISTING_BYTES: usize = 64 * 1024 * 1024;
const DIR_ENTRY_BUDGET_BYTES: usize = 256;
const DIR_ENTRY_BUDGET_BYTES_NO_ATTRS: usize = 32;

#[inline]
fn stat_time_to_i64<T: TryInto<i64>>(time: T) -> i64 {
    time.try_into().unwrap_or(0)
}

#[inline]
fn extract_stat_fields(stat_buf: &Stat) -> (i64, i64, i64, i64, i64, i64, u32) {
    #[cfg(any(target_os = "macos", target_os = "freebsd"))]
    let mode = stat_buf.st_mode as u32;
    #[cfg(not(any(target_os = "macos", target_os = "freebsd")))]
    let mode = stat_buf.st_mode;

    (
        stat_time_to_i64(stat_buf.st_atime),
        stat_time_to_i64(stat_buf.st_atime_nsec),
        stat_time_to_i64(stat_buf.st_mtime),
        stat_time_to_i64(stat_buf.st_mtime_nsec),
        stat_time_to_i64(stat_buf.st_ctime),
        stat_time_to_i64(stat_buf.st_ctime_nsec),
        mode,
    )
}

#[inline]
fn file_type_from_rustix(ft: RustixFileType) -> FileType {
    match ft {
        RustixFileType::RegularFile => FileType::File,
        RustixFileType::Directory => FileType::Directory,
        RustixFileType::Symlink => FileType::Symlink,
        RustixFileType::CharacterDevice => FileType::CharDevice,
        RustixFileType::BlockDevice => FileType::BlockDevice,
        RustixFileType::Fifo => FileType::Fifo,
        RustixFileType::Socket => FileType::Socket,
        RustixFileType::Unknown => FileType::Unknown,
    }
}

/// Get `FileAttributes` using `fstatat` relative to `dir_fd`, leaving `uname`/`gname`
/// empty so the caller can batch-resolve each distinct uid/gid once per listing.
fn get_file_attributes_at_raw(
    dir_fd: BorrowedFd<'_>,
    name: &[u8],
    follow_symlinks: bool,
) -> Option<FileAttributes> {
    let flags = if follow_symlinks {
        AtFlags::empty()
    } else {
        AtFlags::SYMLINK_NOFOLLOW
    };

    let stat_buf = rustix::fs::statat(dir_fd, name, flags).ok()?;
    let file_type = file_type_from_rustix(RustixFileType::from_raw_mode(stat_buf.st_mode as _));

    let link_target = if file_type == FileType::Symlink {
        if name == b"." || name == b".." {
            None
        } else {
            rustix::fs::readlinkat(dir_fd, name, Vec::new())
                .ok()
                .map(std::ffi::CString::into_bytes)
        }
    } else {
        None
    };

    let (atime, atime_nsec, mtime, mtime_nsec, ctime, ctime_nsec, mode) =
        extract_stat_fields(&stat_buf);

    Some(FileAttributes {
        file_type,
        nlinks: stat_buf.st_nlink as u64,
        uid: stat_buf.st_uid,
        gid: stat_buf.st_gid,
        uname: None,
        gname: None,
        atime,
        atime_nsec,
        mtime,
        mtime_nsec,
        ctime,
        ctime_nsec,
        size: stat_buf.st_size as u64,
        mode,
        inode: stat_buf.st_ino as u64,
        dev: stat_buf.st_dev as u64,
        link_target,
    })
}

/// List directory contents using a single directory fd with `d_type` and `fstatat`.
pub async fn list(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
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

    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();
    let include_attrs = params.include_attrs;
    let include_hidden = params.include_hidden;

    let results = super::run_blocking("Dir list", move || {
        list_dir_sync(&path, include_attrs, include_hidden, MAX_DIR_LISTING_BYTES)
            .map_err(|e| map_io_error(e, &path_str))
    })
    .await?;

    let values: Vec<Value> = results.into_iter().map(DirEntry::into_value).collect();
    Ok(Value::Array(values))
}

/// Synchronous directory listing using one directory fd for both `readdir` and `fstatat`.
fn list_dir_sync(
    path: &Path,
    include_attrs: bool,
    include_hidden: bool,
    budget_bytes: usize,
) -> Result<Vec<DirEntry>, std::io::Error> {
    let dir_handle = rustix::fs::open(
        path,
        OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .map_err(std::io::Error::from)?;
    let dir_fd = dir_handle.as_fd();
    // `Dir::read_from` reopens "." relative to the fd, which needs search
    // permission; a readable 0400 directory must still list.  A duplicate
    // shares the open file description and needs no permission check.
    let stream_fd = rustix::io::fcntl_dupfd_cloexec(dir_fd, 0).map_err(std::io::Error::from)?;

    let mut results: Vec<DirEntry> = Vec::new();
    let mut budget_used: usize = 0;
    let mut saw_dot = false;
    let mut saw_dotdot = false;

    let dir = Dir::new(stream_fd).map_err(std::io::Error::from)?;
    for entry_result in dir {
        let entry = entry_result.map_err(std::io::Error::from)?;
        let name_slice = entry.file_name().to_bytes();

        let is_dot = name_slice == b".";
        let is_dotdot = name_slice == b"..";
        if is_dot {
            saw_dot = true;
        } else if is_dotdot {
            saw_dotdot = true;
        }

        let is_hidden = name_slice.first() == Some(&b'.');
        if !include_hidden && is_hidden {
            continue;
        }

        let mut file_type = if is_dot || is_dotdot {
            FileType::Directory
        } else {
            file_type_from_rustix(entry.file_type())
        };

        // Filesystems like XFS v4 / FUSE may report DT_UNKNOWN.
        if file_type == FileType::Unknown
            && let Ok(stat_buf) = rustix::fs::statat(dir_fd, name_slice, AtFlags::SYMLINK_NOFOLLOW)
        {
            file_type = file_type_from_rustix(RustixFileType::from_raw_mode(stat_buf.st_mode as _));
        }

        // For completion paths (include_attrs=false), treat symlinks to
        // directories as directories.
        if !include_attrs
            && file_type == FileType::Symlink
            && let Ok(stat_buf) = rustix::fs::statat(dir_fd, name_slice, AtFlags::empty())
            && RustixFileType::from_raw_mode(stat_buf.st_mode as _) == RustixFileType::Directory
        {
            file_type = FileType::Directory;
        }

        let attrs = if include_attrs {
            // Follow symlinks for "." and ".." (matching prior behavior), lstat others.
            get_file_attributes_at_raw(dir_fd, name_slice, is_dot || is_dotdot)
        } else {
            None
        };

        let per_entry_bytes = if include_attrs {
            DIR_ENTRY_BUDGET_BYTES
        } else {
            DIR_ENTRY_BUDGET_BYTES_NO_ATTRS
        };
        budget_used += per_entry_bytes
            + name_slice.len()
            + attrs
                .as_ref()
                .and_then(|attrs| attrs.link_target.as_ref())
                .map_or(0, Vec::len);
        if budget_used > budget_bytes {
            return Err(std::io::Error::other(format!(
                "Directory listing for {} exceeds {budget_bytes} bytes",
                path.display()
            )));
        }

        results.push(DirEntry {
            name: name_slice.to_vec(),
            file_type,
            attrs,
        });
    }

    // Synthetic filesystems (e.g. some FUSE mounts) may omit "." or "..".
    if include_hidden {
        for (present, name) in [(saw_dot, b".".as_slice()), (saw_dotdot, b"..".as_slice())] {
            if !present {
                let attrs = if include_attrs {
                    get_file_attributes_at_raw(dir_fd, name, true)
                } else {
                    None
                };
                results.push(DirEntry {
                    name: name.to_vec(),
                    file_type: FileType::Directory,
                    attrs,
                });
            }
        }
    }

    // Batch-resolve NSS names once per distinct uid/gid across all entries
    // (including "." and "..").
    if include_attrs {
        let mut uids = HashSet::new();
        let mut gids = HashSet::new();
        for entry in &results {
            if let Some(attrs) = entry.attrs.as_ref() {
                uids.insert(attrs.uid);
                gids.insert(attrs.gid);
            }
        }
        let unames: HashMap<u32, Option<Arc<str>>> = uids
            .into_iter()
            .map(|uid| (uid, super::file::get_user_name(uid)))
            .collect();
        let gnames: HashMap<u32, Option<Arc<str>>> = gids
            .into_iter()
            .map(|gid| (gid, super::file::get_group_name(gid)))
            .collect();
        for entry in &mut results {
            if let Some(attrs) = entry.attrs.as_mut() {
                attrs.uname = unames
                    .get(&attrs.uid)
                    .and_then(|opt| opt.as_deref())
                    .map(str::to_owned);
                attrs.gname = gnames
                    .get(&attrs.gid)
                    .and_then(|opt| opt.as_deref())
                    .map(str::to_owned);
            }
        }
    }

    results.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(results)
}

/// Return the directory components that do not exist yet, from top to bottom.
fn missing_directory_chain(path: &Path) -> Vec<PathBuf> {
    let mut missing = Vec::new();
    let mut current = Some(path);

    while let Some(component) = current {
        if component.exists() {
            break;
        }
        missing.push(component.to_path_buf());
        current = component.parent();
    }

    missing.reverse();
    missing
}

/// Create a directory in a single blocking task using `DirBuilder::mode`.
pub async fn create(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
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
    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();
    let parents = params.parents;
    let mode = params.mode;

    let created = super::run_blocking("Dir create", move || {
        let created_paths = if parents {
            missing_directory_chain(&path)
        } else {
            vec![path.clone()]
        };

        // Owner access during construction lets a restrictive MODE such as
        // 0555 still create missing descendants.
        let mut builder = std::fs::DirBuilder::new();
        builder.recursive(parents).mode(mode | 0o700);
        builder
            .create(&path)
            .map_err(|e| map_io_error(e, &path_str))?;

        // Apply exact mode (unmasked by umask) only to directories created by
        // this request, avoiding chmod side effects on pre-existing parents.
        // Deepest first, so a MODE without search permission cannot block
        // the chmod of a descendant.
        let perms = std::fs::Permissions::from_mode(mode);
        for created_path in created_paths.iter().rev() {
            std::fs::set_permissions(created_path, perms.clone())
                .map_err(|e| map_io_error(e, &created_path.to_string_lossy()))?;
        }

        Ok(!created_paths.is_empty())
    })
    .await?;

    Ok(Value::Boolean(created))
}

/// Remove a directory
pub async fn remove(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// Remove recursively
        #[serde(default)]
        recursive: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();

    let result = if params.recursive {
        fs::remove_dir_all(&path).await
    } else {
        fs::remove_dir(&path).await
    };

    result.map_err(|e| map_io_error(e, &path_str))?;

    Ok(Value::Boolean(true))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn listing_over_budget_is_rejected_instead_of_allocated() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        for index in 0..8 {
            std::fs::write(tmp.path().join(format!("entry-{index}")), b"x").unwrap();
        }

        let error = list_dir_sync(tmp.path(), false, true, 64)
            .expect_err("a tiny budget must stop the listing");
        assert!(
            error.to_string().contains("exceeds 64 bytes"),
            "unexpected error: {error}"
        );

        let listed = list_dir_sync(tmp.path(), false, true, 64 * 1024)
            .expect("a normal budget lists the directory");
        assert!(listed.len() >= 8);
    }

    #[test]
    fn listing_a_readable_directory_does_not_need_search_permission() {
        if rustix::process::geteuid().is_root() {
            return; // root bypasses directory permission checks
        }
        let tmp = tempfile::tempdir().expect("create tempdir");
        let dir = tmp.path().join("read-only");
        std::fs::create_dir(&dir).unwrap();
        std::fs::write(dir.join("entry"), b"x").unwrap();
        std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o400)).unwrap();

        let listed = list_dir_sync(&dir, false, false, 64 * 1024);
        std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o700)).unwrap();

        let names: Vec<Vec<u8>> = listed
            .expect("list a readable directory")
            .into_iter()
            .map(|entry| entry.name)
            .collect();
        assert_eq!(names, vec![b"entry".to_vec()]);
    }

    async fn create_with_parents(path: &Path, mode: u32) -> HandlerResult {
        create(Value::Map(vec![
            (
                Value::String("path".into()),
                Value::String(path.to_string_lossy().into_owned().into()),
            ),
            (Value::String("parents".into()), Value::Boolean(true)),
            (Value::String("mode".into()), Value::Integer(mode.into())),
        ]))
        .await
    }

    #[tokio::test]
    async fn restrictive_mode_applies_after_missing_descendants_are_created() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        for (name, mode) in [("read-exec", 0o555), ("read-only", 0o444)] {
            let top = tmp.path().join(name);
            let leaf = top.join("child");
            let result = create_with_parents(&leaf, mode).await;
            let top_mode = std::fs::metadata(&top).map(|m| m.permissions().mode() & 0o7777);
            // Restore access before asserting so the tempdir can be removed.
            let _ = std::fs::set_permissions(&top, std::fs::Permissions::from_mode(0o700));
            let leaf_mode = std::fs::metadata(&leaf).map(|m| m.permissions().mode() & 0o7777);

            assert_eq!(
                result.expect("create nested directories"),
                Value::Boolean(true)
            );
            assert_eq!(top_mode.expect("top directory exists"), mode);
            assert_eq!(leaf_mode.expect("leaf directory exists"), mode);
            std::fs::set_permissions(&leaf, std::fs::Permissions::from_mode(0o700)).unwrap();
        }
    }
}
