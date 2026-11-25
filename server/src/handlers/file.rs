// SPDX-License-Identifier: GPL-3.0-or-later

//! File metadata operations

use crate::protocol::{FileAttributes, FileType, RpcError, RpcPath, from_value};
use rmpv::Value;
use serde::Deserialize;
use std::collections::{HashMap, VecDeque};
use std::ffi::{OsStr, OsString};
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::{Component, Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, LazyLock, Mutex, RwLock};
use std::time::{Duration, Instant};

use super::HandlerResult;

/// Get file attributes
pub async fn stat(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// If true, don't follow symlinks
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = params.path.resolve().await?;
    match get_file_attributes(path, params.lstat).await {
        Ok(attrs) => Ok(attrs.into_value()),
        Err(e) if e.code == RpcError::FILE_NOT_FOUND => Ok(Value::Nil),
        Err(e) => Err(e),
    }
}

/// Return an effective-credential access decision, following symlinks.
/// Filesystem denials are results; unsupported checks and RPC failures are errors.
pub async fn access(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
        /// Any combination of r/w/x; empty checks existence only.
        mode: String,
        /// For write-only checks, retry ENOENT against the immediate parent.
        #[serde(default)]
        check_parent_if_missing: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let mode = params
        .mode
        .bytes()
        .try_fold(rustix::fs::Access::empty(), |mode, bit| {
            Ok::<_, RpcError>(
                mode | match bit {
                    b'r' => rustix::fs::Access::READ_OK,
                    b'w' => rustix::fs::Access::WRITE_OK,
                    b'x' => rustix::fs::Access::EXEC_OK,
                    _ => {
                        return Err(RpcError::invalid_params(
                            "Access mode must contain only r/w/x",
                        ));
                    }
                },
            )
        })?;
    if params.check_parent_if_missing && mode != rustix::fs::Access::WRITE_OK {
        return Err(RpcError::invalid_params(
            "Parent fallback requires write-only access mode",
        ));
    }
    let path = params.path.resolve().await?;
    super::run_blocking("Access", move || {
        let result = crate::access::check(&path, mode);
        let result = match result {
            Err(crate::access::Error::Filesystem(rustix::io::Errno::NOENT))
                if params.check_parent_if_missing =>
            {
                match access_parent(&path) {
                    Some(parent) => crate::access::check(
                        parent,
                        rustix::fs::Access::WRITE_OK | rustix::fs::Access::EXEC_OK,
                    ),
                    None => result,
                }
            }
            _ => result,
        };
        let (errno, message) = match result {
            Ok(()) => (0, None),
            Err(crate::access::Error::Unsupported(error)) => {
                // Keep the errno for diagnostics, but an EPERM here means the
                // check could not run, not that access was denied.
                return Err(RpcError::io_error(std::io::Error::from_raw_os_error(
                    error.raw_os_error(),
                ))
                .with_error_kind("unsupported"));
            }
            Err(crate::access::Error::Filesystem(error)) => {
                (error.raw_os_error(), Some(error.to_string()))
            }
        };
        let mut result = vec![(Value::from("errno"), Value::from(errno))];
        if let Some(message) = message {
            result.push((Value::from("message"), Value::from(message)));
        }
        Ok(Value::Map(result))
    })
    .await
}

/// Keep trailing separators and dot components, like Emacs file-name-directory.
/// Path::parent would strip them and incorrectly allow missing directory paths.
fn access_parent(path: &Path) -> Option<&Path> {
    let bytes = path.as_os_str().as_bytes();
    match bytes.iter().rposition(|&byte| byte == b'/') {
        Some(separator) => Some(Path::new(OsStr::from_bytes(&bytes[..=separator]))),
        None if !bytes.is_empty() => Some(Path::new(".")),
        None => None,
    }
}

/// Maximum number of symlink hops before reporting ELOOP, matching Emacs's
/// `file-truename` counter limit.
const MAX_SYMLINK_HOPS: usize = 100;

/// Resolve symlinks in `path` (including symlinked parent directories of
/// non-existent files and dangling symlinks), matching Emacs `file-truename`.
pub(crate) fn resolve_truename_sync(path: &Path) -> std::io::Result<PathBuf> {
    // Fast path for existing paths: kernel/libc canonicalize resolves everything
    // in a single pass.  Its ELOOP limit (40 links with glibc) is lower than
    // MAX_SYMLINK_HOPS, so the walker below decides whether a chain loops.
    if let Ok(canonical) = std::fs::canonicalize(path) {
        return Ok(canonical);
    }

    let mut current = if path.is_absolute() {
        PathBuf::from("/")
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"))
    };

    let mut pending: VecDeque<OsString> = VecDeque::new();
    for component in path.components() {
        match component {
            Component::RootDir | Component::CurDir => {}
            Component::ParentDir => pending.push_back(OsString::from("..")),
            Component::Normal(part) => pending.push_back(part.to_os_string()),
            Component::Prefix(_) => {}
        }
    }

    let mut hops: usize = 0;
    let mut missing = false;

    while let Some(part) = pending.pop_front() {
        if part == "." {
            continue;
        }
        if part == ".." {
            current.pop();
            // Popping back out of a non-existent suffix may land on an
            // existing directory again.
            missing = false;
            continue;
        }

        let candidate = current.join(&part);
        if missing {
            current = candidate;
            continue;
        }

        match std::fs::symlink_metadata(&candidate) {
            Ok(meta) if meta.file_type().is_symlink() => {
                hops += 1;
                if hops > MAX_SYMLINK_HOPS {
                    return Err(std::io::Error::from_raw_os_error(libc::ELOOP));
                }
                let target = std::fs::read_link(&candidate)?;
                if target.is_absolute() {
                    current = PathBuf::from("/");
                }
                // Prepend target components in order before remaining pending parts.
                let mut target_parts = Vec::new();
                for component in target.components() {
                    match component {
                        Component::RootDir | Component::CurDir => {}
                        Component::ParentDir => target_parts.push(OsString::from("..")),
                        Component::Normal(name) => target_parts.push(name.to_os_string()),
                        Component::Prefix(_) => {}
                    }
                }
                for target_part in target_parts.into_iter().rev() {
                    pending.push_front(target_part);
                }
            }
            Ok(_) => {
                current = candidate;
            }
            Err(err) if err.raw_os_error() == Some(libc::ELOOP) => {
                return Err(err);
            }
            Err(_) => {
                // Missing component, ENOTDIR, or EACCES: stop chasing symlinks
                // below this point and append remaining components verbatim,
                // matching Emacs's `file-truename`.
                current = candidate;
                missing = true;
            }
        }
    }

    Ok(current)
}

/// Get the true name of a file (resolve symlinks, including symlinked ancestors
/// of non-existent files and dangling symlinks).
pub async fn truename(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        path: RpcPath,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let path = params.path.resolve().await?;
    let path_str = path.to_string_lossy().into_owned();

    let canonical = super::run_blocking("Truename", move || {
        resolve_truename_sync(&path).map_err(|e| map_io_error(e, &path_str))
    })
    .await?;

    let bytes = canonical.as_os_str().as_bytes();
    Ok(Value::Binary(bytes.to_vec()))
}

// ============================================================================
// Helper functions
// ============================================================================

/// Build `FileAttributes` synchronously from an already-fetched `Metadata`
/// (e.g. from `fstat` on an open file descriptor) plus optional symlink target.
pub(crate) fn file_attributes_from_metadata(
    metadata: &std::fs::Metadata,
    link_target: Option<Vec<u8>>,
) -> FileAttributes {
    let file_type = file_type_from_metadata_ft(&metadata.file_type());
    let uid = metadata.uid();
    let gid = metadata.gid();
    let uname = get_user_name(uid).map(|s| s.to_string());
    let gname = get_group_name(gid).map(|s| s.to_string());

    FileAttributes {
        file_type,
        nlinks: metadata.nlink(),
        uid,
        gid,
        uname,
        gname,
        atime: metadata.atime(),
        atime_nsec: metadata.atime_nsec(),
        mtime: metadata.mtime(),
        mtime_nsec: metadata.mtime_nsec(),
        ctime: metadata.ctime(),
        ctime_nsec: metadata.ctime_nsec(),
        size: metadata.len(),
        mode: metadata.mode(),
        inode: metadata.ino(),
        dev: metadata.dev(),
        link_target,
    }
}

/// Collect file attributes in a single `spawn_blocking` call (metadata +
/// readlink + NSS resolution), avoiding multiple threadpool hops.
pub async fn get_file_attributes(path: PathBuf, lstat: bool) -> Result<FileAttributes, RpcError> {
    super::run_blocking("Stat", move || {
        let metadata = if lstat {
            std::fs::symlink_metadata(&path)
        } else {
            std::fs::metadata(&path)
        }
        .map_err(|e| map_io_error(e, &path.to_string_lossy()))?;

        let file_type = file_type_from_metadata_ft(&metadata.file_type());
        let link_target = if file_type == FileType::Symlink {
            std::fs::read_link(&path)
                .ok()
                .map(|p| p.as_os_str().as_bytes().to_vec())
        } else {
            None
        };

        Ok(file_attributes_from_metadata(&metadata, link_target))
    })
    .await
}

pub(crate) fn file_type_from_metadata_ft(ft: &std::fs::FileType) -> FileType {
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

/// Selects whether to resolve a user (uid) or group (gid) name.
#[derive(Clone, Copy)]
enum NssKind {
    User,
    Group,
}

/// Maximum entries retained in each NSS cache before eviction.
const MAX_NSS_CACHE_ENTRIES: usize = 4096;

/// Cache of uid -> resolved name. A cached `None` records a *definitive*
/// miss (the uid is absent from all NSS databases). Transient failures
/// (timeout, backend unavailable) are not cached so a later lookup can
/// succeed once the directory service recovers.
static USER_NAMES: LazyLock<RwLock<HashMap<u32, Option<Arc<str>>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Cache of gid -> resolved name. Same caching semantics as `USER_NAMES`.
static GROUP_NAMES: LazyLock<RwLock<HashMap<u32, Option<Arc<str>>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Short negative cache for transient NSS failures (LDAP/SSSD outage).
/// Without this, every directory entry with the same uid/gid retries libc +
/// `getent` (~2s each), turning one listing into a per-entry retry storm.
/// Entries expire after `TRANSIENT_NSS_TTL` so recovery is still prompt.
const TRANSIENT_NSS_TTL: Duration = Duration::from_secs(10);
static TRANSIENT_USER_FAILURES: LazyLock<Mutex<HashMap<u32, Instant>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));
static TRANSIENT_GROUP_FAILURES: LazyLock<Mutex<HashMap<u32, Instant>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

fn transient_failure_recent(cache: &Mutex<HashMap<u32, Instant>>, id: u32) -> bool {
    let mut cache = cache.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(&at) = cache.get(&id) {
        if at.elapsed() < TRANSIENT_NSS_TTL {
            return true;
        }
        cache.remove(&id);
    }
    false
}

fn record_transient_failure(cache: &Mutex<HashMap<u32, Instant>>, id: u32) {
    let mut cache = cache.lock().unwrap_or_else(|e| e.into_inner());
    cache.insert(id, Instant::now());
    // Bound growth: outages can touch many distinct ids; drop expired first,
    // then cap at a generous size to avoid unbounded memory.
    if cache.len() > 4096 {
        cache.retain(|_, at| at.elapsed() < TRANSIENT_NSS_TTL);
    }
    if cache.len() > 8192 {
        cache.clear();
    }
}

/// Maximum wall-clock time to wait for a `getent` fallback lookup before
/// giving up. NSS backends (LDAP, SSSD) can hang indefinitely when the
/// directory service is unreachable; without a bound this would stall
/// attribute generation for every unresolved id.
const GETENT_TIMEOUT: Duration = Duration::from_secs(2);

/// Poll interval while waiting for the child to exit.
const GETENT_POLL_INTERVAL: Duration = Duration::from_millis(10);

/// Maximum time to wait for the reader thread to finish after killing the child.
/// Bounds `reader.join()` in case an orphaned process inherited the pipe write-end.
const GETENT_READER_TIMEOUT: Duration = Duration::from_millis(500);

/// Maximum stdout bytes buffered from `getent` (we only need the name field
/// before the first ':'; any remaining member list is drained without storing).
const GETENT_MAX_CAPTURE_BYTES: usize = 4096;

/// Join the reader thread with a bounded timeout.
fn bounded_join_reader(reader: std::thread::JoinHandle<Vec<u8>>) {
    let deadline = Instant::now() + GETENT_READER_TIMEOUT;
    loop {
        if reader.is_finished() {
            let _ = reader.join();
            return;
        }
        if Instant::now() >= deadline {
            return; // leak thread; unblocks once all pipe write-ends close
        }
        std::thread::sleep(GETENT_POLL_INTERVAL);
    }
}

/// Resolve a uid/gid to a name via the `getent` command, used only as a
/// fallback when the reentrant libc lookup fails.
///
/// Buffers at most `GETENT_MAX_CAPTURE_BYTES` while draining any remaining
/// stdout so large LDAP group lines never cause unbounded memory allocation
/// or spurious SIGPIPE failures.
fn getent_name(database: &str, id: u32) -> Result<Option<String>, ()> {
    let mut child = Command::new("getent")
        .arg(database)
        .arg(id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|_| ())?;

    let mut stdout = child.stdout.take().ok_or(())?;
    let reader = std::thread::spawn(move || {
        let mut buf = Vec::new();
        let _ = (&mut stdout)
            .take(GETENT_MAX_CAPTURE_BYTES as u64)
            .read_to_end(&mut buf);
        // Drain any remaining bytes into a fixed stack buffer so `getent`
        // cannot block on the pipe or fail with SIGPIPE on huge group lines.
        let mut discard = [0u8; 4096];
        while let Ok(n) = stdout.read(&mut discard) {
            if n == 0 {
                break;
            }
        }
        buf
    });

    let deadline = Instant::now() + GETENT_TIMEOUT;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if Instant::now() >= deadline {
                    let _ = child.kill();
                    let _ = child.wait();
                    bounded_join_reader(reader);
                    return Err(());
                }
                std::thread::sleep(GETENT_POLL_INTERVAL);
            }
            Err(_) => {
                let _ = child.kill();
                let _ = child.wait();
                bounded_join_reader(reader);
                return Err(());
            }
        }
    };

    let buf = reader.join().map_err(|_| ())?;

    if !status.success() {
        return Ok(None); // definitive: id does not exist in any NSS database
    }

    parse_getent_name(&buf)
}

/// Extract the name field from captured `getent` output.  The capture may end
/// inside a multibyte character of a long member list, so only the bytes
/// before the first ':' are decoded.  A capture without a complete name
/// field is an error.
fn parse_getent_name(buf: &[u8]) -> Result<Option<String>, ()> {
    let end = buf.iter().position(|&byte| byte == b':').ok_or(())?;
    let name = &buf[..end];
    if name.contains(&b'\n') {
        return Err(());
    }
    let name = std::str::from_utf8(name).map_err(|_| ())?;
    if name.is_empty() {
        Ok(None)
    } else {
        Ok(Some(name.to_string()))
    }
}

/// Reentrant passwd/group lookup by id through nix, which retries with a
/// growing buffer on `ERANGE` so large LDAP/SSSD records still resolve.
fn nss_name_by_id(kind: NssKind, id: u32) -> nix::Result<Option<String>> {
    match kind {
        NssKind::User => nix::unistd::User::from_uid(nix::unistd::Uid::from_raw(id))
            .map(|user| user.map(|user| user.name)),
        NssKind::Group => nix::unistd::Group::from_gid(nix::unistd::Gid::from_raw(id))
            .map(|group| group.map(|group| group.name)),
    }
}

/// Shared NSS name resolution for both uid and gid using a concurrent read-heavy
/// `RwLock` cache returning `Arc<str>`.
fn resolve_nss_name(
    cache: &'static RwLock<HashMap<u32, Option<Arc<str>>>>,
    kind: NssKind,
    id: u32,
) -> Option<Arc<str>> {
    // Fast path: shared read lock.
    {
        let c = cache.read().unwrap_or_else(|e| e.into_inner());
        if let Some(result) = c.get(&id) {
            return result.clone();
        }
    }
    let transient_cache = match kind {
        NssKind::User => &*TRANSIENT_USER_FAILURES,
        NssKind::Group => &*TRANSIENT_GROUP_FAILURES,
    };
    if transient_failure_recent(transient_cache, id) {
        return None;
    }

    let database = match kind {
        NssKind::User => "passwd",
        NssKind::Group => "group",
    };

    let name_result: Result<Option<String>, ()> = match nss_name_by_id(kind, id) {
        Ok(Some(name)) => Ok(Some(name)),
        Ok(None) | Err(_) => getent_name(database, id),
    };

    match name_result {
        Ok(name) => {
            let shared: Option<Arc<str>> = name.map(Arc::from);
            let mut c = cache.write().unwrap_or_else(|e| e.into_inner());
            if c.len() >= MAX_NSS_CACHE_ENTRIES {
                c.clear();
            }
            c.insert(id, shared.clone());
            shared
        }
        Err(()) => {
            record_transient_failure(transient_cache, id);
            None
        }
    }
}

pub fn get_user_name(uid: u32) -> Option<Arc<str>> {
    resolve_nss_name(&USER_NAMES, NssKind::User, uid)
}

pub fn get_group_name(gid: u32) -> Option<Arc<str>> {
    resolve_nss_name(&GROUP_NAMES, NssKind::Group, gid)
}

/// Resolve the login shell for `uid` via getpwuid_r.
pub(crate) fn get_user_login_shell(uid: u32) -> Option<String> {
    nix::unistd::User::from_uid(nix::unistd::Uid::from_raw(uid))
        .ok()
        .flatten()
        .and_then(|user| user.shell.into_os_string().into_string().ok())
}

/// Cache of user name -> resolved home directory.
/// Only successful lookups are cached.
static USER_HOMES: LazyLock<RwLock<HashMap<String, Arc<[u8]>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Resolve the home directory for a user name via getpwnam_r.
pub(crate) fn get_user_home_dir(user: &str) -> Option<Vec<u8>> {
    {
        let cache = USER_HOMES.read().unwrap_or_else(|e| e.into_inner());
        if let Some(home) = cache.get(user) {
            return Some(home.to_vec());
        }
    }

    let resolved = getpwnam_home_dir_uncached(user);
    if let Some(home) = resolved.as_ref() {
        let mut cache = USER_HOMES.write().unwrap_or_else(|e| e.into_inner());
        if cache.len() >= MAX_NSS_CACHE_ENTRIES {
            cache.clear();
        }
        cache.insert(user.to_owned(), Arc::from(home.as_slice()));
    }
    resolved
}

fn getpwnam_home_dir_uncached(user: &str) -> Option<Vec<u8>> {
    // Keep raw bytes: home directories need not be UTF-8.
    nix::unistd::User::from_name(user)
        .ok()
        .flatten()
        .map(|user| OsStrExt::as_bytes(user.dir.as_os_str()).to_vec())
}

pub fn map_io_error(err: std::io::Error, path: &str) -> RpcError {
    use std::io::ErrorKind;

    match err.kind() {
        ErrorKind::NotFound => RpcError::file_not_found(path),
        ErrorKind::PermissionDenied => RpcError::permission_denied(path),
        ErrorKind::AlreadyExists => {
            let mut rpc_error = RpcError::io_error(err);
            let mut fields = match rpc_error.data.take() {
                Some(Value::Map(fields)) => fields,
                _ => Vec::new(),
            };
            if !fields.iter().any(|(k, _)| k.as_str() == Some("error_kind")) {
                fields.push((
                    Value::String("error_kind".into()),
                    Value::String("already_exists".into()),
                ));
            }
            if !fields.iter().any(|(k, _)| k.as_str() == Some("os_errno")) {
                fields.push((
                    Value::String("os_errno".into()),
                    Value::Integer(libc::EEXIST.into()),
                ));
            }
            rpc_error.data = Some(Value::Map(fields));
            rpc_error
        }
        _ => RpcError::io_error(err),
    }
}

/// Convert raw bytes to a PathBuf without blocking a Tokio worker on NSS.
pub async fn bytes_to_path(bytes: &[u8]) -> Result<PathBuf, RpcError> {
    let expanded = if bytes
        .strip_prefix(b"~")
        .is_some_and(|rest| !rest.is_empty() && !rest.starts_with(b"/"))
    {
        let bytes = bytes.to_vec();
        tokio::task::spawn_blocking(move || super::system::expand_tilde_bytes(&bytes))
            .await
            .map_err(|error| {
                RpcError::internal_error(format!("Tilde expansion task join error: {error}"))
            })?
    } else {
        super::system::expand_tilde_bytes(bytes)
    };

    Ok(match expanded {
        Some(expanded) => PathBuf::from(OsStr::from_bytes(&expanded)),
        None => PathBuf::from(OsStr::from_bytes(bytes)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msgpack_map;

    fn access_params(path: &Path, mode: &str) -> Value {
        Value::Map(vec![
            (
                Value::from("path"),
                Value::Binary(path.as_os_str().as_bytes().to_vec()),
            ),
            (Value::from("mode"), Value::from(mode)),
        ])
    }

    fn access_errno(result: Value) -> Option<i64> {
        result
            .as_map()?
            .iter()
            .find(|(key, _)| key.as_str() == Some("errno"))?
            .1
            .as_i64()
    }

    #[tokio::test]
    async fn test_access_paths_and_errors() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join("file");
        std::fs::write(&file, "contents").unwrap();
        assert_eq!(
            access_errno(access(access_params(&file, "rw")).await.unwrap()),
            Some(0)
        );
        assert_eq!(
            access_errno(
                access(access_params(&temp.path().join("./"), ""))
                    .await
                    .unwrap()
            ),
            Some(0)
        );
        let result = access(access_params(&file.join("./"), "")).await.unwrap();
        assert_eq!(access_errno(result), Some(libc::ENOTDIR.into()));

        let missing = temp.path().join("missing");
        let link = temp.path().join("link");
        std::os::unix::fs::symlink(&missing, &link).unwrap();
        let result = access(access_params(&link, "r")).await.unwrap();
        assert_eq!(access_errno(result), Some(libc::ENOENT.into()));
        std::fs::remove_file(&link).unwrap();
        std::os::unix::fs::symlink("link", &link).unwrap();
        let result = access(access_params(&link, "r")).await.unwrap();
        assert_eq!(access_errno(result), Some(libc::ELOOP.into()));
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn test_access_non_utf8_path() {
        let temp = tempfile::tempdir().unwrap();
        let file = temp.path().join(OsStr::from_bytes(b"file-\xff"));
        std::fs::write(&file, "contents").unwrap();
        assert_eq!(
            access_errno(access(access_params(&file, "rw")).await.unwrap()),
            Some(0)
        );
    }

    fn writable_params(path: &Path) -> Value {
        msgpack_map! {
            "path" => Value::Binary(path.as_os_str().as_bytes().to_vec()),
            "mode" => "w",
            "check_parent_if_missing" => true,
        }
    }

    #[tokio::test]
    async fn test_access_missing_writable_parent() {
        let temp = tempfile::tempdir().unwrap();
        let missing = temp.path().join("missing");
        assert_eq!(
            access_errno(access(writable_params(&missing)).await.unwrap()),
            Some(0)
        );
        for path in [missing.join(""), missing.join("."), missing.join("child")] {
            assert_eq!(
                access_errno(access(writable_params(&path)).await.unwrap()),
                Some(libc::ENOENT.into())
            );
        }
        let file = temp.path().join("file");
        std::fs::write(&file, "contents").unwrap();
        assert_eq!(
            access_errno(access(writable_params(&file.join("child"))).await.unwrap()),
            Some(libc::ENOTDIR.into())
        );
        if !rustix::process::geteuid().is_root() {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o444)).unwrap();
            assert_eq!(
                access_errno(access(writable_params(&file)).await.unwrap()),
                Some(libc::EACCES.into())
            );
            std::fs::set_permissions(temp.path(), std::fs::Permissions::from_mode(0o555)).unwrap();
            let result = access(writable_params(&missing)).await.unwrap();
            std::fs::set_permissions(temp.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
            assert_eq!(access_errno(result), Some(libc::EACCES.into()));
        }
    }

    #[test]
    fn test_access_parent_preserves_path_bytes() {
        for (path, expected) in [
            (b"/parent/missing".as_slice(), b"/parent/".as_slice()),
            (b"/parent/missing/", b"/parent/missing/"),
            (b"/parent/missing/.", b"/parent/missing/"),
            (b"/parent-\xff/missing", b"/parent-\xff/"),
            (b"missing", b"."),
        ] {
            assert_eq!(
                access_parent(Path::new(OsStr::from_bytes(path)))
                    .unwrap()
                    .as_os_str()
                    .as_bytes(),
                expected
            );
        }
        assert!(access_parent(Path::new("")).is_none());
    }

    #[tokio::test]
    async fn test_access_parent_rejects_other_modes() {
        for mode in ["", "r", "rw", "wx"] {
            let error = access(msgpack_map! {
                "path" => "/", "mode" => mode, "check_parent_if_missing" => true,
            })
            .await
            .unwrap_err();
            assert_eq!(error.code, RpcError::INVALID_PARAMS);
        }
    }

    #[tokio::test]
    async fn test_access_rejects_invalid_mode() {
        let error = access(access_params(Path::new("/"), "z"))
            .await
            .unwrap_err();
        assert_eq!(error.code, RpcError::INVALID_PARAMS);
    }

    #[test]
    fn test_resolve_truename_missing_under_symlink_and_dangling() {
        let temp = tempfile::tempdir().unwrap();
        let canonical_root = std::fs::canonicalize(temp.path()).unwrap();
        let real_dir = canonical_root.join("real");
        std::fs::create_dir(&real_dir).unwrap();
        let link_dir = canonical_root.join("linkdir");
        std::os::unix::fs::symlink(&real_dir, &link_dir).unwrap();

        // Missing file under symlinked parent directory resolves parent symlink.
        let resolved = resolve_truename_sync(&link_dir.join("missing/sub")).unwrap();
        assert_eq!(resolved, real_dir.join("missing/sub"));

        // Dangling symlink under symlinked parent resolves both links.
        let dangling = real_dir.join("dangling");
        std::os::unix::fs::symlink("target-missing", &dangling).unwrap();
        let resolved_dangling = resolve_truename_sync(&link_dir.join("dangling")).unwrap();
        assert_eq!(resolved_dangling, real_dir.join("target-missing"));

        // Cyclic symlink returns ELOOP.
        let loop_a = canonical_root.join("loop_a");
        let loop_b = canonical_root.join("loop_b");
        std::os::unix::fs::symlink(&loop_b, &loop_a).unwrap();
        std::os::unix::fs::symlink(&loop_a, &loop_b).unwrap();
        let err = resolve_truename_sync(&loop_a).unwrap_err();
        assert_eq!(err.raw_os_error(), Some(libc::ELOOP));
    }

    #[test]
    fn test_parse_getent_name_ignores_truncated_member_list() {
        // The capture limit cuts a two-byte character in the member list.
        let mut capture = "staff:x:50:".as_bytes().to_vec();
        while capture.len() <= GETENT_MAX_CAPTURE_BYTES {
            capture.extend_from_slice("é".as_bytes());
        }
        capture.truncate(GETENT_MAX_CAPTURE_BYTES);
        assert!(std::str::from_utf8(&capture).is_err());
        assert_eq!(parse_getent_name(&capture), Ok(Some("staff".to_string())));

        assert_eq!(
            parse_getent_name(b"gr\xc3\xbcn:x:1:\n"),
            Ok(Some("grün".into()))
        );
        assert_eq!(parse_getent_name(b":x:1:\n"), Ok(None));
        assert_eq!(parse_getent_name(b"no-separator"), Err(()));
        assert_eq!(parse_getent_name(b"bad\xff:x:1:\n"), Err(()));
    }

    #[test]
    fn test_resolve_truename_follows_finite_chain_past_libc_limit() {
        let temp = tempfile::tempdir().unwrap();
        let root = std::fs::canonicalize(temp.path()).unwrap();
        let target = root.join("target");
        std::fs::write(&target, b"x").unwrap();
        // Longer than the 40 links libc `realpath' follows, shorter than the
        // MAX_SYMLINK_HOPS that Emacs `file-truename' allows.
        let mut previous = target.clone();
        for index in 0..60 {
            let link = root.join(format!("link-{index}"));
            std::os::unix::fs::symlink(&previous, &link).unwrap();
            previous = link;
        }

        assert_eq!(resolve_truename_sync(&previous).unwrap(), target);
    }

    #[test]
    fn test_get_user_name_current_uid() {
        let uid = rustix::process::getuid().as_raw();
        let name = get_user_name(uid);
        assert!(
            name.is_some(),
            "get_user_name({uid}) should resolve the current user"
        );
        assert!(
            !name.as_ref().unwrap().is_empty(),
            "resolved name should be non-empty"
        );
    }

    #[test]
    fn test_get_group_name_current_gid() {
        let gid = rustix::process::getgid().as_raw();
        let name = get_group_name(gid);
        assert!(
            name.is_some(),
            "get_group_name({gid}) should resolve the current group"
        );
        assert!(
            !name.as_ref().unwrap().is_empty(),
            "resolved name should be non-empty"
        );
    }

    #[test]
    fn test_get_user_name_unknown_uid() {
        let name = get_user_name(0x7FFF_FFFE);
        assert!(
            name.is_none(),
            "get_user_name for a non-existent uid should return None"
        );
    }

    #[test]
    fn test_get_group_name_unknown_gid() {
        let name = get_group_name(0x7FFF_FFFE);
        assert!(
            name.is_none(),
            "get_group_name for a non-existent gid should return None"
        );
    }

    #[test]
    fn test_get_user_name_root() {
        let name = get_user_name(0);
        assert_eq!(name.as_deref(), Some("root"));
    }

    #[test]
    fn test_get_group_name_root() {
        let name = get_group_name(0);
        assert!(name.is_some(), "gid 0 should always have a group entry");
    }

    #[tokio::test]
    async fn test_expand_tilde_preserves_non_utf8_suffix() {
        let Some(home) = std::env::var_os("HOME") else {
            return;
        };
        let path = bytes_to_path(b"~/\xff").await.unwrap();
        let mut expected = PathBuf::from(home);
        expected.push(OsStr::from_bytes(b"\xff"));
        assert_eq!(path, expected);
    }

    #[tokio::test]
    async fn test_expand_tilde_preserves_repeated_separators() {
        let Some(home) = std::env::var_os("HOME") else {
            return;
        };
        let path = bytes_to_path(b"~//\xff").await.unwrap();
        let mut expected = home;
        expected.push(OsStr::from_bytes(b"//\xff"));
        assert_eq!(path, PathBuf::from(expected));
    }

    #[test]
    fn test_user_name_caching() {
        let uid = rustix::process::getuid().as_raw();
        let first = get_user_name(uid);
        let second = get_user_name(uid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    #[test]
    fn test_group_name_caching() {
        let gid = rustix::process::getgid().as_raw();
        let first = get_group_name(gid);
        let second = get_group_name(gid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    #[test]
    fn test_user_home_misses_are_not_cached() {
        let user = format!("tramp-rpc-missing-user-{}", std::process::id());
        assert_eq!(get_user_home_dir(&user), None);
        let cache = USER_HOMES.read().unwrap_or_else(|e| e.into_inner());
        assert!(!cache.contains_key(&user));
    }

    fn getent_fields(database: &str, key: &str) -> Option<Vec<String>> {
        let output = Command::new("getent")
            .arg(database)
            .arg(key)
            .output()
            .ok()?;
        if !output.status.success() {
            return None;
        }
        let stdout = String::from_utf8(output.stdout).ok()?;
        Some(
            stdout
                .lines()
                .next()?
                .split(':')
                .map(str::to_owned)
                .collect(),
        )
    }

    #[test]
    fn test_passwd_lookups_match_getent() {
        let uid = rustix::process::getuid().as_raw();
        let Some(fields) = getent_fields("passwd", &uid.to_string()) else {
            return;
        };
        let (name, dir, shell) = (&fields[0], &fields[5], &fields[6]);
        assert_eq!(nss_name_by_id(NssKind::User, uid), Ok(Some(name.clone())));
        assert_eq!(get_user_name(uid).as_deref(), Some(name.as_str()));
        assert_eq!(get_user_login_shell(uid).as_deref(), Some(shell.as_str()));
        assert_eq!(get_user_home_dir(name), Some(dir.as_bytes().to_vec()));
    }

    #[test]
    fn test_group_lookup_matches_getent() {
        let gid = rustix::process::getgid().as_raw();
        let Some(fields) = getent_fields("group", &gid.to_string()) else {
            return;
        };
        let name = &fields[0];
        assert_eq!(nss_name_by_id(NssKind::Group, gid), Ok(Some(name.clone())));
        assert_eq!(get_group_name(gid).as_deref(), Some(name.as_str()));
    }

    #[tokio::test]
    async fn test_stat_returns_user_group_names() {
        use std::io::Write;
        let mut tmp = tempfile::NamedTempFile::new().expect("create tempfile");
        write!(tmp, "test").unwrap();

        let path = tmp.path().to_path_buf();
        let attrs = get_file_attributes(path, false)
            .await
            .expect("stat should succeed");

        assert!(
            attrs.uname.is_some(),
            "stat should resolve user name, got uid={}",
            attrs.uid
        );
        assert!(
            attrs.gname.is_some(),
            "stat should resolve group name, got gid={}",
            attrs.gid
        );

        let expected_uid = rustix::process::getuid().as_raw();
        assert_eq!(attrs.uid, expected_uid);
        assert_eq!(
            attrs.uname.as_deref(),
            get_user_name(expected_uid).as_deref()
        );
    }
}
