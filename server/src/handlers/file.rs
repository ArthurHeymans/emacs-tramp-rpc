// SPDX-License-Identifier: GPL-3.0-or-later

//! File metadata operations

use crate::protocol::{FileAttributes, FileType, RpcError, from_value};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::io::Read;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::{Duration, Instant};
use tokio::fs;

use super::HandlerResult;

/// Get file attributes
pub async fn stat(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Path as string (UTF-8) or bytes (non-UTF8)
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
        /// If true, don't follow symlinks
        #[serde(default)]
        lstat: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path).await?;
    match get_file_attributes(path.as_path(), params.lstat).await {
        Ok(attrs) => Ok(attrs.to_value()),
        Err(e) if e.code == RpcError::FILE_NOT_FOUND => Ok(Value::Nil),
        Err(e) => Err(e),
    }
}

/// Get the true name of a file (resolve symlinks)
pub async fn truename(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        #[serde(with = "path_or_bytes")]
        path: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let path = bytes_to_path(&params.path).await?;
    let path_str = path.to_string_lossy().into_owned();

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

    let file_type = file_type_from_metadata_ft(&metadata.file_type());

    let link_target = if file_type == FileType::Symlink {
        fs::read_link(path)
            .await
            .ok()
            .map(|p| p.as_os_str().as_bytes().to_vec())
    } else {
        None
    };

    let uid = metadata.uid();
    let gid = metadata.gid();

    // Name resolution can block (libc NSS + getent subprocess): run it on a
    // dedicated blocking thread so Tokio worker threads are not stalled.
    let (uname, gname) =
        tokio::task::spawn_blocking(move || (get_user_name(uid), get_group_name(gid)))
            .await
            .unwrap_or((None, None));

    Ok(FileAttributes {
        file_type,
        nlinks: metadata.nlink(),
        uid,
        gid,
        uname,
        gname,
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

/// Cache of uid -> resolved name. A cached `None` records a *definitive*
/// miss (the uid is absent from all NSS databases). Transient failures
/// (timeout, backend unavailable) are not cached so a later lookup can
/// succeed once the directory service recovers.
static USER_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, Option<String>>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Cache of gid -> resolved name. Same caching semantics as `USER_NAMES`.
static GROUP_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, Option<String>>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Short negative cache for transient NSS failures (LDAP/SSSD outage).
/// Without this, every directory entry with the same uid/gid retries libc +
/// `getent` (~2s each), turning one listing into a per-entry retry storm.
/// Entries expire after `TRANSIENT_NSS_TTL` so recovery is still prompt.
const TRANSIENT_NSS_TTL: Duration = Duration::from_secs(10);
static TRANSIENT_USER_FAILURES: std::sync::LazyLock<Mutex<HashMap<u32, Instant>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));
static TRANSIENT_GROUP_FAILURES: std::sync::LazyLock<Mutex<HashMap<u32, Instant>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

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

/// Join the reader thread with a bounded timeout.
///
/// After `child.kill()` + `child.wait()` the direct child's write-end of the
/// pipe is closed, but an orphaned subprocess that inherited the fd would keep
/// `read_to_end` (and therefore `join`) blocked forever.  We poll
/// `is_finished` for up to `GETENT_READER_TIMEOUT`; if it doesn't complete we
/// leak the thread (it will eventually unblock once the orphan exits or the
/// pipe is otherwise closed).
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
/// Returns a tri-state:
/// - `Ok(Some(name))`: id resolved successfully.
/// - `Ok(None)`: id is definitively absent (getent exited non-zero cleanly).
/// - `Err(())`: transient failure (spawn error, timeout, I/O error); the
///   caller must *not* cache this result so a later lookup can succeed once
///   the directory service recovers.
///
/// Stdout is drained on a dedicated thread so the child can never block
/// when its output exceeds the pipe buffer (e.g. `getent group` with many
/// members). The deadline kills the child if it does not exit in time.
fn getent_name(database: &str, id: u32) -> Result<Option<String>, ()> {
    let mut child = Command::new("getent")
        .arg(database)
        .arg(id.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|_| ())?;

    // Drain stdout on a separate thread so the child is never blocked
    // writing when its output exceeds the pipe buffer.
    let mut stdout = child.stdout.take().ok_or(())?;
    let reader = std::thread::spawn(move || {
        let mut buf = Vec::new();
        stdout.read_to_end(&mut buf).ok();
        buf
    });

    let deadline = Instant::now() + GETENT_TIMEOUT;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if Instant::now() >= deadline {
                    // Deadline expired: terminate the child and give up.
                    let _ = child.kill();
                    let _ = child.wait();
                    bounded_join_reader(reader);
                    return Err(()); // transient: timeout
                }
                std::thread::sleep(GETENT_POLL_INTERVAL);
            }
            Err(_) => {
                let _ = child.kill();
                let _ = child.wait();
                bounded_join_reader(reader);
                return Err(()); // transient: I/O error on try_wait
            }
        }
    };

    let buf = reader.join().map_err(|_| ())?;

    if !status.success() {
        return Ok(None); // definitive: id does not exist in any NSS database
    }

    let line = std::str::from_utf8(&buf)
        .ok()
        .and_then(|s| s.lines().next())
        .ok_or(())?;
    let name = line.split(':').next().ok_or(())?;
    if name.is_empty() {
        Ok(None)
    } else {
        Ok(Some(name.to_string()))
    }
}

/// Reentrant passwd/group lookup by id through nix, which retries with a
/// growing buffer on `ERANGE` so large LDAP/SSSD records still resolve.
///
/// Returns `Ok(Some(name))` on a hit, `Ok(None)` when libc reports a miss and
/// `Err(errno)` when the backend failed (unavailable, timeout, ...).
fn nss_name_by_id(kind: NssKind, id: u32) -> nix::Result<Option<String>> {
    match kind {
        NssKind::User => nix::unistd::User::from_uid(nix::unistd::Uid::from_raw(id))
            .map(|user| user.map(|user| user.name)),
        NssKind::Group => nix::unistd::Group::from_gid(nix::unistd::Gid::from_raw(id))
            .map(|group| group.map(|group| group.name)),
    }
}

/// Shared NSS name resolution for both uid and gid.
///
/// Uses `getpwuid_r` or `getgrgid_r` (selected by `kind`) through nix, falls
/// back to `getent` on libc misses and errors, caches definitive results
/// indefinitely, and caches transient failures (timeout, backend
/// unavailable) for `TRANSIENT_NSS_TTL` so one listing under outage does not
/// retry per entry.
fn resolve_nss_name(
    cache: &'static Mutex<HashMap<u32, Option<String>>>,
    kind: NssKind,
    id: u32,
) -> Option<String> {
    // Fast path: check cache under lock, release immediately.
    {
        let c = cache.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(result) = c.get(&id) {
            return result.clone();
        }
    }
    // Short negative cache for transient outage: fail fast without hitting
    // libc/getent again within the TTL window.
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
        // Some NSS stacks can report a libc miss here while `getent` still
        // resolves through another backend; libc errors (backend unavailable,
        // etc.) take the same fallback.
        Ok(None) | Err(_) => getent_name(database, id),
    };

    // Cache definitive results indefinitely; cache transient failures for
    // TRANSIENT_NSS_TTL so a later lookup can succeed once the service
    // recovers, without retrying per directory entry during an outage.
    match name_result {
        Ok(name) => {
            let mut c = cache.lock().unwrap_or_else(|e| e.into_inner());
            c.insert(id, name.clone());
            name
        }
        Err(()) => {
            record_transient_failure(transient_cache, id);
            None
        }
    }
}

pub fn get_user_name(uid: u32) -> Option<String> {
    resolve_nss_name(&USER_NAMES, NssKind::User, uid)
}

pub fn get_group_name(gid: u32) -> Option<String> {
    resolve_nss_name(&GROUP_NAMES, NssKind::Group, gid)
}

/// Resolve the login shell for `uid` via getpwuid_r.
///
/// nix retries with a growing buffer on `ERANGE`, so passwd records larger
/// than the initial sysconf hint (large LDAP/SSSD entries) still resolve
/// instead of being reported as absent.
pub(crate) fn get_user_login_shell(uid: u32) -> Option<String> {
    nix::unistd::User::from_uid(nix::unistd::Uid::from_raw(uid))
        .ok()
        .flatten()
        .and_then(|user| user.shell.into_os_string().into_string().ok())
}

/// Cache of user name -> resolved home directory.
///
/// Only successful lookups are cached.  In particular, arbitrary nonexistent
/// user names must not grow this process-lifetime map without bound.
static USER_HOMES: std::sync::LazyLock<Mutex<HashMap<String, Vec<u8>>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Resolve the home directory for a user name via getpwnam_r.
///
/// Used for `~user` tilde expansion.  Results are cached because the lookup
/// can hit slow NSS backends.  Failed lookups stay uncached so transient
/// directory-service failures can be retried without retaining arbitrary
/// nonexistent user names forever.
pub(crate) fn get_user_home_dir(user: &str) -> Option<Vec<u8>> {
    {
        let cache = USER_HOMES.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(home) = cache.get(user) {
            return Some(home.clone());
        }
    }

    let resolved = getpwnam_home_dir_uncached(user);
    if let Some(home) = resolved.as_ref() {
        let mut cache = USER_HOMES.lock().unwrap_or_else(|e| e.into_inner());
        cache.insert(user.to_owned(), home.clone());
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
            if rpc_error.data.is_none() {
                rpc_error.data = Some(Value::Map(vec![(
                    Value::String("os_errno".into()),
                    Value::Integer(libc::EEXIST.into()),
                )]));
            }
            rpc_error
        }
        _ => RpcError::io_error(err),
    }
}

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

/// Convert raw bytes to a PathBuf without blocking a Tokio worker on NSS.
pub async fn bytes_to_path(bytes: &[u8]) -> Result<PathBuf, RpcError> {
    // Tilde expansion operates on the raw bytes so non-UTF-8 path components
    // never pass through a lossy string conversion.  Only `~user` expansion
    // can reach NSS, so ordinary paths and `$HOME` expansion remain immediate.
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

use crate::protocol::path_or_bytes;

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify get_user_name resolves the current process uid.
    /// This must succeed on any system -- the running user always has
    /// a passwd entry (local or NSS/LDAP).
    #[test]
    fn test_get_user_name_current_uid() {
        let uid = nix::unistd::getuid().as_raw();
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

    /// Verify get_group_name resolves the current process gid.
    #[test]
    fn test_get_group_name_current_gid() {
        let gid = nix::unistd::getgid().as_raw();
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

    /// A uid that almost certainly has no passwd entry should return
    /// None rather than panicking or looping forever.
    /// Note: 0xFFFF_FFFE (-2 signed) is macOS's `nobody`, so we use
    /// 0x7FFF_FFFE which is unused on both Linux and macOS.
    #[test]
    fn test_get_user_name_unknown_uid() {
        let name = get_user_name(0x7FFF_FFFE);
        assert!(
            name.is_none(),
            "get_user_name for a non-existent uid should return None"
        );
    }

    /// A gid that almost certainly has no group entry should return None.
    #[test]
    fn test_get_group_name_unknown_gid() {
        let name = get_group_name(0x7FFF_FFFE);
        assert!(
            name.is_none(),
            "get_group_name for a non-existent gid should return None"
        );
    }

    /// Verify root (uid 0) resolves to "root".
    #[test]
    fn test_get_user_name_root() {
        let name = get_user_name(0);
        assert_eq!(name.as_deref(), Some("root"));
    }

    /// Verify gid 0 resolves (usually "root" or "wheel").
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

    /// Repeated separators after "~/" must survive: the suffix begins with
    /// "/" and must not replace the HOME prefix.
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

    /// Repeated lookups should hit the cache and return the same value.
    #[test]
    fn test_user_name_caching() {
        let uid = nix::unistd::getuid().as_raw();
        let first = get_user_name(uid);
        let second = get_user_name(uid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    /// Repeated group lookups should hit the cache.
    #[test]
    fn test_group_name_caching() {
        let gid = nix::unistd::getgid().as_raw();
        let first = get_group_name(gid);
        let second = get_group_name(gid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    /// Failed user-home lookups must not grow the process-lifetime cache.
    #[test]
    fn test_user_home_misses_are_not_cached() {
        let user = format!("tramp-rpc-missing-user-{}", std::process::id());
        assert_eq!(get_user_home_dir(&user), None);
        let cache = USER_HOMES.lock().unwrap_or_else(|e| e.into_inner());
        assert!(!cache.contains_key(&user));
    }

    /// Independent oracle for the nix passwd/group mapping: the `getent`
    /// binary resolves through the same NSS stack but shares no code with
    /// our wrappers, so a mixed-up field (name, dir, shell) or a spurious
    /// libc miss shows up as a mismatch.  Returns the colon-separated record
    /// fields, or None when `getent` is unavailable or the key is absent.
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

    /// The uid, login shell and `~user` lookups must agree field for field
    /// with `getent passwd`.  The direct libc lookup is checked as well,
    /// since `get_user_name` would otherwise hide a libc miss behind its own
    /// `getent` fallback.
    #[test]
    fn test_passwd_lookups_match_getent() {
        let uid = nix::unistd::getuid().as_raw();
        let Some(fields) = getent_fields("passwd", &uid.to_string()) else {
            return; // no getent on this platform, or no passwd entry
        };
        let (name, dir, shell) = (&fields[0], &fields[5], &fields[6]);
        assert_eq!(nss_name_by_id(NssKind::User, uid), Ok(Some(name.clone())));
        assert_eq!(get_user_name(uid).as_deref(), Some(name.as_str()));
        assert_eq!(get_user_login_shell(uid).as_deref(), Some(shell.as_str()));
        assert_eq!(get_user_home_dir(name), Some(dir.as_bytes().to_vec()));
    }

    /// The gid lookup must agree with `getent group`, directly and cached.
    #[test]
    fn test_group_lookup_matches_getent() {
        let gid = nix::unistd::getgid().as_raw();
        let Some(fields) = getent_fields("group", &gid.to_string()) else {
            return; // no getent on this platform, or no group entry
        };
        let name = &fields[0];
        assert_eq!(nss_name_by_id(NssKind::Group, gid), Ok(Some(name.clone())));
        assert_eq!(get_group_name(gid).as_deref(), Some(name.as_str()));
    }

    /// Verify that file.stat via the RPC handler returns uname/gname for
    /// a file owned by the current user (e.g. /tmp which is world-writable,
    /// so we create a temp file to be certain of ownership).
    #[tokio::test]
    async fn test_stat_returns_user_group_names() {
        use std::io::Write;
        let mut tmp = tempfile::NamedTempFile::new().expect("create tempfile");
        write!(tmp, "test").unwrap();

        let path = tmp.path();
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

        let expected_uid = nix::unistd::getuid().as_raw();
        assert_eq!(attrs.uid, expected_uid);
        assert_eq!(
            attrs.uname.as_deref(),
            get_user_name(expected_uid).as_deref()
        );
    }
}
