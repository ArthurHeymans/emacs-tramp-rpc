//! File metadata operations

use crate::protocol::{from_value, FileAttributes, FileType, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::os::unix::fs::{FileTypeExt, MetadataExt};
use std::path::Path;
use std::sync::Mutex;
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

    let path = bytes_to_path(&params.path);
    match get_file_attributes(&path, params.lstat).await {
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

    let path = bytes_to_path(&params.path);
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

    let file_type = get_file_type(&metadata);

    let link_target = if file_type == FileType::Symlink {
        fs::read_link(path)
            .await
            .ok()
            .map(|p| p.to_string_lossy().into_owned())
    } else {
        None
    };

    let uid = metadata.uid();
    let gid = metadata.gid();

    Ok(FileAttributes {
        file_type,
        nlinks: metadata.nlink(),
        uid,
        gid,
        uname: get_user_name(uid),
        gname: get_group_name(gid),
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

fn get_file_type(metadata: &std::fs::Metadata) -> FileType {
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

static USER_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, String>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Initial buffer size hint from sysconf, or a reasonable default.
fn sysconf_bufsize(name: libc::c_int, fallback: usize) -> usize {
    let ret = unsafe { libc::sysconf(name) };
    if ret > 0 {
        ret as usize
    } else {
        fallback
    }
}

/// Maximum buffer size we will attempt before giving up (1 MiB).
/// Used for both getpwuid_r and getgrgid_r retry loops.
const MAX_NSS_BUFSIZE: usize = 1024 * 1024;

/// Get user name from uid using thread-safe getpwuid_r.
///
/// Uses `sysconf(_SC_GETPW_R_SIZE_MAX)` for the initial buffer size and
/// retries with a doubled buffer on `ERANGE`, which can happen when user
/// records are served by LDAP or other NSS backends that return large
/// entries.
///
/// The mutex is only held for cache lookups/inserts, not during the
/// (potentially slow) NSS syscall, to avoid blocking other threads
/// when the directory backend is slow.
pub fn get_user_name(uid: u32) -> Option<String> {
    // Fast path: check cache under lock, release immediately.
    {
        let cache = USER_NAMES.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(result) = cache.get(&uid) {
            return Some(result.clone());
        }
    }

    // Slow path: perform the syscall without holding the lock.
    let init_size = sysconf_bufsize(libc::_SC_GETPW_R_SIZE_MAX, 1024);
    let mut bufsize = init_size;

    let name = loop {
        let mut buf = vec![0u8; bufsize];
        let mut pwd: libc::passwd = unsafe { std::mem::zeroed() };
        let mut result_ptr: *mut libc::passwd = std::ptr::null_mut();

        let ret = unsafe {
            libc::getpwuid_r(
                uid,
                &mut pwd,
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut result_ptr,
            )
        };

        if ret == libc::ERANGE && bufsize < MAX_NSS_BUFSIZE {
            bufsize = bufsize.saturating_mul(2).min(MAX_NSS_BUFSIZE);
            continue;
        }

        if ret != 0 || result_ptr.is_null() {
            return None;
        }

        let cname = unsafe { std::ffi::CStr::from_ptr(pwd.pw_name) };
        break cname.to_str().ok().map(|s| s.to_string());
    };

    // Re-acquire lock to insert into cache.
    if let Some(ref n) = name {
        let mut cache = USER_NAMES.lock().unwrap_or_else(|e| e.into_inner());
        cache.insert(uid, n.clone());
    }

    name
}

static GROUP_NAMES: std::sync::LazyLock<Mutex<HashMap<u32, String>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Get group name from gid using thread-safe getgrgid_r.
///
/// Uses `sysconf(_SC_GETGR_R_SIZE_MAX)` for the initial buffer size and
/// retries with a doubled buffer on `ERANGE`, which can happen when group
/// records are served by LDAP or other NSS backends that return large
/// entries (e.g. groups with many members).
///
/// The mutex is only held for cache lookups/inserts, not during the
/// (potentially slow) NSS syscall.
pub fn get_group_name(gid: u32) -> Option<String> {
    // Fast path: check cache under lock, release immediately.
    {
        let cache = GROUP_NAMES.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(result) = cache.get(&gid) {
            return Some(result.clone());
        }
    }

    // Slow path: perform the syscall without holding the lock.
    let init_size = sysconf_bufsize(libc::_SC_GETGR_R_SIZE_MAX, 1024);
    let mut bufsize = init_size;

    let name = loop {
        let mut buf = vec![0u8; bufsize];
        let mut grp: libc::group = unsafe { std::mem::zeroed() };
        let mut result_ptr: *mut libc::group = std::ptr::null_mut();

        let ret = unsafe {
            libc::getgrgid_r(
                gid,
                &mut grp,
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                &mut result_ptr,
            )
        };

        if ret == libc::ERANGE && bufsize < MAX_NSS_BUFSIZE {
            bufsize = bufsize.saturating_mul(2).min(MAX_NSS_BUFSIZE);
            continue;
        }

        if ret != 0 || result_ptr.is_null() {
            return None;
        }

        let cname = unsafe { std::ffi::CStr::from_ptr(grp.gr_name) };
        break cname.to_str().ok().map(|s| s.to_string());
    };

    // Re-acquire lock to insert into cache.
    if let Some(ref n) = name {
        let mut cache = GROUP_NAMES.lock().unwrap_or_else(|e| e.into_inner());
        cache.insert(gid, n.clone());
    }

    name
}

pub fn map_io_error(err: std::io::Error, path: &str) -> RpcError {
    use std::io::ErrorKind;

    match err.kind() {
        ErrorKind::NotFound => RpcError::file_not_found(path),
        ErrorKind::PermissionDenied => RpcError::permission_denied(path),
        _ => RpcError::io_error(err),
    }
}

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

/// Convert raw bytes to a PathBuf
pub fn bytes_to_path(bytes: &[u8]) -> PathBuf {
    let path = PathBuf::from(OsStr::from_bytes(bytes));
    expand_tilde_path(&path)
}

/// Expand ~ to home directory in a PathBuf.
/// Delegates to the canonical string-based `expand_tilde` in mod.rs.
fn expand_tilde_path(path: &Path) -> PathBuf {
    let s = path.to_string_lossy();
    let expanded = super::expand_tilde(&s);
    if expanded.as_str() != s.as_ref() {
        PathBuf::from(expanded)
    } else {
        path.to_path_buf()
    }
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
        let uid = unsafe { libc::getuid() };
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
        let gid = unsafe { libc::getgid() };
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

    /// Repeated lookups should hit the cache and return the same value.
    #[test]
    fn test_user_name_caching() {
        let uid = unsafe { libc::getuid() };
        let first = get_user_name(uid);
        let second = get_user_name(uid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    /// Repeated group lookups should hit the cache.
    #[test]
    fn test_group_name_caching() {
        let gid = unsafe { libc::getgid() };
        let first = get_group_name(gid);
        let second = get_group_name(gid);
        assert_eq!(first, second, "cached lookup should match initial lookup");
    }

    /// sysconf_bufsize should return a positive value for _SC_GETPW_R_SIZE_MAX.
    #[test]
    fn test_sysconf_bufsize_pw() {
        let size = sysconf_bufsize(libc::_SC_GETPW_R_SIZE_MAX, 1024);
        assert!(size > 0, "sysconf should return a positive buffer size");
    }

    /// sysconf_bufsize should return a positive value for _SC_GETGR_R_SIZE_MAX.
    #[test]
    fn test_sysconf_bufsize_gr() {
        let size = sysconf_bufsize(libc::_SC_GETGR_R_SIZE_MAX, 1024);
        assert!(size > 0, "sysconf should return a positive buffer size");
    }

    /// sysconf_bufsize should return the fallback for an invalid argument.
    #[test]
    fn test_sysconf_bufsize_fallback() {
        // -1 is not a valid sysconf name, so it should return the fallback.
        let size = sysconf_bufsize(-1, 4096);
        assert_eq!(size, 4096, "invalid sysconf should return fallback");
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

        let expected_uid = unsafe { libc::getuid() };
        assert_eq!(attrs.uid, expected_uid);
        assert_eq!(
            attrs.uname.as_deref(),
            get_user_name(expected_uid).as_deref()
        );
    }
}
