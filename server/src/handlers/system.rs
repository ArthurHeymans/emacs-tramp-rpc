// SPDX-License-Identifier: GPL-3.0-or-later

//! `system.*` handlers and the tilde expansion they share with the path
//! handling in the other modules.

use crate::msgpack_map;
use crate::protocol::{IntoValue, RpcError, from_value};
use rmpv::Value;

use super::{HandlerResult, file, io};

/// Get system information without running NSS lookups on a Tokio worker.
pub(super) async fn system_info() -> HandlerResult {
    use nix::unistd::{getgid, getuid};
    use std::env;

    let shell = tokio::task::spawn_blocking(login_shell)
        .await
        .map_err(|e| RpcError::internal_error(format!("Login shell task join error: {e}")))?;

    Ok(msgpack_map! {
        "version" => env!("CARGO_PKG_VERSION"),
        "os" => std::env::consts::OS,
        "arch" => std::env::consts::ARCH,
        "watcher" => watcher_kind(),
        "watcher_available" => Value::Boolean(crate::watcher::is_active()),
        "max_read_chunk_bytes" => io::MAX_FILE_READ_CHUNK_BYTES as u64,
        "hostname" => hostname(),
        "uid" => getuid().as_raw(),
        "gid" => getgid().as_raw(),
        "home" => env::var("HOME").ok().into_value(),
        "user" => env::var("USER").ok().into_value(),
        "shell" => shell.into_value()
    })
}

fn watcher_kind() -> &'static str {
    use notify::{RecommendedWatcher, Watcher, WatcherKind};

    match RecommendedWatcher::kind() {
        WatcherKind::Inotify => "inotify",
        WatcherKind::Fsevent => "fsevent",
        WatcherKind::Kqueue => "kqueue",
        WatcherKind::PollWatcher => "poll",
        WatcherKind::ReadDirectoryChangesWatcher => "windows",
        WatcherKind::NullWatcher => "null",
        _ => "unknown",
    }
}

/// Look up the current user's login shell from the passwd database.
/// Delegates to file.rs's shared getpwuid_r lookup, which retries with a
/// growing buffer on ERANGE so large NSS records (LDAP, SSSD) still resolve.
fn login_shell() -> Option<String> {
    let uid = nix::unistd::getuid().as_raw();
    file::get_user_login_shell(uid)
}

pub(super) fn hostname() -> String {
    use nix::unistd::gethostname;

    gethostname()
        .ok()
        .and_then(|name| name.into_string().ok())
        .unwrap_or_else(|| "unknown".to_string())
}
/// Get environment variable
pub(super) fn system_getenv(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        name: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    Ok(std::env::var(&params.name).ok().into_value())
}

/// Expand path with tilde and environment variables
pub(super) async fn system_expand_path(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // `~user` expansion consults the passwd database, which can block on
    // slow NSS backends; keep it off the Tokio workers.
    let expanded = tokio::task::spawn_blocking(move || expand_tilde(&params.path))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {e}")))?;
    Ok(expanded.into_value())
}

/// Get filesystem information (like df)
pub(super) async fn system_statvfs(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Both `~user` expansion (passwd lookup) and the statvfs call block;
    // run them off the Tokio workers.
    tokio::task::spawn_blocking(move || statvfs_blocking(&params.path))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {e}")))?
}

fn statvfs_blocking(path: &str) -> HandlerResult {
    use nix::sys::statvfs;
    use std::os::unix::ffi::OsStrExt;

    // Expand at the byte level: HOME and passwd home directories need not be
    // UTF-8, and NixPath accepts raw OsStr bytes.
    let expanded = expand_tilde_bytes(path.as_bytes());
    let expanded = std::ffi::OsStr::from_bytes(expanded.as_deref().unwrap_or(path.as_bytes()));
    let stats = statvfs::statvfs(std::path::Path::new(expanded))
        .map_err(|error| RpcError::io_error(std::io::Error::from(error)))?;

    // Return values in bytes (multiply by block size)
    #[allow(clippy::unnecessary_cast)]
    let block_size = stats.fragment_size() as u64;
    let total = stats.blocks() as u64 * block_size;
    let free = stats.blocks_free() as u64 * block_size;
    let available = stats.blocks_available() as u64 * block_size;

    Ok(msgpack_map! {
        "total" => total,
        "free" => free,
        "available" => available,
        "block_size" => block_size
    })
}

/// Get groups for the current user without NSS lookups on a Tokio worker.
pub(super) async fn system_groups() -> HandlerResult {
    tokio::task::spawn_blocking(|| {
        // rustix sizes the buffer with a `getgroups(0)` probe, which is the
        // one wrapper available on every supported platform (nix declines to
        // expose `getgroups` on Apple targets).
        let gids: Vec<u32> = rustix::process::getgroups()
            .map_err(|error| RpcError::io_error(error.into()))?
            .iter()
            .map(|gid| gid.as_raw())
            .collect();

        // Convert to group info with names
        let group_info: Vec<Value> = gids
            .iter()
            .map(|&gid| {
                let gname = get_group_name(gid);
                msgpack_map! {
                    "gid" => gid,
                    "name" => gname.into_value()
                }
            })
            .collect();

        Ok(Value::Array(group_info))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Group lookup task join error: {e}")))?
}

/// Get group name from gid (delegates to file.rs's mutex-protected, cached version)
fn get_group_name(gid: u32) -> Option<String> {
    file::get_group_name(gid)
}

/// Expand ~ to home directory.
///
/// Handles `~`, `~/...`, and `~user/...` (resolved via the passwd database).
/// Upstream tramp-sh relies on the remote shell to expand these at every
/// operation; this server has no shell, so path resolution must do it.
pub(crate) fn expand_tilde(path: &str) -> String {
    match expand_tilde_bytes(path.as_bytes()) {
        Some(expanded) => String::from_utf8_lossy(&expanded).into_owned(),
        None => path.to_string(),
    }
}

/// Byte-level tilde expansion used by `bytes_to_path`, so non-UTF-8 paths
/// never pass through a lossy string conversion.
///
/// Returns None when the path contains no expandable tilde prefix.
pub(crate) fn expand_tilde_bytes(path: &[u8]) -> Option<Vec<u8>> {
    // var_os keeps raw bytes: HOME need not be valid UTF-8.
    if let Some(home) = std::env::var_os("HOME") {
        let home = std::os::unix::ffi::OsStrExt::as_bytes(home.as_os_str());
        if path == b"~" {
            return Some(home.to_vec());
        }
        if let Some(rest) = path.strip_prefix(b"~/") {
            let mut expanded = home.to_vec();
            expanded.push(b'/');
            expanded.extend_from_slice(rest);
            return Some(expanded);
        }
    }

    // `~user` or `~user/...`.  The user name must be valid UTF-8 to look up.
    let rest = path.strip_prefix(b"~")?;
    if rest.starts_with(b"/") || rest.is_empty() {
        return None;
    }
    let (user, suffix) = match rest.iter().position(|&byte| byte == b'/') {
        Some(slash) => (&rest[..slash], &rest[slash..]),
        None => (rest, &b""[..]),
    };
    let user = std::str::from_utf8(user).ok()?;
    let mut expanded = file::get_user_home_dir(user)?;
    expanded.extend_from_slice(suffix);
    Some(expanded)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tilde_expands_home_and_user() {
        let home = std::env::var("HOME").unwrap();
        assert_eq!(expand_tilde("~"), home);
        assert_eq!(expand_tilde("~/foo/bar"), format!("{home}/foo/bar"));
        assert_eq!(expand_tilde("/tmp"), "/tmp");
        assert_eq!(expand_tilde("~/"), format!("{home}/"));
    }

    #[test]
    fn tilde_bytes_expand_without_lossy_conversion() {
        let home = std::env::var("HOME").unwrap();
        // A non-UTF-8 component must survive expansion untouched.
        let path = b"~/\xff";
        let expanded = expand_tilde_bytes(path).expect("~ path should expand");
        let mut expected = home.into_bytes();
        expected.push(b'/');
        expected.push(0xff);
        assert_eq!(expanded, expected);
        assert_eq!(expand_tilde_bytes(b"/tmp/\xff"), None);
    }

    #[test]
    fn tilde_user_expands_via_passwd_database() {
        let name = "root";
        let Some(expected) = file::get_user_home_dir(name) else {
            return; // no root entry on this host; nothing to compare
        };
        let expected = String::from_utf8_lossy(&expected).into_owned();
        assert_eq!(expand_tilde("~root"), expected);
        assert_eq!(expand_tilde("~root/foo"), format!("{expected}/foo"));
        assert!(!expand_tilde("~nonexistent-user-xyz").starts_with('/'));
    }
}
