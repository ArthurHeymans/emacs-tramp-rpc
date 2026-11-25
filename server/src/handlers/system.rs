// SPDX-License-Identifier: GPL-3.0-or-later

//! `system.*` handlers and the tilde expansion they share with the path
//! handling in the other modules.

use crate::msgpack_map;
use crate::protocol::{IntoValue, RpcError, RpcPath, from_value};
use rmpv::Value;
use std::path::Path;

use super::{HandlerResult, file, io};

/// Get system information without running blocking probes on a Tokio worker.
pub(super) async fn system_info() -> HandlerResult {
    use rustix::process::{getgid, getuid};
    use std::env;

    let (shell, default_path, acl_available, selinux_available) =
        super::run_blocking("System info", || {
            Ok((
                login_shell(),
                confstr_default_path(),
                command_exists_in_path(
                    "getfacl",
                    std::env::var_os("PATH").as_deref(),
                    confstr_default_path().as_deref(),
                ),
                selinux_is_enabled(),
            ))
        })
        .await?;

    Ok(msgpack_map! {
        "version" => env!("CARGO_PKG_VERSION"),
        "protocol_revision" => crate::protocol::PROTOCOL_REVISION,
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
        "shell" => shell.into_value(),
        "default_path" => default_path.into_value(),
        "signals" => signal_table_value(),
        "acl_available" => Value::Boolean(acl_available),
        "selinux_available" => Value::Boolean(selinux_available)
    })
}

/// Resolve a user's home directory natively via `getpwnam_r` (no `getent`
/// subprocess, works on Linux and macOS, preserves non-UTF-8 paths).
pub(super) async fn system_user_home(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        user: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let home = super::run_blocking("User home", move || {
        Ok(file::get_user_home_dir(&params.user))
    })
    .await?;
    Ok(home.map_or(Value::Nil, Value::Binary))
}

/// Query POSIX `confstr(_CS_PATH)` so the client never has to spawn `getconf PATH`.
fn confstr_default_path() -> Option<String> {
    // SAFETY: `confstr` with a null buffer and zero length safely queries the
    // required buffer size including NUL terminator.
    let len = unsafe { libc::confstr(libc::_CS_PATH, std::ptr::null_mut(), 0) };
    if len <= 1 {
        return None;
    }
    let mut buf = vec![0u8; len];
    // SAFETY: `buf` has capacity `len` bytes.
    let written = unsafe {
        libc::confstr(
            libc::_CS_PATH,
            buf.as_mut_ptr().cast::<libc::c_char>(),
            buf.len(),
        )
    };
    if written == 0 {
        return None;
    }
    let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    let path = String::from_utf8_lossy(&buf[..end]).into_owned();
    (!path.is_empty()).then_some(path)
}

/// Return `[signal_number, "NAME"]` pairs for every named OS signal so the
/// client never has to spawn `/bin/sh -c "kill -l"`.
fn signal_table_value() -> Value {
    let entries: Vec<Value> = nix::sys::signal::Signal::iterator()
        .map(|sig| {
            let name = sig.as_str();
            let short = name.strip_prefix("SIG").unwrap_or(name);
            Value::Array(vec![
                Value::Integer((sig as i32).into()),
                Value::String(short.into()),
            ])
        })
        .collect();
    Value::Array(entries)
}

/// Check if an executable named `cmd` exists in `PATH` or POSIX default `PATH`.
/// Whether `process.run` can execute CMD: search PATH_VAR when set, and
/// DEFAULT_PATH only when it is not, as `execvp' does.  Advertising a
/// command found elsewhere would make the capability and the command
/// runner disagree.
fn command_exists_in_path(
    cmd: &str,
    path_var: Option<&std::ffi::OsStr>,
    default_path: Option<&str>,
) -> bool {
    let search = path_var.or(default_path.map(std::ffi::OsStr::new));
    search
        .into_iter()
        .flat_map(std::env::split_paths)
        .any(|dir| {
            let candidate = dir.join(cmd);
            candidate.is_file()
                && crate::access::check(&candidate, rustix::fs::Access::EXEC_OK).is_ok()
        })
}

/// Check whether SELinux is active on this host without spawning `selinuxenabled`.
fn selinux_is_enabled() -> bool {
    #[cfg(target_os = "linux")]
    {
        Path::new("/sys/fs/selinux/enforce").exists() || Path::new("/selinux/enforce").exists()
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = Path::new("");
        false
    }
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
fn login_shell() -> Option<String> {
    let uid = rustix::process::getuid().as_raw();
    file::get_user_login_shell(uid)
}

pub(super) fn hostname() -> String {
    let uname = rustix::system::uname();
    uname.nodename().to_string_lossy().into_owned()
}

/// Get filesystem information (like df)
pub(super) async fn system_statvfs(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: RpcPath,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Both `~user` expansion (passwd lookup) and the statvfs call block;
    // run them off the Tokio workers.
    super::run_blocking("Statvfs", move || statvfs_blocking(&params.path)).await
}

fn statvfs_blocking(path: &RpcPath) -> HandlerResult {
    let resolved = path.resolve_sync();
    let stats = rustix::fs::statvfs(&resolved)
        .map_err(|error| RpcError::io_error(std::io::Error::from(error)))?;

    #[allow(clippy::unnecessary_cast)]
    let block_size = stats.f_frsize as u64;
    let total = (stats.f_blocks as u64).saturating_mul(block_size);
    let free = (stats.f_bfree as u64).saturating_mul(block_size);
    let available = (stats.f_bavail as u64).saturating_mul(block_size);

    Ok(msgpack_map! {
        "total" => total,
        "free" => free,
        "available" => available,
        "block_size" => block_size
    })
}

/// Expand ~ to home directory.
#[cfg(test)]
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

    // `~user` or `~user/...`. The user name must be valid UTF-8 to look up.
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

    #[tokio::test]
    async fn system_info_reports_protocol_revision_and_metadata() {
        let info = system_info().await.unwrap();
        let map = info.as_map().unwrap();
        let get = |k: &str| {
            map.iter()
                .find(|(key, _)| key.as_str() == Some(k))
                .map(|(_, v)| v)
        };
        assert_eq!(
            get("protocol_revision").and_then(Value::as_u64),
            Some(crate::protocol::PROTOCOL_REVISION)
        );
        assert!(
            get("watch_remove_nofollow").is_none(),
            "dead capability shim should be removed"
        );
        assert!(
            get("signals")
                .and_then(Value::as_array)
                .is_some_and(|a| !a.is_empty())
        );
        assert!(get("acl_available").and_then(Value::as_bool).is_some());
        assert!(get("selinux_available").and_then(Value::as_bool).is_some());
        assert!(confstr_default_path().is_some());
    }

    #[test]
    fn command_lookup_follows_exec_search_path() {
        use std::os::unix::fs::PermissionsExt;
        let temp = tempfile::tempdir().unwrap();
        let (inherited, default) = (temp.path().join("bin"), temp.path().join("default"));
        for dir in [&inherited, &default] {
            std::fs::create_dir(dir).unwrap();
        }
        let tool = default.join("tool");
        std::fs::write(&tool, "#!/bin/sh\n").unwrap();
        std::fs::set_permissions(&tool, std::fs::Permissions::from_mode(0o755)).unwrap();
        let default_path = default.to_str();

        // With PATH set, exec never consults the default path.
        assert!(!command_exists_in_path(
            "tool",
            Some(inherited.as_os_str()),
            default_path
        ));
        assert!(command_exists_in_path("tool", None, default_path));
        assert!(command_exists_in_path(
            "tool",
            Some(default.as_os_str()),
            None
        ));
    }

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
            return;
        };
        let expected = String::from_utf8_lossy(&expected).into_owned();
        assert_eq!(expand_tilde("~root"), expected);
        assert_eq!(expand_tilde("~root/foo"), format!("{expected}/foo"));
        assert!(!expand_tilde("~nonexistent-user-xyz").starts_with('/'));
    }
}
