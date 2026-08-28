//! Request handlers for TRAMP-RPC operations

pub mod commands;
pub mod dir;
pub mod file;
pub mod io;
pub mod process;

use crate::msgpack_map;
use crate::protocol::{Request, Response, RpcError, from_value};
use futures::{StreamExt, TryStreamExt};
use rmpv::Value;

/// Dispatch a request to the appropriate handler
pub async fn dispatch(request: Request) -> Response {
    // Handle batch separately (it needs special handling and can't recurse)
    if request.method == "batch" {
        let id = request.id.clone();
        let result = batch_execute(request.params).await;
        let response = match result {
            Ok(value) => Response::success(id.clone(), value),
            Err(error) => Response::error(Some(id.clone()), error),
        };
        return match validate_batch_response_size(&response, crate::MAX_FRAME_SIZE) {
            Ok(()) => response,
            Err(error) => Response::error(Some(id), error),
        };
    }

    // All other methods go through dispatch_inner
    dispatch_inner(request).await
}

/// Signal and reap managed async children before a connection task exits.
pub async fn cleanup_managed_processes() -> Result<(), RpcError> {
    process::cleanup_managed_processes().await
}

pub type HandlerResult = Result<Value, RpcError>;

/// Get system information without running NSS lookups on a Tokio worker.
async fn system_info() -> HandlerResult {
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

use crate::protocol::IntoValue;

/// Maximum entries accepted by one batch request.  This remains well above
/// the existing 10-stat benchmark while bounding per-request work.
const MAX_BATCH_ENTRIES: usize = 64;
/// Keep nested batch work below the global general-request admission limit.
pub(crate) const BATCH_CONCURRENCY: usize = 4;

fn bounded_batch_futures<F>(
    futures: impl IntoIterator<Item = F>,
) -> impl futures::Stream<Item = F::Output>
where
    F: std::future::Future,
{
    futures::stream::iter(futures).buffered(BATCH_CONCURRENCY)
}

fn hostname() -> String {
    use nix::unistd::gethostname;

    gethostname()
        .ok()
        .and_then(|name| name.into_string().ok())
        .unwrap_or_else(|| "unknown".to_string())
}
/// Get environment variable
fn system_getenv(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        name: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    Ok(std::env::var(&params.name).ok().into_value())
}

/// Expand path with tilde and environment variables
async fn system_expand_path(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // `~user` expansion consults the passwd database, which can block on
    // slow NSS backends; keep it off the Tokio workers.
    let expanded = tokio::task::spawn_blocking(move || expand_tilde(&params.path))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?;
    Ok(expanded.into_value())
}

/// Get filesystem information (like df)
async fn system_statvfs(params: Value) -> HandlerResult {
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

    let expanded = expand_tilde(path);
    // NixPath accepts raw OsStr bytes, so non-UTF-8 paths still resolve.
    let stats = statvfs::statvfs(std::path::Path::new(&expanded))
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

/// Apple targets where nix does not expose `getgroups` (membership lives in
/// opendirectoryd, so nix declines to wrap the raw call there).
#[cfg(any(
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos",
    target_os = "visionos"
))]
fn current_groups() -> std::io::Result<Vec<libc::gid_t>> {
    let count = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
    if count < 0 {
        return Err(std::io::Error::last_os_error());
    }

    // Group membership can change after the sizing call; retry with a larger
    // buffer instead of relying on a fixed supplementary limit.
    let mut count = count as usize;
    loop {
        let mut groups = vec![0; count];
        let actual_count =
            unsafe { libc::getgroups(groups.len() as libc::c_int, groups.as_mut_ptr()) };
        if actual_count < 0 {
            let error = std::io::Error::last_os_error();
            if matches!(
                error.raw_os_error(),
                Some(libc::EINVAL) | Some(libc::ERANGE)
            ) {
                count = count.saturating_mul(2).max(1);
                continue;
            }
            return Err(error);
        }
        groups.truncate(actual_count as usize);
        return Ok(groups);
    }
}

/// Get groups for the current user
/// Get groups for the current user without NSS lookups on a Tokio worker.
async fn system_groups() -> HandlerResult {
    tokio::task::spawn_blocking(|| {
        #[cfg(not(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "watchos",
            target_os = "visionos"
        )))]
        use nix::unistd::{Gid, getgroups};

        #[cfg(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "watchos",
            target_os = "visionos"
        ))]
        // Raw libc fallback: nix does not expose `getgroups` on Apple platforms.
        let gids = current_groups().map_err(RpcError::io_error)?;

        #[cfg(not(any(
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "watchos",
            target_os = "visionos"
        )))]
        // nix sizes the buffer internally and retries when group membership
        // changes between the sizing and retrieval calls.
        let gids: Vec<_> = getgroups()
            .map_err(|error| RpcError::io_error(error.into()))?
            .iter()
            .map(|&gid| Gid::as_raw(gid))
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
fn get_group_name(gid: libc::gid_t) -> Option<String> {
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

fn validate_batch_response_size(
    response: &Response,
    max_frame_size: usize,
) -> Result<(), RpcError> {
    let encoded_size = rmp_serde::to_vec_named(response)
        .map_err(|error| {
            RpcError::internal_error(format!("Failed to encode batch response: {error}"))
        })?
        .len();
    if encoded_size > max_frame_size {
        return Err(RpcError::internal_error(
            "Batch response exceeds maximum frame size",
        ));
    }
    Ok(())
}

/// Execute multiple RPC requests in a single batch
async fn batch_execute(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct BatchParams {
        requests: Vec<BatchRequest>,
    }

    fn default_params() -> Value {
        Value::Nil
    }

    #[derive(serde::Deserialize)]
    struct BatchRequest {
        method: String,
        #[serde(default = "default_params")]
        params: Value,
    }

    let batch_params: BatchParams =
        from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    if batch_params.requests.len() > MAX_BATCH_ENTRIES {
        return Err(RpcError::invalid_params(format!(
            "batch requests cannot exceed {MAX_BATCH_ENTRIES} entries"
        )));
    }

    let results = bounded_batch_futures(batch_params.requests.into_iter().map(|req| async move {
        // Batch subrequests have no request id of their own; route them
        // directly to the handlers instead of round-tripping through a
        // synthetic Request/Response pair.
        let result = match route(req.method, req.params).await {
            Ok(value) => msgpack_map! { "result" => value },
            Err(error) => {
                let mut error_fields = vec![
                    (
                        Value::String("code".into()),
                        Value::Integer(error.code.into()),
                    ),
                    (
                        Value::String("message".into()),
                        Value::String(error.message.into()),
                    ),
                ];
                if let Some(data) = error.data {
                    error_fields.push((Value::String("data".into()), data));
                }
                msgpack_map! {
                    "error" => Value::Map(error_fields)
                }
            }
        };
        Ok::<Value, RpcError>(result)
    }))
    .try_fold(
        (Vec::new(), 0usize),
        |(mut results, size), result| async move {
            let result_size = rmp_serde::to_vec_named(&result)
                .map_err(|error| {
                    RpcError::internal_error(format!("Failed to encode batch entry: {error}"))
                })?
                .len();
            let size = size
                .checked_add(result_size)
                .ok_or_else(|| RpcError::internal_error("Batch response size overflow"))?;
            if size > crate::MAX_FRAME_SIZE {
                return Err(RpcError::internal_error(
                    "Batch response exceeds maximum frame size",
                ));
            }
            results.push(result);
            Ok((results, size))
        },
    )
    .await
    .map(|(results, _)| results)?;

    Ok(msgpack_map! { "results" => Value::Array(results) })
}

/// Dispatch a single request to its handler and wrap the outcome in a
/// response for its request id.
async fn dispatch_inner(request: Request) -> Response {
    let Request {
        id, method, params, ..
    } = request;
    match route(method, params).await {
        Ok(value) => Response::success(id, value),
        Err(error) => Response::error(Some(id), error),
    }
}

/// Route a method to its handler.  Shared by single requests and batch
/// subrequests.
async fn route(method: String, params: Value) -> HandlerResult {
    match method.as_str() {
        // File metadata operations
        "file.stat" => file::stat(params).await,
        "file.truename" => file::truename(params).await,

        // Directory operations
        "dir.list" => dir::list(params).await,
        "dir.create" => dir::create(params).await,
        "dir.remove" => dir::remove(params).await,

        // File I/O operations
        "file.read" => io::read(params).await,
        "file.write" => io::write(params).await,
        "file.copy" => io::copy(params).await,
        "file.rename" => io::rename(params).await,
        "file.delete" => io::delete(params).await,
        "file.set_modes" => io::set_modes(params).await,
        "file.set_times" => io::set_times(params).await,
        "file.make_symlink" => io::make_symlink(params).await,
        "file.make_hardlink" => io::make_hardlink(params).await,
        "file.chown" => io::chown(params).await,

        // Process operations
        "process.run" => process::run(params).await,
        "process.start" => process::start(params).await,
        "process.write" => process::write(params).await,
        "process.read" => process::read(params).await,
        "process.status" => process::status(params).await,
        "process.close_stdin" => process::close_stdin(params).await,
        "process.kill" => process::kill(params).await,
        "process.signal" => process::signal_pid(params).await,
        "process.list" => process::list(params).await,

        // PTY (pseudo-terminal) process operations
        "process.start_pty" => process::start_pty(params).await,
        "process.read_pty" => process::read_pty(params).await,
        "process.write_pty" => process::write_pty(params).await,
        "process.resize_pty" => process::resize_pty(params).await,
        "process.kill_pty" => process::kill_pty(params).await,
        "process.close_pty" => process::close_pty(params).await,
        "process.list_pty" => process::list_pty(params).await,

        // System info
        "system.info" => system_info().await,
        "system.getenv" => system_getenv(params),
        "system.expand_path" => system_expand_path(params).await,
        "system.statvfs" => system_statvfs(params).await,
        "system.groups" => system_groups().await,

        // Parallel command execution and ancestor scanning
        "commands.run_parallel" => commands::run_parallel(params).await,
        "ancestors.scan" => commands::ancestors_scan(params).await,
        "highlevel.test_files_in_dir" => commands::highlevel_test_files_in_dir(params).await,
        "highlevel.locate_dominating_file_multi" => {
            commands::highlevel_locate_dominating_file_multi(params).await
        }
        "highlevel.dir_locals_find_file_cache_update" => {
            commands::highlevel_dir_locals_find_file_cache_update(params).await
        }

        // Filesystem watch operations (for cache invalidation)
        "watch.add" => crate::watcher::handle_add(params).await,
        "watch.remove" => crate::watcher::handle_remove(params).await,
        "watch.list" => crate::watcher::handle_list(params),

        // Note: "batch" is NOT allowed in batch (no recursion)
        _ => Err(RpcError::method_not_found(&method)),
    }
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

    use crate::msgpack_map;
    use futures::StreamExt;
    use std::os::unix::ffi::OsStrExt;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use tokio::sync::{Barrier, Semaphore};

    #[tokio::test]
    async fn batch_accepts_max_entries_in_order() {
        let result = batch_execute(msgpack_map! {
            "requests" => Value::Array(
                (0..MAX_BATCH_ENTRIES)
                    .map(|index| msgpack_map! {
                        "method" => "system.expand_path",
                        "params" => msgpack_map! {
                            "path" => format!("/batch-order-{index}"),
                        },
                    })
                    .collect(),
            ),
        })
        .await
        .expect("maximum-sized batch should be accepted");

        let results = result
            .as_map()
            .and_then(|map| map.iter().find(|(key, _)| key.as_str() == Some("results")))
            .and_then(|(_, value)| value.as_array())
            .expect("results array");
        assert_eq!(results.len(), MAX_BATCH_ENTRIES);
        for (index, entry) in results.iter().enumerate() {
            let value = entry
                .as_map()
                .and_then(|map| map.iter().find(|(key, _)| key.as_str() == Some("result")))
                .and_then(|(_, value)| value.as_str())
                .expect("ordered batch result");
            assert_eq!(value, format!("/batch-order-{index}"));
        }
    }

    #[tokio::test]
    async fn batch_executor_limits_in_flight_subrequests() {
        let active = Arc::new(AtomicUsize::new(0));
        let peak = Arc::new(AtomicUsize::new(0));
        let ready = Arc::new(Barrier::new(BATCH_CONCURRENCY + 1));
        let release = Arc::new(Semaphore::new(0));
        let total = BATCH_CONCURRENCY * 2;
        let worker_active = Arc::clone(&active);
        let worker_peak = Arc::clone(&peak);
        let worker_ready = Arc::clone(&ready);
        let worker_release = Arc::clone(&release);
        let futures = (0..total).map(move |index| {
            let active = Arc::clone(&worker_active);
            let peak = Arc::clone(&worker_peak);
            let ready = Arc::clone(&worker_ready);
            let release = Arc::clone(&worker_release);
            async move {
                let in_flight = active.fetch_add(1, Ordering::AcqRel) + 1;
                peak.fetch_max(in_flight, Ordering::AcqRel);
                if index < BATCH_CONCURRENCY {
                    ready.wait().await;
                }
                let _permit = release.acquire().await.expect("release semaphore is open");
                active.fetch_sub(1, Ordering::AcqRel);
                index
            }
        });
        let executor =
            tokio::spawn(async move { bounded_batch_futures(futures).collect::<Vec<_>>().await });

        tokio::time::timeout(std::time::Duration::from_secs(1), ready.wait())
            .await
            .expect("executor should reach the configured concurrency");
        assert_eq!(active.load(Ordering::Acquire), BATCH_CONCURRENCY);
        assert!(peak.load(Ordering::Acquire) <= BATCH_CONCURRENCY);
        assert_eq!(peak.load(Ordering::Acquire), BATCH_CONCURRENCY);

        release.add_permits(total);
        assert_eq!(
            executor
                .await
                .expect("bounded executor task should complete"),
            (0..total).collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn batch_rejects_too_many_entries_and_keeps_nested_non_recursive() {
        let too_many = batch_execute(msgpack_map! {
            "requests" => Value::Array(
                (0..=MAX_BATCH_ENTRIES)
                    .map(|_| msgpack_map! { "method" => "system.info" })
                    .collect(),
            ),
        })
        .await
        .expect_err("oversized batch should be rejected");
        assert_eq!(too_many.code, RpcError::INVALID_PARAMS);

        let nested = batch_execute(msgpack_map! {
            "requests" => Value::Array(vec![msgpack_map! {
                "method" => "batch",
                "params" => msgpack_map! {
                    "requests" => Value::Array(vec![msgpack_map! {
                        "method" => "system.info",
                    }]),
                },
            }]),
        })
        .await
        .expect("nested batch should remain a bounded per-entry error");
        let nested_error_code = nested
            .as_map()
            .and_then(|map| map.iter().find(|(key, _)| key.as_str() == Some("results")))
            .and_then(|(_, value)| value.as_array())
            .and_then(|results| results.first())
            .and_then(Value::as_map)
            .and_then(|entry| entry.iter().find(|(key, _)| key.as_str() == Some("error")))
            .and_then(|(_, value)| value.as_map())
            .and_then(|error| error.iter().find(|(key, _)| key.as_str() == Some("code")))
            .and_then(|(_, value)| value.as_i64());
        assert_eq!(
            nested_error_code,
            Some(i64::from(RpcError::METHOD_NOT_FOUND))
        );

        let result = msgpack_map! {
            "results" => Value::Array(vec![Value::Binary(vec![0; 64])]),
        };
        let response = Response::success(crate::protocol::RequestId::Number(99), result.clone());
        let inner_size = rmp_serde::to_vec_named(&result).unwrap().len();
        let frame_size = rmp_serde::to_vec_named(&response).unwrap().len();
        assert!(frame_size > inner_size);
        let frame_error = validate_batch_response_size(&response, inner_size)
            .expect_err("response envelope must count toward the frame limit");
        assert_eq!(frame_error.code, RpcError::INTERNAL_ERROR);
        validate_batch_response_size(&response, frame_size)
            .expect("a response exactly at the frame limit is legal");
    }

    #[tokio::test]
    async fn batch_errors_preserve_data() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let file = tmp.path().join("file");
        tokio::fs::write(&file, b"payload").await.unwrap();
        let not_a_dir = file.join("child");

        let result = batch_execute(msgpack_map! {
            "requests" => Value::Array(vec![msgpack_map! {
                "method" => "file.stat",
                "params" => msgpack_map! {
                    "path" => Value::Binary(not_a_dir.as_os_str().as_bytes().to_vec()),
                },
            }]),
        })
        .await
        .expect("batch should return per-request errors");

        let results = result
            .as_map()
            .and_then(|m| m.iter().find(|(k, _)| k.as_str() == Some("results")))
            .and_then(|(_, v)| v.as_array())
            .expect("results array");
        let error = results[0]
            .as_map()
            .and_then(|m| m.iter().find(|(k, _)| k.as_str() == Some("error")))
            .and_then(|(_, v)| v.as_map())
            .expect("error map");
        let data = error
            .iter()
            .find(|(k, _)| k.as_str() == Some("data"))
            .and_then(|(_, v)| v.as_map())
            .expect("error data");
        let errno = data
            .iter()
            .find(|(k, _)| k.as_str() == Some("os_errno"))
            .and_then(|(_, v)| v.as_i64())
            .expect("os_errno");

        assert_eq!(errno, i64::from(libc::ENOTDIR));
    }
}
