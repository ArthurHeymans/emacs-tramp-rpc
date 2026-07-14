//! Request handlers for TRAMP-RPC operations

pub mod commands;
pub mod dir;
pub mod file;
pub mod io;
pub mod process;

use crate::msgpack_map;
use crate::protocol::{Request, RequestId, Response, RpcError, from_value};
use futures::{StreamExt, TryStreamExt};
use rmpv::Value;

/// Dispatch a request to the appropriate handler
pub async fn dispatch(request: Request) -> Response {
    // Handle batch separately (it needs special handling and can't recurse)
    if request.method == "batch" {
        let result = batch_execute(request.params.clone()).await;
        return match result {
            Ok(value) => Response::success(request.id.clone(), value),
            Err(error) => Response::error(Some(request.id.clone()), error),
        };
    }

    // All other methods go through dispatch_inner
    dispatch_inner(request).await
}

/// Signal and reap managed async children before a connection task exits.
pub async fn cleanup_managed_processes() {
    process::cleanup_managed_processes().await;
}

pub type HandlerResult = Result<Value, RpcError>;

/// Get system information
fn system_info() -> HandlerResult {
    use std::env;

    Ok(msgpack_map! {
        "version" => env!("CARGO_PKG_VERSION"),
        "os" => std::env::consts::OS,
        "arch" => std::env::consts::ARCH,
        "watcher" => watcher_kind(),
        "hostname" => hostname(),
        "uid" => unsafe { libc::getuid() },
        "gid" => unsafe { libc::getgid() },
        "home" => env::var("HOME").ok().into_value(),
        "user" => env::var("USER").ok().into_value(),
        "shell" => login_shell().into_value()
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
/// Uses getpwuid_r (reentrant) for thread safety, matching the pattern
/// in file.rs for get_user_name/get_group_name.
fn login_shell() -> Option<String> {
    let uid = unsafe { libc::getuid() };
    let mut buf = vec![0u8; 1024];
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

    if ret != 0 || result_ptr.is_null() {
        return None;
    }

    let shell = unsafe { std::ffi::CStr::from_ptr(pwd.pw_shell) };
    shell.to_str().ok().map(|s| s.to_string())
}

use crate::protocol::IntoValue;

/// Maximum entries accepted by one batch request.  This remains well above
/// the existing 10-stat benchmark while bounding per-request work.
const MAX_BATCH_ENTRIES: usize = 64;
/// Keep nested batch work below the global general-request admission limit.
const BATCH_CONCURRENCY: usize = 4;

fn bounded_batch_futures<F>(
    futures: impl IntoIterator<Item = F>,
) -> impl futures::Stream<Item = F::Output>
where
    F: std::future::Future,
{
    futures::stream::iter(futures).buffered(BATCH_CONCURRENCY)
}

fn hostname() -> String {
    let mut buf = [0u8; 256];
    unsafe {
        if libc::gethostname(buf.as_mut_ptr() as *mut libc::c_char, buf.len()) == 0 {
            let len = buf.iter().position(|&c| c == 0).unwrap_or(buf.len());
            String::from_utf8_lossy(&buf[..len]).into_owned()
        } else {
            "unknown".to_string()
        }
    }
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
fn system_expand_path(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let expanded = expand_tilde(&params.path);
    Ok(expanded.into_value())
}

/// Get filesystem information (like df)
fn system_statvfs(params: Value) -> HandlerResult {
    #[derive(serde::Deserialize)]
    struct Params {
        path: String,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    use std::ffi::CString;
    let expanded = expand_tilde(&params.path);
    let path_cstr =
        CString::new(expanded.as_str()).map_err(|_| RpcError::invalid_params("Invalid path"))?;

    let mut stat: libc::statvfs = unsafe { std::mem::zeroed() };
    let result = unsafe { libc::statvfs(path_cstr.as_ptr(), &mut stat) };

    if result != 0 {
        return Err(RpcError::io_error(std::io::Error::last_os_error()));
    }

    // Return values in bytes (multiply by block size)
    // Allow unnecessary casts for cross-platform compatibility (types differ between Linux/macOS)
    #[allow(clippy::unnecessary_cast)]
    let block_size = stat.f_frsize as u64;
    #[allow(clippy::unnecessary_cast)]
    let total = stat.f_blocks as u64 * block_size;
    #[allow(clippy::unnecessary_cast)]
    let free = stat.f_bfree as u64 * block_size;
    #[allow(clippy::unnecessary_cast)]
    let available = stat.f_bavail as u64 * block_size;

    Ok(msgpack_map! {
        "total" => total,
        "free" => free,
        "available" => available,
        "block_size" => block_size
    })
}

fn supplementary_groups_with<F>(
    initial_count: usize,
    mut getgroups: F,
) -> std::io::Result<Vec<libc::gid_t>>
where
    F: FnMut(&mut [libc::gid_t]) -> std::io::Result<usize>,
{
    let mut count = initial_count;
    loop {
        let mut groups = vec![0; count];
        match getgroups(&mut groups) {
            Ok(actual_count) => {
                groups.truncate(actual_count);
                return Ok(groups);
            }
            // Group membership can change after the sizing call.  Retry with a
            // larger buffer instead of relying on a fixed supplementary limit.
            Err(error)
                if matches!(
                    error.raw_os_error(),
                    Some(libc::EINVAL) | Some(libc::ERANGE)
                ) =>
            {
                count = count.saturating_mul(2).max(1);
            }
            Err(error) => return Err(error),
        }
    }
}

fn supplementary_groups() -> std::io::Result<Vec<libc::gid_t>> {
    let count = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
    if count < 0 {
        return Err(std::io::Error::last_os_error());
    }

    supplementary_groups_with(count as usize, |groups| {
        let actual_count =
            unsafe { libc::getgroups(groups.len() as libc::c_int, groups.as_mut_ptr()) };
        if actual_count < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(actual_count as usize)
        }
    })
}

/// Get groups for the current user
fn system_groups() -> HandlerResult {
    let groups = supplementary_groups().map_err(RpcError::io_error)?;

    // Convert to group info with names
    let group_info: Vec<Value> = groups
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
}

/// Get group name from gid (delegates to file.rs's mutex-protected, cached version)
fn get_group_name(gid: libc::gid_t) -> Option<String> {
    file::get_group_name(gid)
}

/// Expand ~ to home directory
pub(crate) fn expand_tilde(path: &str) -> String {
    if path.starts_with("~/") {
        if let Ok(home) = std::env::var("HOME") {
            return format!("{}{}", home, &path[1..]);
        }
    } else if path == "~"
        && let Ok(home) = std::env::var("HOME")
    {
        return home;
    }
    path.to_string()
}

fn validate_batch_response_size(value: &Value, max_frame_size: usize) -> Result<(), RpcError> {
    let encoded_size = rmp_serde::to_vec_named(value)
        .map_err(|error| {
            RpcError::internal_error(format!("Failed to encode batch response: {error}"))
        })?
        .len();
    if encoded_size >= max_frame_size {
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
        // Create a fake Request to reuse dispatch logic.
        let fake_request = Request {
            version: "2.0".to_string(),
            id: RequestId::Number(0), // Dummy ID, not used in batch
            method: req.method,
            params: req.params,
        };

        // Get the result by calling the handler directly (not full dispatch).
        let response = dispatch_inner(fake_request).await;

        // Convert Response to a result object.
        let result = match (response.result, response.error) {
            (Some(result), None) => msgpack_map! { "result" => result },
            (None, Some(error)) => {
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
            _ => msgpack_map! { "result" => Value::Nil },
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
            if size >= crate::MAX_FRAME_SIZE {
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

    let result = msgpack_map! { "results" => Value::Array(results) };
    validate_batch_response_size(&result, crate::MAX_FRAME_SIZE)?;
    Ok(result)
}

/// Inner dispatch that handles the actual method routing
/// Used by both single requests and batch requests
async fn dispatch_inner(request: Request) -> Response {
    let Request {
        id, method, params, ..
    } = request;

    let result = match method.as_str() {
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
        "system.info" => system_info(),
        "system.getenv" => system_getenv(params),
        "system.expand_path" => system_expand_path(params),
        "system.statvfs" => system_statvfs(params),
        "system.groups" => system_groups(),

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
        "watch.add" => crate::watcher::handle_add(params),
        "watch.remove" => crate::watcher::handle_remove(params),
        "watch.list" => crate::watcher::handle_list(params),

        // Note: "batch" is NOT allowed in batch (no recursion)
        _ => Err(RpcError::method_not_found(&method)),
    };

    match result {
        Ok(value) => Response::success(id, value),
        Err(error) => Response::error(Some(id), error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn groups_retry_after_erange() {
        let mut calls = 0;
        let groups = supplementary_groups_with(1, |buffer| {
            calls += 1;
            if calls == 1 {
                return Err(std::io::Error::from_raw_os_error(libc::ERANGE));
            }
            assert!(buffer.len() >= 2);
            buffer[..2].copy_from_slice(&[10, 20]);
            Ok(2)
        })
        .expect("retry getgroups after ERANGE");

        assert_eq!(groups, vec![10, 20]);
        assert_eq!(calls, 2);
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

        let oversized = msgpack_map! {
            "results" => Value::Array(vec![Value::Binary(vec![0; 64])]),
        };
        let frame_error = validate_batch_response_size(&oversized, 32)
            .expect_err("oversized serialized response should be rejected");
        assert_eq!(frame_error.code, RpcError::INTERNAL_ERROR);
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
