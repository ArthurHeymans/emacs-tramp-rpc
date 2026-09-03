// SPDX-License-Identifier: GPL-3.0-or-later

//! Request handlers for TRAMP-RPC operations

pub mod commands;
pub mod dir;
pub mod file;
pub mod io;
pub mod process;
pub mod system;

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
        "system.info" => system::system_info().await,
        "system.getenv" => system::system_getenv(params),
        "system.expand_path" => system::system_expand_path(params).await,
        "system.statvfs" => system::system_statvfs(params).await,
        "system.groups" => system::system_groups().await,

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
