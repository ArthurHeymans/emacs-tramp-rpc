// SPDX-License-Identifier: GPL-3.0-or-later

//! Request handlers for TRAMP-RPC operations

pub mod commands;
pub mod dir;
pub mod file;
pub mod io;
pub mod process;
pub mod system;

use crate::msgpack_map;
use crate::protocol::{
    DispatchOutcome, Method, PostSendAction, Request, Response, RpcError, encoded_msgpack_len,
    from_value,
};
use futures::{StreamExt, TryStreamExt};
use rmpv::Value;

pub type HandlerResult = Result<Value, RpcError>;

/// Run a synchronous filesystem or OS operation on Tokio's blocking pool,
/// mapping join failures into an `RpcError::internal_error`.
pub(crate) async fn run_blocking<T, F>(label: &'static str, f: F) -> Result<T, RpcError>
where
    F: FnOnce() -> Result<T, RpcError> + Send + 'static,
    T: Send + 'static,
{
    tokio::task::spawn_blocking(f)
        .await
        .map_err(|e| RpcError::internal_error(format!("{label} task join error: {e}")))?
}

/// Dispatch a request to the appropriate handler.
pub async fn dispatch(request: Request) -> DispatchOutcome {
    let Request {
        id, method, params, ..
    } = request;

    match method {
        // Handle batch separately (it needs special handling and can't recurse)
        Method::Batch => {
            let result = batch_execute(params).await;
            let response = match result {
                Ok(value) => Response::success(id.clone(), value),
                Err(error) => Response::error(Some(id.clone()), error),
            };
            let response = match validate_batch_response_size(&response, crate::MAX_FRAME_SIZE) {
                Ok(()) => response,
                Err(error) => Response::error(Some(id), error),
            };
            DispatchOutcome::new(response)
        }
        // Process-start methods arm server-pushed notifications only after the
        // start response has been written to stdout.
        Method::ProcessStart => match process::start_outcome(params).await {
            Ok((value, pid)) => DispatchOutcome::with_post_send(
                Response::success(id, value),
                PostSendAction::StartOutputPush(pid),
            ),
            Err(error) => DispatchOutcome::new(Response::error(Some(id), error)),
        },
        Method::ProcessStartPty => match process::start_pty_outcome(params).await {
            Ok((value, pid)) => DispatchOutcome::with_post_send(
                Response::success(id, value),
                PostSendAction::StartOutputPush(pid),
            ),
            Err(error) => DispatchOutcome::new(Response::error(Some(id), error)),
        },
        other => {
            let response = match route(other, params).await {
                Ok(value) => Response::success(id, value),
                Err(error) => Response::error(Some(id), error),
            };
            DispatchOutcome::new(response)
        }
    }
}

/// Execute a typed post-send side effect after the response has been written on
/// the wire (e.g., arming output push for a newly started managed process).
pub async fn run_post_send(action: PostSendAction) {
    match action {
        PostSendAction::StartOutputPush(pid) => process::start_output_push(pid).await,
    }
}

/// Signal and reap managed async children before a connection task exits.
pub async fn cleanup_managed_processes() -> Result<(), RpcError> {
    process::cleanup_managed_processes().await
}

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
    let encoded_size = encoded_msgpack_len(response).map_err(|error| {
        RpcError::internal_error(format!("Failed to encode batch response: {error}"))
    })?;
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
        method: Method,
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
        // A started process pushes output only after its top-level
        // response; inside a batch it would stay silent forever.
        let routed = match req.method {
            Method::ProcessStart => Err(RpcError::invalid_params(
                "process.start must be sent as its own request",
            )),
            Method::ProcessStartPty => Err(RpcError::invalid_params(
                "process.start_pty must be sent as its own request",
            )),
            Method::Batch => Err(RpcError::method_not_found("batch")),
            method => route(method, req.params).await,
        };
        let result = match routed {
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
            let result_size = encoded_msgpack_len(&result).map_err(|error| {
                RpcError::internal_error(format!("Failed to encode batch entry: {error}"))
            })?;
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

/// Route a method to its handler.  Shared by single requests and batch
/// subrequests.
async fn route(method: Method, params: Value) -> HandlerResult {
    match method {
        // File metadata operations
        Method::FileStat => file::stat(params).await,
        Method::FileAccess => file::access(params).await,
        Method::FileTruename => file::truename(params).await,

        // Directory operations
        Method::DirList => dir::list(params).await,
        Method::DirCreate => dir::create(params).await,
        Method::DirRemove => dir::remove(params).await,

        // File I/O operations
        Method::FileRead => io::read(params).await,
        Method::FileWrite => io::write(params).await,
        Method::FileCopy => io::copy(params).await,
        Method::FileRename => io::rename(params).await,
        Method::FileDelete => io::delete(params).await,
        Method::FileSetModes => io::set_modes(params).await,
        Method::FileSetTimes => io::set_times(params).await,
        Method::FileMakeSymlink => io::make_symlink(params).await,
        Method::FileMakeHardlink => io::make_hardlink(params).await,
        Method::FileChown => io::chown(params).await,

        // Process operations
        Method::ProcessRun => process::run(params).await,
        Method::ProcessStart => process::start(params).await,
        Method::ProcessWrite => process::write(params).await,
        Method::ProcessCloseStdin => process::close_stdin(params).await,
        Method::ProcessKill => process::kill(params).await,
        Method::ProcessSignal => process::signal_pid(params).await,

        // PTY (pseudo-terminal) process operations
        Method::ProcessStartPty => process::start_pty(params).await,
        Method::ProcessWritePty => process::write_pty(params).await,
        Method::ProcessResizePty => process::resize_pty(params).await,
        Method::ProcessKillPty => process::kill_pty(params).await,
        Method::ProcessClosePty => process::close_pty(params).await,

        // System info and a lightweight, reserved-slot connection probe.
        Method::SystemPing => Ok(Value::Boolean(true)),
        Method::SystemInfo => system::system_info().await,
        Method::SystemStatvfs => system::system_statvfs(params).await,
        Method::SystemUserHome => system::system_user_home(params).await,

        // Parallel command execution and ancestor scanning
        Method::CommandsRunParallel => commands::run_parallel(params).await,
        Method::AncestorsScan => commands::ancestors_scan(params).await,
        Method::HighlevelTestFilesInDir => commands::highlevel_test_files_in_dir(params).await,
        Method::HighlevelLocateDominatingFileMulti => {
            commands::highlevel_locate_dominating_file_multi(params).await
        }
        Method::HighlevelDirLocalsFindFileCacheUpdate => {
            commands::highlevel_dir_locals_find_file_cache_update(params).await
        }

        // Filesystem watch operations (for cache invalidation)
        Method::WatchAdd => crate::watcher::handle_add(params).await,
        Method::WatchRemove => crate::watcher::handle_remove(params).await,

        // Note: "batch" is handled at top level only (no recursion)
        Method::Batch => Err(RpcError::method_not_found("batch")),
        Method::Unknown(name) => Err(RpcError::method_not_found(&name)),
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
        let tmp = tempfile::tempdir().expect("create tempdir");
        let dir = tmp.path().canonicalize().expect("canonical tempdir");
        for index in 0..MAX_BATCH_ENTRIES {
            std::fs::write(dir.join(format!("batch-order-{index}")), b"").unwrap();
        }
        let result = batch_execute(msgpack_map! {
            "requests" => Value::Array(
                (0..MAX_BATCH_ENTRIES)
                    .map(|index| msgpack_map! {
                        "method" => "file.truename",
                        "params" => msgpack_map! {
                            "path" => dir.join(format!("batch-order-{index}")).to_str().unwrap(),
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
                .and_then(|(_, value)| value.as_slice())
                .expect("ordered batch result");
            assert!(value.ends_with(format!("/batch-order-{index}").as_bytes()));
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
        let inner_size = encoded_msgpack_len(&result).unwrap();
        let frame_size = encoded_msgpack_len(&response).unwrap();
        assert!(frame_size > inner_size);
        let frame_error = validate_batch_response_size(&response, inner_size)
            .expect_err("response envelope must count toward the frame limit");
        assert_eq!(frame_error.code, RpcError::INTERNAL_ERROR);
        validate_batch_response_size(&response, frame_size)
            .expect("a response exactly at the frame limit is legal");
    }

    #[tokio::test]
    async fn access_decisions_are_results_in_single_and_batch_dispatch() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("file");
        std::fs::write(&file, "contents").unwrap();
        let mut requests = Vec::new();
        let mut expected = Vec::new();
        for path in [file.clone(), tmp.path().join("missing"), file.join("child")] {
            let params = msgpack_map! {
                "path" => Value::Binary(path.as_os_str().as_bytes().to_vec()),
                "mode" => "r",
            };
            expected.push(route(Method::FileAccess, params.clone()).await.unwrap());
            requests.push(msgpack_map! { "method" => "file.access", "params" => params });
        }
        let params = msgpack_map! {
            "path" => Value::Binary(tmp.path().join("missing").as_os_str().as_bytes().to_vec()),
            "mode" => "w",
            "check_parent_if_missing" => true,
        };
        expected.push(route(Method::FileAccess, params.clone()).await.unwrap());
        requests.push(msgpack_map! { "method" => "file.access", "params" => params });
        let batch = batch_execute(msgpack_map! { "requests" => Value::Array(requests) })
            .await
            .unwrap();
        let batch: std::collections::HashMap<
            String,
            Vec<std::collections::HashMap<String, Value>>,
        > = from_value(batch).unwrap();
        for (entry, decision) in batch["results"].iter().zip(&expected) {
            assert_eq!(entry.get("result"), Some(decision));
            assert!(!entry.contains_key("error"));
        }
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
