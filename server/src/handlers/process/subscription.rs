// SPDX-License-Identifier: GPL-3.0-or-later

//! Server-pushed managed-process output notifications.

use crate::WriterHandle;
use crate::msgpack_map;
use crate::protocol::{Notification, RpcError, exit_fields, from_value};
use rmpv::Value;
use serde::Deserialize;
use std::process::ExitStatus;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::io::AsyncWriteExt;
use tokio::sync::Notify;
use tokio::task::JoinHandle;

use super::super::HandlerResult;
use super::pipe::{get_process_map, read_output, terminate_pipe_process};
use super::pty::{get_pty_process_map, read_pty_now, terminate_pty_process, wait_for_pty_readable};

const PUSH_READ_MAX_BYTES: usize = 65_536;
const PUSH_READ_TIMEOUT_MS: u64 = 200;
const _: () = assert!(PUSH_READ_MAX_BYTES <= 64 * 1024);
const _: () = assert!(PUSH_READ_MAX_BYTES < crate::MAX_FRAME_SIZE);

static PROCESS_NOTIFICATION_WRITER: OnceLock<WriterHandle> = OnceLock::new();

pub(super) struct PushSubscription {
    stop: Arc<AtomicBool>,
    wake: Arc<Notify>,
    pub(super) task: JoinHandle<()>,
}

pub(super) async fn stop_push_subscription(subscription: PushSubscription) {
    subscription.stop.store(true, Ordering::Release);
    subscription.wake.notify_one();
    let _ = subscription.task.await;
}

pub fn init_notification_writer(writer: WriterHandle) {
    let _ = PROCESS_NOTIFICATION_WRITER.set(writer);
}

pub(super) async fn send_process_notification(method: &str, params: Value) -> Result<(), RpcError> {
    let writer = PROCESS_NOTIFICATION_WRITER
        .get()
        .cloned()
        .ok_or_else(|| RpcError::internal_error("Process notification writer not initialized"))?;
    let notification = Notification::new(method, params);
    let bytes = rmp_serde::to_vec_named(&notification)
        .map_err(|e| RpcError::internal_error(format!("Failed to encode notification: {e}")))?;
    let mut writer = writer.lock().await;
    writer
        .write_all(&(bytes.len() as u32).to_be_bytes())
        .await
        .map_err(|e| {
            RpcError::internal_error(format!("Failed to write notification length: {e}"))
        })?;
    writer.write_all(&bytes).await.map_err(|e| {
        RpcError::internal_error(format!("Failed to write notification payload: {e}"))
    })?;
    writer
        .flush()
        .await
        .map_err(|e| RpcError::internal_error(format!("Failed to flush notification: {e}")))
}

/// Send a terminal `process.exit`/`process.pty_exit` notification for PID.
/// STATUS is `None` when the remote status is unknown.
pub(super) async fn send_exit_notification(method: &str, pid: u32, status: Option<ExitStatus>) {
    let mut pairs = vec![(Value::String("pid".into()), Value::from(pid))];
    pairs.extend(exit_fields(status));
    let _ = send_process_notification(method, Value::Map(pairs)).await;
}

fn spawn_pipe_subscription(pid: u32, stop: Arc<AtomicBool>) -> JoinHandle<()> {
    // A pipe read is deliberately allowed to finish after stop is requested:
    // cancelling it after it consumed bytes could lose output.
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_output(pid, PUSH_READ_MAX_BYTES, PUSH_READ_TIMEOUT_MS).await
            else {
                // Read failed; kill and remove the child so it does not linger.
                let _ = terminate_pipe_process(pid, libc::SIGKILL, false).await;
                send_exit_notification("process.exit", pid, None).await;
                break;
            };

            if !result.stdout.is_empty() || !result.stderr.is_empty() {
                let bytes_or_nil = |data: Vec<u8>| {
                    if data.is_empty() {
                        Value::Nil
                    } else {
                        Value::Binary(data)
                    }
                };
                let _ = send_process_notification(
                    "process.output",
                    msgpack_map! {
                        "pid" => pid,
                        "stdout" => bytes_or_nil(result.stdout),
                        "stderr" => bytes_or_nil(result.stderr)
                    },
                )
                .await;
                tokio::task::yield_now().await;
            }

            if result.exited {
                send_exit_notification("process.exit", pid, result.exit).await;
                break;
            }
        }
    })
}

fn spawn_pty_subscription(pid: u32, stop: Arc<AtomicBool>, wake: Arc<Notify>) -> JoinHandle<()> {
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_pty_now(pid, PUSH_READ_MAX_BYTES).await else {
                // Read failed; kill and remove the PTY so it does not linger.
                let _ = terminate_pty_process(pid, libc::SIGKILL, true, true, false).await;
                send_exit_notification("process.pty_exit", pid, None).await;
                break;
            };
            if result.pending {
                tokio::select! {
                    _ = wake.notified() => break,
                    _ = wait_for_pty_readable(pid) => {}
                }
                continue;
            }
            if !result.output.is_empty() {
                let _ = send_process_notification(
                    "process.pty_output",
                    msgpack_map! {
                        "pid" => pid,
                        "output" => Value::Binary(result.output)
                    },
                )
                .await;
                tokio::task::yield_now().await;
            }
            if result.exited {
                send_exit_notification("process.pty_exit", pid, result.exit).await;
                break;
            }
        }
    })
}

pub(super) fn new_pipe_subscription(pid: u32) -> PushSubscription {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_pipe_subscription(pid, Arc::clone(&stop));
    PushSubscription { stop, wake, task }
}

pub(super) fn new_pty_subscription(pid: u32) -> PushSubscription {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_pty_subscription(pid, Arc::clone(&stop), Arc::clone(&wake));
    PushSubscription { stop, wake, task }
}

pub async fn subscribe(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let mut processes = get_process_map().lock().await;
    let managed = processes
        .get_mut(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
    if managed.terminating {
        return Err(RpcError::process_error(format!(
            "Process is terminating: {}",
            params.pid
        )));
    }
    if managed.push_subscription.is_none() {
        managed.push_subscription = Some(new_pipe_subscription(params.pid));
    }
    managed.subscription_requested = true;
    Ok(Value::Boolean(true))
}

pub async fn unsubscribe(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let subscription = {
        let mut processes = get_process_map().lock().await;
        let managed = processes
            .get_mut(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        managed.subscription_requested = false;
        managed.push_subscription.take()
    };
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    Ok(Value::Boolean(true))
}

pub async fn subscribe_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let mut processes = get_pty_process_map().lock().await;
    let managed = processes
        .get_mut(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("PTY process not found: {}", params.pid)))?;
    if managed.terminating {
        return Err(RpcError::process_error(format!(
            "PTY process is terminating: {}",
            params.pid
        )));
    }
    if managed.push_subscription.is_none() {
        managed.push_subscription = Some(new_pty_subscription(params.pid));
    }
    managed.subscription_requested = true;
    Ok(Value::Boolean(true))
}

pub async fn unsubscribe_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let subscription = {
        let mut processes = get_pty_process_map().lock().await;
        let managed = processes.get_mut(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        managed.subscription_requested = false;
        managed.push_subscription.take()
    };
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    Ok(Value::Boolean(true))
}
