// SPDX-License-Identifier: GPL-3.0-or-later

//! Server-pushed managed-process output notifications.

use crate::WriterHandle;
use crate::msgpack_map;
use crate::protocol::{Notification, RpcError, exit_fields};
use rmpv::Value;
use std::process::ExitStatus;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::Notify;
use tokio::task::JoinHandle;

use super::pipe::{get_process_map, read_output, terminate_pipe_process};
use super::pty::{get_pty_process_map, read_pty_now, terminate_pty_process, wait_for_pty_readable};

const PUSH_READ_MAX_BYTES: usize = 65_536;
const PUSH_READ_TIMEOUT_MS: u64 = 200;
const PUSH_IDLE_WAIT: std::time::Duration = std::time::Duration::from_millis(20);
const _: () = assert!(PUSH_READ_MAX_BYTES <= 64 * 1024);
const _: () = assert!(PUSH_READ_MAX_BYTES < crate::MAX_FRAME_SIZE);

static PROCESS_NOTIFICATION_WRITER: OnceLock<WriterHandle> = OnceLock::new();

pub(super) struct OutputPush {
    stop: Arc<AtomicBool>,
    wake: Arc<Notify>,
    pub(super) task: JoinHandle<()>,
}

pub(super) async fn stop_output_push(push: OutputPush) {
    push.stop.store(true, Ordering::Release);
    push.wake.notify_one();
    let _ = push.task.await;
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
    writer
        .write_frame(&bytes)
        .await
        .map_err(|e| RpcError::internal_error(format!("Failed to write notification: {e}")))
}

/// Send the terminal `process.exit` notification for PID.
/// STATUS is `None` when the remote status is unknown.
pub(super) async fn send_exit_notification(pid: u32, status: Option<ExitStatus>) {
    let mut pairs = vec![(Value::String("pid".into()), Value::from(pid))];
    pairs.extend(exit_fields(status));
    let _ = send_process_notification("process.exit", Value::Map(pairs)).await;
}

/// EOF need not mean child exit: it can close its output and keep running.
/// Back off between empty reads without delaying an explicit push stop.
async fn wait_for_idle_push(stop: &AtomicBool, wake: &Notify) -> bool {
    if stop.load(Ordering::Acquire) {
        return false;
    }
    tokio::select! {
        _ = wake.notified() => false,
        _ = tokio::time::sleep(PUSH_IDLE_WAIT) => !stop.load(Ordering::Acquire),
    }
}

fn spawn_pipe_push(pid: u32, stop: Arc<AtomicBool>, wake: Arc<Notify>) -> JoinHandle<()> {
    // A pipe read is deliberately allowed to finish after stop is requested:
    // cancelling it after it consumed bytes could lose output.
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_output(pid, PUSH_READ_MAX_BYTES, PUSH_READ_TIMEOUT_MS).await
            else {
                // The client treats the exit as final, so the child must not
                // outlive it.
                let shared = get_process_map()
                    .lock()
                    .await
                    .get(&pid)
                    .map(|managed| Arc::clone(&managed.shared_exit_status));
                let _ = terminate_pipe_process(pid, libc::SIGKILL, false).await;
                get_process_map().lock().await.remove(&pid);
                let status = shared.and_then(|status| *status.lock().expect("shared exit status"));
                send_exit_notification(pid, status).await;
                break;
            };

            let idle = result.stdout.is_empty() && result.stderr.is_empty();
            if !idle {
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
                send_exit_notification(pid, result.exit).await;
                break;
            }
            if idle && !wait_for_idle_push(&stop, &wake).await {
                break;
            }
        }
    })
}

fn spawn_pty_push(pid: u32, stop: Arc<AtomicBool>, wake: Arc<Notify>) -> JoinHandle<()> {
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_pty_now(pid, PUSH_READ_MAX_BYTES).await else {
                // The client treats the exit as final, so the child must not
                // outlive it.
                let shared = get_pty_process_map()
                    .lock()
                    .await
                    .get(&pid)
                    .map(|managed| Arc::clone(&managed.shared_exit_status));
                let _ = terminate_pty_process(pid, libc::SIGKILL, true, true).await;
                get_pty_process_map().lock().await.remove(&pid);
                let status = shared.and_then(|status| *status.lock().expect("shared exit status"));
                send_exit_notification(pid, status).await;
                break;
            };
            if result.pending {
                tokio::select! {
                    _ = wake.notified() => break,
                    _ = wait_for_pty_readable(pid) => {}
                }
                continue;
            }
            let idle = result.output.is_empty();
            if !idle {
                let _ = send_process_notification(
                    "process.output",
                    msgpack_map! {
                        "pid" => pid,
                        "stdout" => Value::Binary(result.output)
                    },
                )
                .await;
                tokio::task::yield_now().await;
            }
            if result.exited {
                send_exit_notification(pid, result.exit).await;
                break;
            }
            if idle && !wait_for_idle_push(&stop, &wake).await {
                break;
            }
        }
    })
}

pub(super) fn new_pipe_push(pid: u32) -> OutputPush {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_pipe_push(pid, Arc::clone(&stop), Arc::clone(&wake));
    OutputPush { stop, wake, task }
}

pub(super) fn new_pty_push(pid: u32) -> OutputPush {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_pty_push(pid, Arc::clone(&stop), Arc::clone(&wake));
    OutputPush { stop, wake, task }
}

/// Start pushing output and exit notifications for the newly started PID.
///
/// Runs once the start response has been written, so a notification for a
/// PID never precedes the response that announces it.  PIDs come from one
/// counter shared by pipe and PTY processes.
pub async fn start_output_push(pid: u32) {
    if let Some(managed) = get_process_map().lock().await.get_mut(&pid) {
        if !managed.terminating && managed.output_push.is_none() {
            managed.output_push = Some(new_pipe_push(pid));
        }
        return;
    }
    if let Some(managed) = get_pty_process_map().lock().await.get_mut(&pid)
        && !managed.terminating
        && managed.output_push.is_none()
    {
        managed.output_push = Some(new_pty_push(pid));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn idle_push_backs_off_and_stop_wakes_it() {
        let stop = AtomicBool::new(false);
        let wake = Notify::new();
        let before = tokio::time::Instant::now();
        assert!(wait_for_idle_push(&stop, &wake).await);
        assert!(before.elapsed() >= PUSH_IDLE_WAIT);

        let wait = wait_for_idle_push(&stop, &wake);
        tokio::pin!(wait);
        assert!(futures::poll!(&mut wait).is_pending());
        stop.store(true, Ordering::Release);
        wake.notify_one();
        assert_eq!(futures::poll!(&mut wait), std::task::Poll::Ready(false));
        assert!(!wait_for_idle_push(&stop, &wake).await);
    }
}
