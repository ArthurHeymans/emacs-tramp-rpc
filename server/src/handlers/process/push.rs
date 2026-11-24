// SPDX-License-Identifier: GPL-3.0-or-later

//! Server-pushed managed-process output notifications.

use crate::WriterHandle;
use crate::msgpack_map;
use crate::output::encode_framed;
use crate::protocol::{Notification, RpcError, exit_fields};
use rmpv::Value;
use std::process::ExitStatus;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use tokio::sync::Notify;
use tokio::task::JoinHandle;

use super::pipe::{read_output_into, terminate_pipe_process};
use super::pty::{read_pty_into, terminate_pty_process, wait_for_pty_readable};
use super::{ProcessBackend, registry};

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
    let framed = encode_framed(&notification)
        .map_err(|e| RpcError::internal_error(format!("Failed to encode notification: {e}")))?;
    writer
        .write_prefixed_frame(&framed)
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
    // Reuse read scratch buffers across iterations to avoid allocating
    // 2 * 64 KiB on every idle read cycle, and pass `wake` so stop returns
    // promptly instead of waiting out the full 200 ms timeout.
    tokio::spawn(async move {
        let mut stdout_buf = vec![0u8; PUSH_READ_MAX_BYTES];
        let mut stderr_buf = vec![0u8; PUSH_READ_MAX_BYTES];

        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_output_into(
                pid,
                &mut stdout_buf,
                &mut stderr_buf,
                PUSH_READ_TIMEOUT_MS,
                Some(&wake),
            )
            .await
            else {
                // The client treats the exit as final, so the child must not
                // outlive it.
                let shared = registry()
                    .lock()
                    .get(&pid)
                    .map(|managed| Arc::clone(&managed.shared_exit_status));
                let _ = terminate_pipe_process(pid, libc::SIGKILL, false).await;
                registry().lock().remove(&pid);
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
                if send_process_notification(
                    "process.output",
                    msgpack_map! {
                        "pid" => pid,
                        "stdout" => bytes_or_nil(result.stdout),
                        "stderr" => bytes_or_nil(result.stderr)
                    },
                )
                .await
                .is_err()
                {
                    // Transport stdout is gone; stop pushing rather than
                    // spinning on broken-pipe writes.
                    break;
                }
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
        let mut buf = vec![0u8; PUSH_READ_MAX_BYTES];
        // Set when readiness was reported but the read has not yet found data.
        let mut woke_readable = false;

        while !stop.load(Ordering::Acquire) {
            let Ok(result) = read_pty_into(pid, &mut buf).await else {
                // The client treats the exit as final, so the child must not
                // outlive it.
                let shared = registry()
                    .lock()
                    .get(&pid)
                    .map(|managed| Arc::clone(&managed.shared_exit_status));
                let _ = terminate_pty_process(pid, libc::SIGKILL, true, true).await;
                registry().lock().remove(&pid);
                let status = shared.and_then(|status| *status.lock().expect("shared exit status"));
                send_exit_notification(pid, status).await;
                break;
            };
            if result.pending {
                // After a slave close Tokio keeps the master's read-closed
                // readiness, which `WouldBlock` does not clear.  A reopened
                // but silent slave would then report readiness at once on
                // every wait, so back off when a wake yielded no data.
                if std::mem::take(&mut woke_readable) {
                    if !wait_for_idle_push(&stop, &wake).await {
                        break;
                    }
                    continue;
                }
                let wait_outcome = tokio::select! {
                    _ = wake.notified() => break,
                    outcome = wait_for_pty_readable(pid) => outcome,
                };
                match wait_outcome {
                    Ok(true) => {
                        woke_readable = true;
                        continue;
                    }
                    // Entry disappeared while waiting; stop immediately instead
                    // of tight-spinning.
                    Ok(false) => break,
                    Err(_) => {
                        if !wait_for_idle_push(&stop, &wake).await {
                            break;
                        }
                        continue;
                    }
                }
            }
            woke_readable = false;
            let idle = result.bytes_read == 0;
            if !idle {
                if send_process_notification(
                    "process.output",
                    msgpack_map! {
                        "pid" => pid,
                        "stdout" => Value::Binary(buf[..result.bytes_read].to_vec())
                    },
                )
                .await
                .is_err()
                {
                    break;
                }
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
    let mut processes = registry().lock();
    let Some(managed) = processes.get_mut(&pid) else {
        return;
    };
    if managed.terminating || managed.output_push.is_some() {
        return;
    }
    let push = match managed.backend {
        ProcessBackend::Pipe(_) => new_pipe_push(pid),
        ProcessBackend::Pty(_) => new_pty_push(pid),
    };
    managed.output_push = Some(push);
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
