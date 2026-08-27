//! TRAMP-RPC Server
//!
//! A MessagePack-RPC server for TRAMP remote file access.
//! Communicates over stdin/stdout using length-prefixed MessagePack messages.
//!
//! Protocol framing:
//!   <4-byte big-endian length><msgpack payload>
//!
//! Uses tokio for async concurrent request processing - multiple requests
//! can be processed in parallel while waiting on I/O.

mod handlers;
mod protocol;
mod watcher;

use protocol::{Request, RequestId, Response, RpcError};
use rmpv::Value;
use std::collections::VecDeque;
use std::io::Cursor;
use std::sync::Arc;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufWriter};
use tokio::sync::{Mutex, OwnedSemaphorePermit, Semaphore, mpsc};
use tokio::task::JoinSet;

pub(crate) const MAX_FRAME_SIZE: usize = 100 * 1024 * 1024;
pub(crate) const MAX_RESPONSE_OUTPUT_BYTES: usize = MAX_FRAME_SIZE - 1024 * 1024;
const FRAME_CHANNEL_SIZE: usize = 2;
const GENERAL_TASK_LIMIT: usize = 16;
const CONTROL_TASK_LIMIT: usize = 4;
const PTY_WRITE_TASK_LIMIT: usize = 16;
/// How many decoded-but-not-yet-started requests are buffered before the
/// connection stops reading frames.  Past this point the bounded frame
/// channel and the OS pipe throttle the client, which is the only
/// backpressure signal it can actually act on -- a synthetic "too busy"
/// error would just surface as a spurious `remote-file-error' in the middle
/// of an ordinary file operation.
const DEFERRED_REQUEST_LIMIT: usize = 64;
const ERROR_RESPONSE_CHANNEL_SIZE: usize = 16;
const EOF_TASK_JOIN_WAIT: std::time::Duration = std::time::Duration::from_millis(500);

#[cfg(test)]
struct CleanupBarrier {
    first_pass_complete: tokio::sync::Notify,
    continue_cleanup: tokio::sync::Notify,
}

/// Shared handle to the stdout writer, used by both response writing
/// and the watcher's notification sending.
pub type WriterHandle = Arc<Mutex<BufWriter<tokio::io::Stdout>>>;

async fn read_frames<R>(
    mut stdin: R,
    sender: mpsc::Sender<Vec<u8>>,
    errors: mpsc::Sender<Response>,
    max_frame_size: usize,
) where
    R: AsyncRead + Unpin,
{
    loop {
        let mut len_buf = [0u8; 4];
        if stdin.read_exact(&mut len_buf).await.is_err() {
            break;
        }
        let len = u32::from_be_bytes(len_buf) as usize;

        // Answer and drain oversized frames so that a following frame remains
        // aligned and the client is not left waiting on its own timeout.
        if len > max_frame_size {
            if errors
                .send(Response::error(
                    None,
                    RpcError::invalid_request(format!(
                        "Request frame exceeds {max_frame_size} byte limit"
                    )),
                ))
                .await
                .is_err()
            {
                return;
            }
            let mut discard = [0u8; 8192];
            let mut remaining = len;
            while remaining > 0 {
                let amount = remaining.min(discard.len());
                if stdin.read_exact(&mut discard[..amount]).await.is_err() {
                    return;
                }
                remaining -= amount;
            }
            continue;
        }

        let mut payload = vec![0u8; len];
        if stdin.read_exact(&mut payload).await.is_err() {
            break;
        }
        if sender.send(payload).await.is_err() {
            break;
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TaskClass {
    General,
    Control,
    PtyWrite,
}

struct Admissions {
    general: Arc<Semaphore>,
    control: Arc<Semaphore>,
    pty_write: Arc<Semaphore>,
}

impl Admissions {
    #[cfg(test)]
    fn try_acquire(&self, class: TaskClass) -> Option<OwnedSemaphorePermit> {
        self.try_acquire_many(class, 1)
    }

    fn semaphore(&self, class: TaskClass) -> &Arc<Semaphore> {
        match class {
            TaskClass::General => &self.general,
            TaskClass::Control => &self.control,
            TaskClass::PtyWrite => &self.pty_write,
        }
    }

    fn available_permits(&self, class: TaskClass) -> usize {
        self.semaphore(class).available_permits()
    }

    fn try_acquire_many(&self, class: TaskClass, permits: usize) -> Option<OwnedSemaphorePermit> {
        Arc::clone(self.semaphore(class))
            .try_acquire_many_owned(u32::try_from(permits).ok()?)
            .ok()
    }
}

impl Default for Admissions {
    fn default() -> Self {
        Self {
            general: Arc::new(Semaphore::new(GENERAL_TASK_LIMIT)),
            control: Arc::new(Semaphore::new(CONTROL_TASK_LIMIT)),
            pty_write: Arc::new(Semaphore::new(PTY_WRITE_TASK_LIMIT)),
        }
    }
}

fn task_class(method: &str) -> TaskClass {
    // These operations only signal or close a process.  Keep them available
    // while long-running general requests consume their slots.
    match method {
        "process.kill"
        | "process.signal"
        | "process.close_stdin"
        | "process.kill_pty"
        | "process.close_pty" => TaskClass::Control,
        // PTY writes can remain blocked until the remote program reads input.
        // Isolate them so they cannot consume every general request permit;
        // lifecycle operations retain their separately reserved control slots.
        "process.write_pty" => TaskClass::PtyWrite,
        _ => TaskClass::General,
    }
}

fn request_permit_count(method: &str) -> usize {
    if method == "batch" {
        handlers::BATCH_CONCURRENCY
    } else {
        1
    }
}

async fn write_response<W>(writer: &Arc<Mutex<W>>, response: &Response)
where
    W: AsyncWrite + Unpin,
{
    let Ok(mut msgpack_bytes) = rmp_serde::to_vec_named(response) else {
        return;
    };
    if msgpack_bytes.len() > MAX_FRAME_SIZE {
        let oversized = Response::error(
            response.id.clone(),
            RpcError::internal_error("Response exceeds maximum frame size"),
        );
        let Ok(encoded_error) = rmp_serde::to_vec_named(&oversized) else {
            return;
        };
        msgpack_bytes = encoded_error;
    }
    let mut writer = writer.lock().await;
    let len_bytes = (msgpack_bytes.len() as u32).to_be_bytes();
    let _ = writer.write_all(&len_bytes).await;
    let _ = writer.write_all(&msgpack_bytes).await;
    let _ = writer.flush().await;
}

fn spawn_request<W>(
    tasks: &mut JoinSet<()>,
    request: Request,
    permit: OwnedSemaphorePermit,
    writer: &Arc<Mutex<W>>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    let writer = Arc::clone(writer);
    tasks.spawn(async move {
        let _permit = permit;
        #[cfg(test)]
        let panic_after_response = request.method == "test.panic";
        let response = handlers::dispatch(request).await;
        write_response(&writer, &response).await;
        #[cfg(test)]
        if panic_after_response {
            panic!("test request task panic");
        }
    });
}

async fn run_connection<R, W>(
    reader: R,
    writer: Arc<Mutex<W>>,
    #[cfg(test)] cleanup_barrier: Option<Arc<CleanupBarrier>>,
) -> Result<(), RpcError>
where
    R: AsyncRead + Unpin + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
{
    // Protocol-level failures are answered by a dedicated writer so they can
    // never be dropped: a missing response leaves the client blocked until
    // its own timeout with no indication of what went wrong.
    let (errors, mut error_responses) = mpsc::channel::<Response>(ERROR_RESPONSE_CHANNEL_SIZE);
    let error_writer = tokio::spawn({
        let writer = Arc::clone(&writer);
        async move {
            while let Some(response) = error_responses.recv().await {
                write_response(&writer, &response).await;
            }
        }
    });

    let (sender, mut frames) = mpsc::channel(FRAME_CHANNEL_SIZE);
    let frame_reader = tokio::spawn(read_frames(reader, sender, errors.clone(), MAX_FRAME_SIZE));

    let mut tasks: JoinSet<()> = JoinSet::new();
    let admissions = Admissions::default();
    let mut deferred: VecDeque<Request> = VecDeque::new();
    let mut general_bypass_budget = None;

    loop {
        start_admissible(
            &mut deferred,
            &mut tasks,
            &writer,
            &admissions,
            &mut general_bypass_budget,
        );
        // Stop pulling frames while the backlog is full.  The bounded frame
        // channel then stops draining the pipe, which is the throttle the
        // client can actually observe.
        let accepting = deferred.len() < DEFERRED_REQUEST_LIMIT;

        if tasks.is_empty() {
            if !accepting {
                break;
            }
            let Some(payload) = frames.recv().await else {
                break;
            };
            accept_frame(
                payload,
                &mut deferred,
                &admissions,
                &mut tasks,
                &writer,
                &errors,
            )
            .await;
        } else {
            tokio::select! {
                Some(_) = tasks.join_next() => {}
                payload = frames.recv(), if accepting => match payload {
                    Some(payload) => {
                        accept_frame(
                            payload, &mut deferred, &admissions, &mut tasks, &writer, &errors,
                        )
                        .await;
                    }
                    // The transport is gone, so stop accepting frames.  The
                    // shutdown path below still joins in-flight requests.
                    None => break,
                },
            }
        }
    }

    // Once the transport is gone no response can be relied on.  Reap managed
    // children before joining request tasks: a task blocked on their pipes or
    // a descendant-held descriptor must not prevent SIGKILL escalation.
    // A failed first pass leaves its map entries registered so the final pass
    // can retry after all request tasks have stopped.
    let _ = handlers::cleanup_managed_processes().await;
    drain_tasks_for(&mut tasks, EOF_TASK_JOIN_WAIT).await;
    tasks.abort_all();
    // `spawn_blocking` work cannot be cancelled once it has started.  Do not
    // let such a task hold EOF teardown forever; the second managed-child
    // cleanup below still catches registrations completed before this deadline.
    drain_tasks_for(&mut tasks, EOF_TASK_JOIN_WAIT).await;
    frame_reader.abort();
    // A request can register a child after the first map snapshot while its
    // task is being joined or aborted.  The second pass closes that race.
    #[cfg(test)]
    if let Some(barrier) = cleanup_barrier {
        barrier.first_pass_complete.notify_one();
        tokio::time::timeout(
            std::time::Duration::from_secs(1),
            barrier.continue_cleanup.notified(),
        )
        .await
        .expect("test should release the connection cleanup barrier");
    }
    let cleanup_result = handlers::cleanup_managed_processes().await;
    drop(errors);
    let _ = tokio::time::timeout(EOF_TASK_JOIN_WAIT, error_writer).await;
    cleanup_result
}

async fn drain_tasks_for(tasks: &mut JoinSet<()>, wait: std::time::Duration) {
    let _ = tokio::time::timeout(wait, async { while tasks.join_next().await.is_some() {} }).await;
}

/// Start every queued request that currently fits in its class.
///
/// Requests of different classes do not block one another.  Within the general
/// class, a large request gets a bounded bypass budget equal to the permits
/// that were idle when it first became blocked.  Those permits may serve
/// already-arriving one-permit work once, but are then reserved as they return,
/// preventing both head-of-line idling and indefinite batch starvation.
fn start_admissible<W>(
    deferred: &mut VecDeque<Request>,
    tasks: &mut JoinSet<()>,
    writer: &Arc<Mutex<W>>,
    admissions: &Admissions,
    general_bypass_budget: &mut Option<usize>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    let mut still_deferred = VecDeque::with_capacity(deferred.len());
    let mut general_waiting = false;
    let mut control_blocked = false;
    let mut pty_write_blocked = false;
    while let Some(request) = deferred.pop_front() {
        let class = task_class(&request.method);
        let permit_count = request_permit_count(&request.method);

        if class == TaskClass::General && general_waiting {
            let Some(budget) = general_bypass_budget.as_mut() else {
                still_deferred.push_back(request);
                continue;
            };
            if permit_count > *budget {
                still_deferred.push_back(request);
                continue;
            }
            if let Some(permit) = admissions.try_acquire_many(class, permit_count) {
                *budget -= permit_count;
                spawn_request(tasks, request, permit, writer);
            } else {
                still_deferred.push_back(request);
            }
            continue;
        }

        let blocked = match class {
            TaskClass::General => false,
            TaskClass::Control => control_blocked,
            TaskClass::PtyWrite => pty_write_blocked,
        };
        if blocked {
            still_deferred.push_back(request);
            continue;
        }

        match admissions.try_acquire_many(class, permit_count) {
            Some(permit) => {
                if class == TaskClass::General && permit_count > 1 {
                    *general_bypass_budget = None;
                }
                spawn_request(tasks, request, permit, writer);
            }
            None => {
                if class == TaskClass::General && permit_count > 1 {
                    if general_bypass_budget.is_none() {
                        *general_bypass_budget =
                            Some(admissions.available_permits(TaskClass::General));
                    }
                    general_waiting = true;
                } else {
                    match class {
                        TaskClass::General => general_waiting = true,
                        TaskClass::Control => control_blocked = true,
                        TaskClass::PtyWrite => pty_write_blocked = true,
                    }
                }
                still_deferred.push_back(request);
            }
        }
    }
    *deferred = still_deferred;
}

async fn accept_frame<W>(
    payload: Vec<u8>,
    deferred: &mut VecDeque<Request>,
    admissions: &Admissions,
    tasks: &mut JoinSet<()>,
    writer: &Arc<Mutex<W>>,
    errors: &mpsc::Sender<Response>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    // Decode and validate before spawning.  This lets the frame Vec be freed
    // before a handler awaits, while retaining the request params it needs.
    let request = match decode_request(&payload) {
        Ok(request) => request,
        Err(response) => {
            let _ = errors.send(*response).await;
            return;
        }
    };
    // Never let a newly arrived request bypass an older deferred request in
    // the same class.  Otherwise a stream of one-permit requests can consume
    // each newly freed slot before an older batch can reserve all its permits.
    let class = task_class(&request.method);
    if deferred
        .iter()
        .any(|queued| task_class(&queued.method) == class)
    {
        deferred.push_back(request);
        return;
    }

    // A batch reserves its full subrequest concurrency from the shared general
    // admission bound, so concurrent batches cannot multiply that limit.
    match admissions.try_acquire_many(class, request_permit_count(&request.method)) {
        Some(permit) => spawn_request(tasks, request, permit, writer),
        // Queue rather than reject.  The caller stops reading frames once the
        // queue is full, so the client is throttled instead of being handed a
        // synthetic error for a request it was entitled to make.
        None => deferred.push_back(request),
    }
}

#[tokio::main]
async fn main() -> std::process::ExitCode {
    let stdout: WriterHandle = Arc::new(Mutex::new(BufWriter::new(tokio::io::stdout())));

    // Initialize the filesystem watcher for cache invalidation notifications.
    // If this fails (e.g. inotify not available), we continue without watching.
    // NOTE: Do NOT use eprintln! here or anywhere in the server -- SSH forwards
    // the remote process's stderr over the same pipe to Emacs, where it gets
    // mixed with the binary msgpack protocol on stdout and corrupts framing.
    if let Ok(manager) = watcher::WatchManager::new(Arc::clone(&stdout)) {
        watcher::init(manager);
    }

    match run_connection(
        tokio::io::stdin(),
        stdout,
        #[cfg(test)]
        None,
    )
    .await
    {
        Ok(()) => std::process::ExitCode::SUCCESS,
        // stderr shares the SSH transport with the binary protocol, so report
        // cleanup failure only through the server's nonzero exit status.
        Err(_) => std::process::ExitCode::FAILURE,
    }
}

fn decode_request(payload: &[u8]) -> Result<Request, Box<Response>> {
    // Decode the wire format first, then validate its request shape.  This
    // keeps malformed MessagePack distinct from a valid but invalid request.
    let mut cursor = Cursor::new(payload);
    let value: Value = match rmpv::decode::read_value(&mut cursor) {
        Ok(value) if cursor.position() == payload.len() as u64 => value,
        Ok(_) => {
            return Err(Box::new(Response::error(
                None,
                RpcError::parse_error("trailing MessagePack data"),
            )));
        }
        Err(e) => {
            return Err(Box::new(Response::error(
                None,
                RpcError::parse_error(e.to_string()),
            )));
        }
    };
    let id = request_id_from_value(&value);
    let request: Request = match protocol::from_value(value) {
        Ok(request) => request,
        Err(e) => {
            return Err(Box::new(Response::error(
                id,
                RpcError::invalid_request(e.to_string()),
            )));
        }
    };

    if request.version != "2.0" {
        return Err(Box::new(Response::error(
            Some(request.id),
            RpcError::invalid_request("Invalid RPC version"),
        )));
    }
    Ok(request)
}

fn request_id_from_value(value: &Value) -> Option<RequestId> {
    let id = value
        .as_map()?
        .iter()
        .find_map(|(key, value)| (key.as_str() == Some("id")).then_some(value))?;
    protocol::from_value(id.clone()).ok()
}

#[cfg(test)]
async fn process_request(payload: &[u8]) -> Response {
    match decode_request(payload) {
        Ok(request) => handlers::dispatch(request).await,
        Err(response) => *response,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmpv::Value;

    fn make_request(method: &str, params: Value) -> Vec<u8> {
        make_request_with_id(1, method, params)
    }

    fn make_request_with_id(id: i64, method: &str, params: Value) -> Vec<u8> {
        let request = rmpv::Value::Map(vec![
            (Value::String("version".into()), Value::String("2.0".into())),
            (Value::String("id".into()), Value::Integer(id.into())),
            (Value::String("method".into()), Value::String(method.into())),
            (Value::String("params".into()), params),
        ]);
        rmp_serde::to_vec_named(&request).unwrap()
    }

    #[tokio::test]
    async fn test_abort_joinset_drain_is_bounded_for_blocking_task() {
        let (started_tx, started_rx) = tokio::sync::oneshot::channel();
        let (release_tx, release_rx) = std::sync::mpsc::channel();
        let mut tasks = JoinSet::new();
        tasks.spawn_blocking(move || {
            started_tx.send(()).expect("test should observe task start");
            release_rx
                .recv()
                .expect("test should release blocking task");
        });
        started_rx.await.expect("blocking task should start");

        tasks.abort_all();
        tokio::time::timeout(
            std::time::Duration::from_millis(100),
            drain_tasks_for(&mut tasks, std::time::Duration::from_millis(10)),
        )
        .await
        .expect("aborted JoinSet drain must not wait for blocking work");
        assert_eq!(tasks.len(), 1);

        release_tx.send(()).expect("release blocking task");
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while tasks.join_next().await.is_some() {}
        })
        .await
        .expect("released blocking task should join");
    }

    #[tokio::test]
    async fn test_parse_request() {
        let params = Value::Map(vec![(
            Value::String("path".into()),
            Value::String("/tmp".into()),
        )]);
        let payload = make_request("file.stat", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none());
    }

    #[tokio::test]
    async fn test_invalid_msgpack() {
        let response = process_request(b"not msgpack").await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::PARSE_ERROR);
    }

    #[tokio::test]
    async fn test_structurally_invalid_request_is_invalid_request() {
        let payload = rmp_serde::to_vec_named(&Value::Map(vec![
            (Value::String("version".into()), Value::String("2.0".into())),
            (Value::String("id".into()), Value::Integer(1.into())),
        ]))
        .unwrap();
        let response = process_request(&payload).await;
        assert!(matches!(response.id, Some(RequestId::Number(1))));
        assert_eq!(response.error.unwrap().code, RpcError::INVALID_REQUEST);
    }

    #[tokio::test]
    async fn test_oversized_frame_is_answered_and_drained() {
        let oversized = frame(b"oversized");
        let valid = frame(&[0xc0]);
        let input = [oversized, valid].concat();
        let (frames_tx, mut frames_rx) = mpsc::channel(1);
        let (errors_tx, mut errors_rx) = mpsc::channel(1);

        read_frames(&input[..], frames_tx, errors_tx, 4).await;

        let response = errors_rx.recv().await.expect("oversized frame response");
        assert_eq!(response.error.unwrap().code, RpcError::INVALID_REQUEST);
        assert_eq!(frames_rx.recv().await, Some(vec![0xc0]));
    }

    #[test]
    fn test_batches_reserve_shared_general_admission() {
        let admissions = Admissions::default();
        let mut permits = Vec::new();
        for _ in 0..GENERAL_TASK_LIMIT / handlers::BATCH_CONCURRENCY {
            permits.push(
                admissions
                    .try_acquire_many(TaskClass::General, request_permit_count("batch"))
                    .expect("batch reservation should fit"),
            );
        }
        assert_eq!(admissions.general.available_permits(), 0);
        assert!(
            admissions
                .try_acquire_many(TaskClass::General, request_permit_count("batch"))
                .is_none()
        );
        assert!(admissions.try_acquire(TaskClass::General).is_none());

        permits.pop();
        assert_eq!(
            admissions.general.available_permits(),
            handlers::BATCH_CONCURRENCY
        );
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_blocked_batch_uses_idle_permits_without_unbounded_bypass() {
        let admissions = Admissions::default();
        let _held = admissions
            .try_acquire_many(
                TaskClass::General,
                GENERAL_TASK_LIMIT - handlers::BATCH_CONCURRENCY + 1,
            )
            .expect("hold all but three general permits");
        let mut deferred = VecDeque::from([
            Request {
                version: "2.0".into(),
                id: RequestId::Number(1),
                method: "batch".into(),
                params: Value::Nil,
            },
            Request {
                version: "2.0".into(),
                id: RequestId::Number(2),
                method: "process.status".into(),
                params: Value::Nil,
            },
            Request {
                version: "2.0".into(),
                id: RequestId::Number(3),
                method: "process.status".into(),
                params: Value::Nil,
            },
            Request {
                version: "2.0".into(),
                id: RequestId::Number(4),
                method: "process.status".into(),
                params: Value::Nil,
            },
            Request {
                version: "2.0".into(),
                id: RequestId::Number(5),
                method: "process.status".into(),
                params: Value::Nil,
            },
        ]);
        let writer = Arc::new(Mutex::new(tokio::io::sink()));
        let mut tasks = JoinSet::new();
        let mut bypass_budget = None;

        start_admissible(
            &mut deferred,
            &mut tasks,
            &writer,
            &admissions,
            &mut bypass_budget,
        );

        assert_eq!(tasks.len(), 3);
        assert_eq!(bypass_budget, Some(0));
        assert_eq!(deferred.len(), 2);
        assert_eq!(deferred[0].method, "batch");
        assert!(matches!(&deferred[1].id, RequestId::Number(5)));
        tasks.abort_all();
    }

    #[tokio::test(flavor = "current_thread")]
    async fn test_new_general_requests_share_bounded_batch_bypass_budget() {
        let admissions = Admissions::default();
        let _held = admissions
            .try_acquire_many(
                TaskClass::General,
                GENERAL_TASK_LIMIT - handlers::BATCH_CONCURRENCY + 1,
            )
            .expect("hold all but three general permits");
        let mut deferred = VecDeque::from([Request {
            version: "2.0".into(),
            id: RequestId::Number(1),
            method: "batch".into(),
            params: Value::Nil,
        }]);
        let writer = Arc::new(Mutex::new(tokio::io::sink()));
        let mut tasks = JoinSet::new();
        let mut bypass_budget = None;
        let (errors, _error_responses) = mpsc::channel(1);

        start_admissible(
            &mut deferred,
            &mut tasks,
            &writer,
            &admissions,
            &mut bypass_budget,
        );
        assert_eq!(bypass_budget, Some(3));

        for id in 2..=5 {
            accept_frame(
                make_request_with_id(id, "process.status", Value::Nil),
                &mut deferred,
                &admissions,
                &mut tasks,
                &writer,
                &errors,
            )
            .await;
        }
        start_admissible(
            &mut deferred,
            &mut tasks,
            &writer,
            &admissions,
            &mut bypass_budget,
        );

        assert_eq!(tasks.len(), 3);
        assert_eq!(bypass_budget, Some(0));
        assert_eq!(deferred.len(), 2);
        assert_eq!(deferred[0].method, "batch");
        assert!(matches!(&deferred[1].id, RequestId::Number(5)));
        tasks.abort_all();
    }

    #[test]
    fn test_blocked_pty_writes_use_dedicated_admission() {
        let admissions = Admissions::default();
        let _general = admissions
            .try_acquire_many(TaskClass::General, GENERAL_TASK_LIMIT)
            .expect("reserve every general permit");

        assert_eq!(task_class("process.write_pty"), TaskClass::PtyWrite);
        assert_eq!(task_class("process.signal"), TaskClass::Control);
        assert!(admissions.try_acquire(TaskClass::PtyWrite).is_some());
        assert!(admissions.try_acquire(TaskClass::Control).is_some());
    }

    /// Malformed frames must always be answered.  A dropped response leaves
    /// the client blocked on its own timeout with no diagnosis.
    #[tokio::test]
    async fn test_every_malformed_frame_is_answered() {
        // Even though this test never starts a process, its connection runs
        // cleanup_managed_processes at EOF, which kills and clears the
        // process-wide registries.  Serialize with tests that own children.
        let _test_lock = handlers::process::test_process_map_lock().await;
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
            None,
        ));

        let malformed = ERROR_RESPONSE_CHANNEL_SIZE * 3;
        let writes = tokio::spawn(async move {
            for _ in 0..malformed {
                client.write_all(&frame(&[0xc1])).await.unwrap();
            }
            client
        });

        for _ in 0..malformed {
            let response = read_frame(&mut client_reader).await;
            let code = map_get(&response, "error").and_then(map_get_code);
            assert!(
                matches!(
                    code,
                    Some(RpcError::PARSE_ERROR) | Some(RpcError::INVALID_REQUEST)
                ),
                "unexpected code {code:?}"
            );
        }

        drop(writes.await.unwrap());
        connection
            .await
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
    }

    fn frame(payload: &[u8]) -> Vec<u8> {
        let mut frame = (payload.len() as u32).to_be_bytes().to_vec();
        frame.extend_from_slice(payload);
        frame
    }

    async fn read_frame<R: AsyncRead + Unpin>(reader: &mut R) -> Value {
        let mut len = [0; 4];
        reader.read_exact(&mut len).await.unwrap();
        let mut payload = vec![0; u32::from_be_bytes(len) as usize];
        reader.read_exact(&mut payload).await.unwrap();
        rmp_serde::from_slice(&payload).unwrap()
    }

    async fn wait_for_marker(path: &std::path::Path) {
        // Generous failure-only ceiling: interpreter cold starts on loaded CI
        // runners can take well over a second.
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            while !path.exists() {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
        })
        .await
        .unwrap_or_else(|_| panic!("marker was not created: {}", path.display()));
    }

    #[tokio::test]
    async fn test_connection_handles_fragmented_frame_while_writing_response() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let (mut client, server_reader) = tokio::io::duplex(1024);
        let (server_writer, mut client_reader) = tokio::io::duplex(1024);
        let writer = Arc::new(Mutex::new(server_writer));
        let connection = tokio::spawn(run_connection(server_reader, writer, None));
        let first = make_request("missing.first", Value::Map(vec![]));
        let second = make_request("missing.second", Value::Map(vec![]));

        client.write_all(&frame(&first)).await.unwrap();
        let second_frame = frame(&second);
        // Leave the reader in the middle of the next prefix while the first
        // request runs through JoinSet and the response writer.
        client.write_all(&second_frame[..1]).await.unwrap();
        let first_response = read_frame(&mut client_reader).await;
        assert_eq!(
            map_get(&first_response, "error").and_then(map_get_code),
            Some(RpcError::METHOD_NOT_FOUND)
        );

        client.write_all(&second_frame[1..4]).await.unwrap();
        client.write_all(&second_frame[4..]).await.unwrap();
        let second_response = read_frame(&mut client_reader).await;
        assert_eq!(
            map_get(&second_response, "error").and_then(map_get_code),
            Some(RpcError::METHOD_NOT_FOUND)
        );

        drop(client);
        connection
            .await
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
    }

    #[tokio::test]
    async fn test_connection_recovers_admission_after_panicked_tasks() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
            None,
        ));

        for id in 1..=GENERAL_TASK_LIMIT as i64 {
            client
                .write_all(&frame(&make_request_with_id(
                    id,
                    "test.panic",
                    Value::Map(vec![]),
                )))
                .await
                .unwrap();
        }
        let mut completed = Vec::with_capacity(GENERAL_TASK_LIMIT);
        for _ in 0..GENERAL_TASK_LIMIT {
            let response = read_frame(&mut client_reader).await;
            completed.push(map_get_id(&response).expect("panicked request response id"));
            assert_eq!(
                map_get(&response, "error").and_then(map_get_code),
                Some(RpcError::METHOD_NOT_FOUND)
            );
        }
        completed.sort_unstable();
        assert_eq!(
            completed,
            (1..=GENERAL_TASK_LIMIT as i64).collect::<Vec<_>>()
        );

        client
            .write_all(&frame(&make_request_with_id(
                999,
                "missing.after-panic",
                Value::Map(vec![]),
            )))
            .await
            .unwrap();
        let response = read_frame(&mut client_reader).await;
        assert_eq!(map_get_id(&response), Some(999));
        assert_eq!(
            map_get(&response, "error").and_then(map_get_code),
            Some(RpcError::METHOD_NOT_FOUND)
        );

        drop(client);
        connection
            .await
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
    }

    #[tokio::test]
    async fn test_process_run_without_stdin_uses_null() {
        let params = Value::Map(vec![(
            Value::String("cmd".into()),
            Value::String("cat".into()),
        )]);
        let response = process_request(&make_request("process.run", params)).await;
        assert!(response.error.is_none());
        assert_eq!(
            map_get(response.result.as_ref().unwrap(), "stdout"),
            Some(&Value::Binary(vec![]))
        );
    }

    #[tokio::test]
    async fn test_process_run_large_bidirectional_io_does_not_deadlock() {
        let input = vec![b'x'; 1024 * 1024];
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("cat".into())),
            (Value::String("stdin".into()), Value::Binary(input.clone())),
        ]);
        let response = process_request(&make_request("process.run", params)).await;
        assert!(response.error.is_none());
        assert_eq!(
            map_get(response.result.as_ref().unwrap(), "stdout"),
            Some(&Value::Binary(input))
        );
    }

    /// A child that stops reading stdin early is ordinary shell behavior.
    /// `tramp-sh' runs `command <infile', where EPIPE never reaches Emacs, so
    /// `process-file' must still see the output and the real exit status.
    #[tokio::test]
    async fn test_process_run_stdin_broken_pipe_preserves_output() {
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("head -c 5".into()),
                ]),
            ),
            (
                Value::String("stdin".into()),
                Value::Binary(vec![b'x'; 8 * 1024 * 1024]),
            ),
        ]);
        let response = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            process_request(&make_request("process.run", params)),
        )
        .await
        .expect("broken stdin pipe must not hang");
        assert!(response.error.is_none(), "error: {:?}", response.error);
        let result = response.result.unwrap();
        assert_eq!(
            map_get(&result, "stdout"),
            Some(&Value::Binary(b"xxxxx".to_vec()))
        );
        assert_eq!(
            map_get(&result, "exit_code").and_then(Value::as_i64),
            Some(0)
        );
    }

    /// A child that closes stdin but keeps running must not stall the RPC
    /// either; the write is abandoned and the child's exit status is used.
    #[tokio::test]
    async fn test_process_run_stdin_closed_early_still_completes() {
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("exec 0<&-; printf done; exit 3".into()),
                ]),
            ),
            (
                Value::String("stdin".into()),
                Value::Binary(vec![b'x'; 1024 * 1024]),
            ),
        ]);
        let response = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            process_request(&make_request("process.run", params)),
        )
        .await
        .expect("closed stdin must not hang");
        assert!(response.error.is_none(), "error: {:?}", response.error);
        let result = response.result.unwrap();
        assert_eq!(
            map_get(&result, "stdout"),
            Some(&Value::Binary(b"done".to_vec()))
        );
        assert_eq!(
            map_get(&result, "exit_code").and_then(Value::as_i64),
            Some(3)
        );
    }

    #[tokio::test]
    async fn test_commands_run_parallel_stdin_broken_pipe_preserves_output() {
        let command = Value::Map(vec![
            (Value::String("key".into()), Value::String("head".into())),
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("head -c 5".into()),
                ]),
            ),
            (
                Value::String("stdin".into()),
                Value::Binary(vec![b'x'; 8 * 1024 * 1024]),
            ),
        ]);
        let result = handlers::commands::run_parallel(Value::Map(vec![(
            Value::String("commands".into()),
            Value::Array(vec![command]),
        )]))
        .await
        .unwrap();
        let entry = map_get(&result, "head").unwrap();
        assert_eq!(
            map_get(entry, "stdout"),
            Some(&Value::Binary(b"xxxxx".to_vec()))
        );
        assert_eq!(map_get(entry, "exit_code").and_then(Value::as_i64), Some(0));
    }

    #[tokio::test]
    async fn test_commands_run_parallel_uses_batch_environment_for_lookup() {
        use std::os::unix::fs::PermissionsExt;

        let temp = tempfile::tempdir().expect("temporary command directory");
        let bin = temp.path().join("bin");
        std::fs::create_dir(&bin).expect("create command directory");
        let program = bin.join("path-probe");
        std::fs::write(&program, "#!/bin/sh\nprintf selected").expect("write command");
        let mut permissions = std::fs::metadata(&program)
            .expect("command metadata")
            .permissions();
        permissions.set_mode(0o755);
        std::fs::set_permissions(&program, permissions).expect("make command executable");

        let command = Value::Map(vec![
            (Value::String("key".into()), Value::String("probe".into())),
            (
                Value::String("cmd".into()),
                Value::String("path-probe".into()),
            ),
        ]);
        let result = handlers::commands::run_parallel(Value::Map(vec![
            (
                Value::String("commands".into()),
                Value::Array(vec![command]),
            ),
            (
                Value::String("env".into()),
                Value::Map(vec![(
                    Value::String("PATH".into()),
                    Value::String(bin.to_string_lossy().into_owned().into()),
                )]),
            ),
        ]))
        .await
        .unwrap();
        let entry = map_get(&result, "probe").unwrap();
        assert_eq!(
            map_get(entry, "stdout"),
            Some(&Value::Binary(b"selected".to_vec()))
        );
        assert_eq!(map_get(entry, "exit_code").and_then(Value::as_i64), Some(0));
    }

    #[tokio::test]
    async fn test_commands_run_parallel_stdin() {
        let command = Value::Map(vec![
            (Value::String("key".into()), Value::String("cat".into())),
            (Value::String("cmd".into()), Value::String("cat".into())),
            (
                Value::String("stdin".into()),
                Value::Binary(b"input".to_vec()),
            ),
        ]);
        let no_stdin = Value::Map(vec![
            (Value::String("key".into()), Value::String("empty".into())),
            (Value::String("cmd".into()), Value::String("cat".into())),
        ]);
        let result = handlers::commands::run_parallel(Value::Map(vec![(
            Value::String("commands".into()),
            Value::Array(vec![command, no_stdin]),
        )]))
        .await
        .unwrap();
        let output = match map_get(map_get(&result, "cat").unwrap(), "stdout").unwrap() {
            Value::Binary(output) => output,
            value => panic!("expected binary stdout, got {value:?}"),
        };
        assert_eq!(output, b"input");
        assert_eq!(
            map_get(map_get(&result, "empty").unwrap(), "stdout"),
            Some(&Value::Binary(vec![]))
        );
    }

    #[tokio::test]
    async fn test_commands_run_parallel_signal_exit_code() {
        let command = Value::Map(vec![
            (Value::String("key".into()), Value::String("signal".into())),
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("kill -TERM $$".into()),
                ]),
            ),
        ]);
        let result = handlers::commands::run_parallel(Value::Map(vec![(
            Value::String("commands".into()),
            Value::Array(vec![command]),
        )]))
        .await
        .unwrap();
        let entry = map_get(&result, "signal").unwrap();
        assert_eq!(
            map_get(entry, "exit_code").and_then(Value::as_i64),
            Some(143)
        );
    }

    #[tokio::test]
    /// A child that closes stdin early must still report its own output and
    /// exit status: `tramp-sh' runs `command <infile', where EPIPE never
    /// reaches Emacs.
    async fn test_commands_run_parallel_stdin_closed_early_completes() {
        let command = Value::Map(vec![
            (Value::String("key".into()), Value::String("closed".into())),
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("exec 0<&-; printf done; exit 7".into()),
                ]),
            ),
            (
                Value::String("stdin".into()),
                Value::Binary(vec![b'x'; 1024 * 1024]),
            ),
        ]);
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            handlers::commands::run_parallel(Value::Map(vec![(
                Value::String("commands".into()),
                Value::Array(vec![command]),
            )])),
        )
        .await
        .expect("closed stdin must not hang")
        .unwrap();
        let entry = map_get(&result, "closed").unwrap();
        assert_eq!(
            map_get(entry, "stdout"),
            Some(&Value::Binary(b"done".to_vec()))
        );
        assert_eq!(map_get(entry, "exit_code").and_then(Value::as_i64), Some(7));
        assert_eq!(map_get(entry, "stderr"), Some(&Value::Binary(vec![])));
    }

    #[tokio::test]
    async fn test_connection_eof_kills_synchronous_process_groups() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        for (index, method) in ["process.run", "commands.run_parallel"]
            .into_iter()
            .enumerate()
        {
            let temp = tempfile::tempdir().expect("temporary marker directory");
            let marker = temp.path().join("pid");
            let args = Value::Array(vec![
                Value::String("-c".into()),
                Value::String(
                    format!(
                        "import os,time; p={marker:?}; open(p+'.tmp', 'w').write(str(os.getpid())); os.replace(p+'.tmp', p); time.sleep(30)"
                    )
                    .into(),
                ),
            ]);
            let command = Value::Map(vec![
                (Value::String("cmd".into()), Value::String("python3".into())),
                (Value::String("args".into()), args),
            ]);
            let params = if method == "process.run" {
                command
            } else {
                let mut entry = command.as_map().unwrap().clone();
                entry.push((Value::String("key".into()), Value::String("test".into())));
                Value::Map(vec![(
                    Value::String("commands".into()),
                    Value::Array(vec![Value::Map(entry)]),
                )])
            };

            let (mut client, server_reader) = tokio::io::duplex(4096);
            let (server_writer, _client_reader) = tokio::io::duplex(4096);
            let connection = tokio::spawn(run_connection(
                server_reader,
                Arc::new(Mutex::new(server_writer)),
                None,
            ));
            client
                .write_all(&frame(&make_request_with_id(
                    700 + index as i64,
                    method,
                    params,
                )))
                .await
                .unwrap();
            wait_for_marker(&marker).await;
            let child_pid: i32 = std::fs::read_to_string(&marker)
                .expect("read child PID")
                .parse()
                .expect("parse child PID");
            drop(client);
            tokio::time::timeout(std::time::Duration::from_secs(3), connection)
                .await
                .expect("connection cleanup should be bounded")
                .expect("connection task should not panic")
                .expect("connection cleanup should succeed");

            handlers::process::wait_for_process_exit(child_pid).await;
        }
    }

    #[tokio::test]
    async fn test_method_not_found() {
        let params = Value::Map(vec![]);
        let payload = make_request("nonexistent.method", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_some());
        assert_eq!(response.error.unwrap().code, RpcError::METHOD_NOT_FOUND);
    }

    fn map_get<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
        value.as_map().and_then(|m| {
            m.iter()
                .find(|(k, _)| k.as_str() == Some(key))
                .map(|(_, v)| v)
        })
    }

    fn map_get_code(value: &Value) -> Option<i32> {
        map_get(value, "code")
            .and_then(Value::as_i64)
            .map(|code| code as i32)
    }

    fn map_get_id(value: &Value) -> Option<i64> {
        map_get(value, "id").and_then(Value::as_i64)
    }

    /// Managed children that ignore SIGTERM and block in-flight reads must
    /// still be SIGKILLed and reaped when the transport reaches EOF, so the
    /// connection task terminates within its bounded cleanup window.
    #[tokio::test]
    async fn test_connection_eof_second_cleanup_catches_late_registration() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let barrier = Arc::new(CleanupBarrier {
            first_pass_complete: tokio::sync::Notify::new(),
            continue_cleanup: tokio::sync::Notify::new(),
        });
        let (client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, _client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
            Some(Arc::clone(&barrier)),
        ));

        drop(client);
        tokio::time::timeout(
            std::time::Duration::from_secs(1),
            barrier.first_pass_complete.notified(),
        )
        .await
        .expect("connection should reach the second cleanup pass");
        let start = handlers::process::start(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("sleep".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![Value::String("30".into())]),
            ),
        ]))
        .await
        .expect("register child between cleanup passes");
        assert!(map_get(&start, "pid").and_then(Value::as_u64).is_some());
        let managed_pids = handlers::process::test_managed_os_pids().await;
        assert_eq!(managed_pids.len(), 1);
        let os_pid = managed_pids[0];
        barrier.continue_cleanup.notify_one();
        tokio::time::timeout(std::time::Duration::from_secs(3), connection)
            .await
            .expect("second cleanup should be bounded")
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
        assert!(handlers::process::test_managed_maps_empty().await);
        assert!(matches!(
            nix::sys::wait::waitpid(
                nix::unistd::Pid::from_raw(os_pid),
                Some(nix::sys::wait::WaitPidFlag::WNOHANG)
            ),
            Err(nix::errno::Errno::ECHILD)
        ));
    }

    #[tokio::test]
    async fn test_connection_eof_sigkills_blocked_pipe_and_pty_requests() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
            None,
        ));
        let temp = tempfile::tempdir().expect("temporary marker directory");
        let markers = [
            temp.path().join("pipe-ready"),
            temp.path().join("pty-ready"),
        ];

        for ((id, method), marker) in [(101, "process.start"), (102, "process.start_pty")]
            .into_iter()
            .zip(&markers)
        {
            let ignore_term = Value::Array(vec![
                Value::String("-c".into()),
                Value::String(
                    format!(
                        "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); open({marker:?}, 'w').close(); time.sleep(30)"
                    )
                    .into(),
                ),
            ]);
            client
                .write_all(&frame(&make_request_with_id(
                    id,
                    method,
                    Value::Map(vec![
                        (Value::String("cmd".into()), Value::String("python3".into())),
                        (Value::String("args".into()), ignore_term),
                    ]),
                )))
                .await
                .unwrap();
        }
        let mut pipe_pid = None;
        let mut pty_pid = None;
        for _ in 0..2 {
            let response = read_frame(&mut client_reader).await;
            assert!(map_get(&response, "error").is_none(), "{response:?}");
            let pid = map_get(&response, "result")
                .and_then(|result| map_get(result, "pid"))
                .and_then(Value::as_u64)
                .expect("start response pid") as i64;
            match map_get_id(&response) {
                Some(101) => pipe_pid = Some(pid),
                Some(102) => pty_pid = Some(pid),
                id => panic!("unexpected start response id: {id:?}"),
            }
        }

        for marker in &markers {
            wait_for_marker(marker).await;
        }

        let managed_pids = handlers::process::test_managed_os_pids().await;
        assert_eq!(managed_pids.len(), 2);
        client
            .write_all(&frame(&make_request(
                "process.read",
                Value::Map(vec![
                    (
                        Value::String("pid".into()),
                        Value::Integer(pipe_pid.expect("pipe pid").into()),
                    ),
                    (
                        Value::String("timeout_ms".into()),
                        Value::Integer(30_000.into()),
                    ),
                ]),
            )))
            .await
            .unwrap();
        client
            .write_all(&frame(&make_request(
                "process.read_pty",
                Value::Map(vec![
                    (
                        Value::String("pid".into()),
                        Value::Integer(pty_pid.expect("pty pid").into()),
                    ),
                    (
                        Value::String("timeout_ms".into()),
                        Value::Integer(30_000.into()),
                    ),
                ]),
            )))
            .await
            .unwrap();

        // The children ignore SIGTERM and both requests are blocked on their
        // output.  EOF must still finish after cleanup escalates to SIGKILL.
        drop(client);
        drop(client_reader);
        // PTY cleanup gives the direct child and its process group separate
        // grace periods before escalating, so allow both to elapse.
        tokio::time::timeout(std::time::Duration::from_secs(5), connection)
            .await
            .expect("EOF cleanup should be bounded")
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
        assert!(handlers::process::test_managed_maps_empty().await);
        for os_pid in managed_pids {
            assert!(matches!(
                nix::sys::wait::waitpid(
                    nix::unistd::Pid::from_raw(os_pid),
                    Some(nix::sys::wait::WaitPidFlag::WNOHANG)
                ),
                Err(nix::errno::Errno::ECHILD)
            ));
        }
    }

    /// An over-subscribed general pool must queue rather than reject, while a
    /// control request still overtakes the backlog.  Rejecting would surface
    /// as a spurious `remote-file-error' for a perfectly valid request.
    #[tokio::test]
    async fn test_general_overload_queues_and_control_is_reserved() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
            None,
        ));

        let start = make_request(
            "process.start",
            Value::Map(vec![
                (Value::String("cmd".into()), Value::String("sleep".into())),
                (
                    Value::String("args".into()),
                    Value::Array(vec![Value::String("30".into())]),
                ),
            ]),
        );
        client.write_all(&frame(&start)).await.unwrap();
        let start_response = read_frame(&mut client_reader).await;
        let pid = map_get(&start_response, "result")
            .and_then(|result| map_get(result, "pid"))
            .and_then(Value::as_u64)
            .expect("process.start pid") as i64;

        let read_params = || {
            Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(30_000.into()),
                ),
            ])
        };
        for id in 1..=GENERAL_TASK_LIMIT as i64 {
            client
                .write_all(&frame(&make_request_with_id(
                    id,
                    "process.read",
                    read_params(),
                )))
                .await
                .unwrap();
        }
        client
            .write_all(&frame(&make_request_with_id(
                999,
                "process.read",
                read_params(),
            )))
            .await
            .unwrap();
        client
            .write_all(&frame(&make_request_with_id(
                1000,
                "process.kill",
                Value::Map(vec![
                    (Value::String("pid".into()), Value::Integer(pid.into())),
                    (Value::String("signal".into()), Value::Integer(9.into())),
                ]),
            )))
            .await
            .unwrap();

        // The kill answers first, from its reserved pool, even though the
        // queued 17th read arrived before it.
        let first = read_frame(&mut client_reader).await;
        assert_eq!(map_get_id(&first), Some(1000));
        assert!(map_get(&first, "error").is_none(), "kill failed: {first:?}");

        // Every read, including the queued one, is eventually answered; none
        // is rejected with a synthetic overload error.
        let mut seen = Vec::new();
        while seen.len() < GENERAL_TASK_LIMIT + 1 {
            let response = read_frame(&mut client_reader).await;
            assert_ne!(
                map_get(&response, "error").and_then(map_get_code),
                Some(RpcError::INTERNAL_ERROR),
                "queued request was rejected: {response:?}"
            );
            seen.push(map_get_id(&response).expect("response id"));
        }
        seen.sort_unstable();
        let mut expected: Vec<i64> = (1..=GENERAL_TASK_LIMIT as i64).collect();
        expected.push(999);
        assert_eq!(seen, expected);

        drop(client);
        connection
            .await
            .expect("connection task should not panic")
            .expect("connection cleanup should succeed");
    }

    #[tokio::test]
    async fn test_process_write_not_blocked_by_long_poll_read() {
        let _test_lock = handlers::process::test_process_map_lock().await;
        let start_params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("cat".into())),
            (Value::String("cwd".into()), Value::String("/tmp".into())),
        ]);
        let start_payload = make_request("process.start", start_params);
        let start_response = process_request(&start_payload).await;
        assert!(
            start_response.error.is_none(),
            "process.start should not error"
        );
        let pid = map_get(start_response.result.as_ref().unwrap(), "pid")
            .and_then(Value::as_u64)
            .expect("process.start should return pid") as u32;

        let read_payload = make_request(
            "process.read",
            Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("timeout_ms".into()),
                    Value::Integer(1_000.into()),
                ),
            ]),
        );
        let read_task = tokio::spawn(async move { process_request(&read_payload).await });

        // Give the long-polling read request time to enter the handler.  If it
        // holds the global process map lock across the read timeout,
        // process.write below will be delayed by roughly timeout_ms.
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;

        let write_payload = make_request(
            "process.write",
            Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (
                    Value::String("data".into()),
                    Value::Binary(b"ping\n".to_vec()),
                ),
            ]),
        );
        let start = std::time::Instant::now();
        let write_response = process_request(&write_payload).await;
        let elapsed = start.elapsed();
        assert!(
            write_response.error.is_none(),
            "process.write should not error"
        );
        assert!(
            elapsed < std::time::Duration::from_millis(500),
            "process.write was blocked behind process.read for {:?}",
            elapsed
        );

        let _ = read_task.await;
        let kill_payload = make_request(
            "process.kill",
            Value::Map(vec![
                (Value::String("pid".into()), Value::Integer(pid.into())),
                (Value::String("signal".into()), Value::Integer(9.into())),
            ]),
        );
        let _ = process_request(&kill_payload).await;
    }

    /// Test that process.run returns 128+signal for signal-killed processes.
    /// This is required by Emacs `process-file' (tramp-test28-process-file).
    #[tokio::test]
    async fn test_process_run_signal_exit_code() {
        // SIGINT (signal 2) -> expect exit code 130
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("kill -2 $$".into()),
                ]),
            ),
            (Value::String("cwd".into()), Value::String("/tmp".into())),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(exit_code, 130, "SIGINT should produce exit code 128+2=130");
    }

    /// Test that process.run returns 128+signal for SIGKILL.
    #[tokio::test]
    async fn test_process_run_sigkill_exit_code() {
        // SIGKILL (signal 9) -> expect exit code 137
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("kill -9 $$".into()),
                ]),
            ),
            (Value::String("cwd".into()), Value::String("/tmp".into())),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(exit_code, 137, "SIGKILL should produce exit code 128+9=137");
    }

    /// Test that process.run returns the correct exit code for normal exit.
    #[tokio::test]
    async fn test_process_run_normal_exit_code() {
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("exit 42".into()),
                ]),
            ),
            (Value::String("cwd".into()), Value::String("/tmp".into())),
        ]);
        let payload = make_request("process.run", params);
        let response = process_request(&payload).await;
        assert!(response.error.is_none(), "process.run should not error");

        let result = response.result.expect("should have result");
        let exit_code = result
            .as_map()
            .and_then(|m| {
                m.iter()
                    .find(|(k, _)| k.as_str() == Some("exit_code"))
                    .map(|(_, v)| v.as_i64().unwrap())
            })
            .expect("should have exit_code");
        assert_eq!(exit_code, 42, "exit 42 should produce exit code 42");
    }

    /// Test exit_code_from_status with raw ExitStatus values.
    #[cfg(unix)]
    #[test]
    fn test_exit_code_from_status_signals() {
        use std::os::unix::process::ExitStatusExt;
        use std::process::ExitStatus;

        // Normal exit with code 0
        let status = ExitStatus::from_raw(0 << 8); // WEXITSTATUS=0, WIFEXITED=true
        assert_eq!(protocol::exit_code_from_status(status), 0);

        // Normal exit with code 42
        let status = ExitStatus::from_raw(42 << 8);
        assert_eq!(protocol::exit_code_from_status(status), 42);

        // Signal 2 (SIGINT): raw status = 2 (low byte = signal, no core dump)
        let status = ExitStatus::from_raw(2);
        assert_eq!(
            protocol::exit_code_from_status(status),
            130,
            "SIGINT raw status should give 128+2=130"
        );

        // Signal 9 (SIGKILL): raw status = 9
        let status = ExitStatus::from_raw(9);
        assert_eq!(
            protocol::exit_code_from_status(status),
            137,
            "SIGKILL raw status should give 128+9=137"
        );

        // Signal 15 (SIGTERM): raw status = 15
        let status = ExitStatus::from_raw(15);
        assert_eq!(
            protocol::exit_code_from_status(status),
            143,
            "SIGTERM raw status should give 128+15=143"
        );

        // Signal 2 with core dump: raw status = 2 | 0x80 = 130
        let status = ExitStatus::from_raw(2 | 0x80);
        assert_eq!(
            protocol::exit_code_from_status(status),
            130,
            "SIGINT with core dump should still give 128+2=130"
        );
    }
}
