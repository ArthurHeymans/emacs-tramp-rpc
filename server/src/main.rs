// SPDX-License-Identifier: GPL-3.0-or-later

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
use std::sync::{Arc, LazyLock};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufWriter};
use tokio::sync::{Mutex, OwnedSemaphorePermit, Semaphore, mpsc};
use tokio::task::JoinSet;

/// Global byte budget for active request params.  Slot admission alone bounds
/// task count (16 general), but each active request retains its decoded params
/// (including stdin blobs up to 100MiB).  Without byte admission, 16 large
/// active requests could retain ~1.6GiB.  This semaphore bounds retained
/// active params; permits are held for the task lifetime alongside slot
/// permits.  Deferred (queued) params are bounded separately by
/// `DEFERRED_BYTE_LIMIT`; see below.
static ACTIVE_PARAM_BYTES: LazyLock<Arc<Semaphore>> =
    LazyLock::new(|| Arc::new(Semaphore::new(128 * 1024 * 1024)));

fn try_acquire_param_bytes(size: usize) -> Option<OwnedSemaphorePermit> {
    let permits = u32::try_from(size).ok()?;
    Arc::clone(&ACTIVE_PARAM_BYTES)
        .try_acquire_many_owned(permits)
        .ok()
}

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
/// Byte budget for decoded-but-not-yet-started requests.  The deferred queue
/// retains full request params (including `process.run' stdin blobs), so a
/// count-only bound can retain ~64 x 100MiB ~= 6.4GiB.  Ordinary traffic
/// (KB-sized requests) stays governed by the count limit; large frames hit
/// this byte limit first and apply backpressure via the bounded frame channel
/// and OS pipe.
///
/// Bound note: the check runs before receiving the next frame, so the queue
/// can overshoot by one frame plus up to FRAME_CHANNEL_SIZE in-flight frames.
/// Worst queued ~= LIMIT + 3 x MAX_FRAME.  With 32MiB this is ~332MiB, far
/// below the unbounded 6.4GiB.  Active params are separately bounded by
/// ACTIVE_PARAM_BYTES (128MiB); active response buffers remain per-request
/// bounded (MAX_RESPONSE_OUTPUT_BYTES each, 16 max) and require the trusted
/// client to request 16 concurrent large outputs — mitigated by per-command
/// timeouts and the per-batch shared output budget.
const DEFERRED_BYTE_LIMIT: usize = 32 * 1024 * 1024;
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

/// A decoded request retained in the deferred queue, with its wire size.
/// `frame_size` approximates retained params (including stdin blobs) so the
/// queue can be byte-bounded, not just count-bounded.
struct DeferredRequest {
    request: Request,
    frame_size: usize,
}

impl DeferredRequest {
    fn new(request: Request, frame_size: usize) -> Self {
        Self {
            request,
            frame_size,
        }
    }

    /// Test-only constructor with zero wire size (no byte-budget impact).
    #[cfg(test)]
    fn for_test(request: Request) -> Self {
        Self {
            request,
            frame_size: 0,
        }
    }
}

impl std::ops::Deref for DeferredRequest {
    type Target = Request;
    fn deref(&self) -> &Self::Target {
        &self.request
    }
}

fn deferred_bytes(deferred: &VecDeque<DeferredRequest>) -> usize {
    deferred.iter().map(|r| r.frame_size).sum()
}

fn enqueue_deferred(
    deferred: &mut VecDeque<DeferredRequest>,
    request: Request,
    frame_size: usize,
) -> Result<(), Box<Response>> {
    if frame_size > DEFERRED_BYTE_LIMIT {
        return Err(Box::new(Response::error(
            Some(request.id),
            RpcError::invalid_request(format!(
                "Request frame exceeds {DEFERRED_BYTE_LIMIT} byte deferred limit"
            )),
        )));
    }
    deferred.push_back(DeferredRequest::new(request, frame_size));
    Ok(())
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
    byte_permit: OwnedSemaphorePermit,
    writer: &Arc<Mutex<W>>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    let writer = Arc::clone(writer);
    tasks.spawn(async move {
        // Hold both the slot permit and the param-byte permit for the task
        // lifetime, bounding active retained params to ACTIVE_PARAM_BYTES.
        let _permit = permit;
        let _byte_permit = byte_permit;
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
    let mut deferred: VecDeque<DeferredRequest> = VecDeque::new();
    let mut general_bypass_budget = None;

    loop {
        start_admissible(
            &mut deferred,
            &mut tasks,
            &writer,
            &admissions,
            &mut general_bypass_budget,
        );
        // Stop pulling frames while the backlog is full by count or bytes.
        // The bounded frame channel then stops draining the pipe, which is
        // the throttle the client can actually observe.
        let accepting = deferred.len() < DEFERRED_REQUEST_LIMIT
            && deferred_bytes(&deferred) < DEFERRED_BYTE_LIMIT;

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

/// Which classes already have a queued request that could not start during
/// one `start_admissible` pass.  Later requests of a blocked class stay queued
/// behind it so a stream of small requests cannot starve an older one.
#[derive(Default)]
struct AdmissionPass {
    general_waiting: bool,
    control_blocked: bool,
    pty_write_blocked: bool,
}

impl AdmissionPass {
    /// General requests are handled by the bypass path instead of a flag.
    fn is_blocked(&self, class: TaskClass) -> bool {
        match class {
            TaskClass::General => false,
            TaskClass::Control => self.control_blocked,
            TaskClass::PtyWrite => self.pty_write_blocked,
        }
    }

    /// Record that a request of `class` could not start.  A blocked
    /// multi-permit general request (a batch) also opens the bypass budget:
    /// the permits idle right now may serve one-permit work once, but are
    /// reserved for the batch as they return.
    fn note_blocked(
        &mut self,
        class: TaskClass,
        permit_count: usize,
        admissions: &Admissions,
        general_bypass_budget: &mut Option<usize>,
    ) {
        match class {
            TaskClass::General => {
                if permit_count > 1 && general_bypass_budget.is_none() {
                    *general_bypass_budget = Some(admissions.available_permits(TaskClass::General));
                }
                self.general_waiting = true;
            }
            TaskClass::Control => self.control_blocked = true,
            TaskClass::PtyWrite => self.pty_write_blocked = true,
        }
    }
}

/// Acquire both the slot permit and the param-byte permit for one request, or
/// neither.  A slot without byte budget is released again and treated like
/// slot exhaustion so backpressure applies.
fn try_start(
    admissions: &Admissions,
    class: TaskClass,
    permit_count: usize,
    frame_size: usize,
) -> Option<(OwnedSemaphorePermit, OwnedSemaphorePermit)> {
    let permit = admissions.try_acquire_many(class, permit_count)?;
    let byte_permit = try_acquire_param_bytes(frame_size)?;
    Some((permit, byte_permit))
}

/// Start every queued request that currently fits in its class.
///
/// Requests of different classes do not block one another.  Within the general
/// class, a large request gets a bounded bypass budget equal to the permits
/// that were idle when it first became blocked.  Those permits may serve
/// already-arriving one-permit work once, but are then reserved as they return,
/// preventing both head-of-line idling and indefinite batch starvation.
fn start_admissible<W>(
    deferred: &mut VecDeque<DeferredRequest>,
    tasks: &mut JoinSet<()>,
    writer: &Arc<Mutex<W>>,
    admissions: &Admissions,
    general_bypass_budget: &mut Option<usize>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    let mut still_deferred = VecDeque::with_capacity(deferred.len());
    let mut pass = AdmissionPass::default();
    while let Some(deferred_req) = deferred.pop_front() {
        let frame_size = deferred_req.frame_size;
        let request = deferred_req.request;
        let class = task_class(&request.method);
        let permit_count = request_permit_count(&request.method);
        // Re-wrap for potential re-queue, preserving the wire size.
        let rewrap = |request: Request| DeferredRequest::new(request, frame_size);

        if class == TaskClass::General && pass.general_waiting {
            // Only one-permit work that fits the bypass budget may overtake
            // the blocked general request.
            let Some(budget) = general_bypass_budget
                .as_mut()
                .filter(|budget| permit_count <= **budget)
            else {
                still_deferred.push_back(rewrap(request));
                continue;
            };
            match try_start(admissions, class, permit_count, frame_size) {
                Some((permit, byte_permit)) => {
                    *budget -= permit_count;
                    spawn_request(tasks, request, permit, byte_permit, writer);
                }
                None => still_deferred.push_back(rewrap(request)),
            }
            continue;
        }

        if pass.is_blocked(class) {
            still_deferred.push_back(rewrap(request));
            continue;
        }

        match try_start(admissions, class, permit_count, frame_size) {
            Some((permit, byte_permit)) => {
                if class == TaskClass::General && permit_count > 1 {
                    *general_bypass_budget = None;
                }
                spawn_request(tasks, request, permit, byte_permit, writer);
            }
            None => {
                pass.note_blocked(class, permit_count, admissions, general_bypass_budget);
                still_deferred.push_back(rewrap(request));
            }
        }
    }
    *deferred = still_deferred;
}

async fn accept_frame<W>(
    payload: Vec<u8>,
    deferred: &mut VecDeque<DeferredRequest>,
    admissions: &Admissions,
    tasks: &mut JoinSet<()>,
    writer: &Arc<Mutex<W>>,
    errors: &mpsc::Sender<Response>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    // Decode and validate before spawning.  The frame Vec is freed after
    // decode, but retained params (e.g. stdin blobs) keep ~payload.len()
    // bytes alive, so track the wire size for byte-bounded backpressure.
    let frame_size = payload.len();
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
        if let Err(response) = enqueue_deferred(deferred, request, frame_size) {
            let _ = errors.send(*response).await;
        }
        return;
    }

    // A batch reserves its full subrequest concurrency from the shared general
    // admission bound, so concurrent batches cannot multiply that limit.
    // Active params are additionally byte-bounded: a slot without byte budget
    // defers, so 16 large active requests cannot retain ~1.6GiB.
    match try_start(
        admissions,
        class,
        request_permit_count(&request.method),
        frame_size,
    ) {
        Some((permit, byte_permit)) => spawn_request(tasks, request, permit, byte_permit, writer),
        // Queue rather than reject when the request fits the deferred byte
        // limit.  An individual frame larger than the entire queue budget can
        // never become admissible there, so reject it instead of deadlocking
        // the connection around an unstartable request.
        None => {
            if let Err(response) = enqueue_deferred(deferred, request, frame_size) {
                let _ = errors.send(*response).await;
            }
        }
    }
}

#[tokio::main]
async fn main() -> std::process::ExitCode {
    let stdout: WriterHandle = Arc::new(Mutex::new(BufWriter::new(tokio::io::stdout())));
    handlers::process::init_notification_writer(Arc::clone(&stdout));

    // Initialize the filesystem watcher for cache invalidation notifications.
    // If this fails (e.g. inotify not available), we continue without watching
    // but record the state so `system.info` reports actual availability
    // instead of just the compiled backend kind.
    // NOTE: Do NOT use eprintln! here or anywhere in the server -- SSH forwards
    // the remote process's stderr over the same pipe to Emacs, where it gets
    // mixed with the binary msgpack protocol on stdout and corrupts framing.
    match watcher::WatchManager::new(Arc::clone(&stdout)) {
        Ok(manager) => watcher::init(manager),
        Err(_) => watcher::set_unavailable(),
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
mod tests;
