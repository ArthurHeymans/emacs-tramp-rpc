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

use protocol::{Request, Response, RpcError};
use rmpv::Value;
use std::io::Cursor;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufWriter};
use tokio::sync::{Mutex, mpsc};
use tokio::task::JoinSet;

pub(crate) const MAX_FRAME_SIZE: usize = 100 * 1024 * 1024;
pub(crate) const MAX_RESPONSE_OUTPUT_BYTES: usize = MAX_FRAME_SIZE - 1024 * 1024;
const FRAME_CHANNEL_SIZE: usize = 2;
const GENERAL_TASK_LIMIT: usize = 16;
const CONTROL_TASK_LIMIT: usize = 4;
const RESPONSE_TASK_LIMIT: usize = 4;
const EOF_TASK_JOIN_WAIT: std::time::Duration = std::time::Duration::from_millis(500);

/// Shared handle to the stdout writer, used by both response writing
/// and the watcher's notification sending.
pub type WriterHandle = Arc<Mutex<BufWriter<tokio::io::Stdout>>>;

async fn read_frames<R>(mut stdin: R, sender: mpsc::Sender<Vec<u8>>)
where
    R: AsyncRead + Unpin,
{
    loop {
        let mut len_buf = [0u8; 4];
        if stdin.read_exact(&mut len_buf).await.is_err() {
            break;
        }
        let len = u32::from_be_bytes(len_buf) as usize;

        // Drain oversized frames so that a following frame remains aligned.
        if len > MAX_FRAME_SIZE {
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

#[derive(Clone, Copy)]
enum TaskClass {
    General,
    Control,
    Response,
}

struct Admissions {
    general: Arc<AtomicUsize>,
    control: Arc<AtomicUsize>,
    response: Arc<AtomicUsize>,
}

struct AdmissionPermit {
    count: Arc<AtomicUsize>,
}

impl Admissions {
    fn try_acquire(&self, class: TaskClass) -> Option<AdmissionPermit> {
        let (count, limit) = match class {
            TaskClass::General => (&self.general, GENERAL_TASK_LIMIT),
            TaskClass::Control => (&self.control, CONTROL_TASK_LIMIT),
            TaskClass::Response => (&self.response, RESPONSE_TASK_LIMIT),
        };
        count
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
                (current < limit).then_some(current + 1)
            })
            .ok()
            .map(|_| AdmissionPermit {
                count: Arc::clone(count),
            })
    }
}

impl Default for Admissions {
    fn default() -> Self {
        Self {
            general: Arc::new(AtomicUsize::new(0)),
            control: Arc::new(AtomicUsize::new(0)),
            response: Arc::new(AtomicUsize::new(0)),
        }
    }
}

impl Drop for AdmissionPermit {
    fn drop(&mut self) {
        let released = self
            .count
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
                current.checked_sub(1)
            })
            .is_ok();
        debug_assert!(released, "admission count underflow");
    }
}

fn task_class(method: &str) -> TaskClass {
    // These operations only signal/close an already managed process.  Keep
    // them available while long-running general requests consume their slots.
    match method {
        "process.kill" | "process.close_stdin" | "process.kill_pty" | "process.close_pty" => {
            TaskClass::Control
        }
        _ => TaskClass::General,
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
    permit: AdmissionPermit,
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

fn spawn_response<W>(
    tasks: &mut JoinSet<()>,
    response: Response,
    permit: AdmissionPermit,
    writer: &Arc<Mutex<W>>,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    let writer = Arc::clone(writer);
    tasks.spawn(async move {
        let _permit = permit;
        write_response(&writer, &response).await;
    });
}

async fn run_connection<R, W>(reader: R, writer: Arc<Mutex<W>>)
where
    R: AsyncRead + Unpin + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
{
    #[cfg(test)]
    // Process-local registries are shared by tests even though production
    // gives each transport its own server process; serialize test loops.
    let _process_test_lock = handlers::process::test_process_map_lock().await;

    let (sender, mut frames) = mpsc::channel(FRAME_CHANNEL_SIZE);
    tokio::spawn(read_frames(reader, sender));
    let mut tasks: JoinSet<()> = JoinSet::new();
    let admissions = Admissions::default();

    loop {
        if tasks.is_empty() {
            let Some(payload) = frames.recv().await else {
                break;
            };
            accept_frame(payload, &mut tasks, &writer, &admissions);
        } else {
            tokio::select! {
                Some(_) = tasks.join_next() => {}
                payload = frames.recv() => match payload {
                    Some(payload) => accept_frame(payload, &mut tasks, &writer, &admissions),
                    None => break,
                },
            }
        }
    }

    // Once the transport is gone no response can be relied on.  Reap managed
    // children before joining request tasks: a task blocked on their pipes or
    // a descendant-held descriptor must not prevent SIGKILL escalation.
    handlers::cleanup_managed_processes().await;
    let _ = tokio::time::timeout(EOF_TASK_JOIN_WAIT, async {
        while tasks.join_next().await.is_some() {}
    })
    .await;
    tasks.abort_all();
}

fn accept_frame<W>(
    payload: Vec<u8>,
    tasks: &mut JoinSet<()>,
    writer: &Arc<Mutex<W>>,
    admissions: &Admissions,
) where
    W: AsyncWrite + Unpin + Send + 'static,
{
    // Decode and validate before spawning.  This lets the frame Vec be freed
    // before a handler awaits, while retaining the request params it needs.
    let request = match decode_request(&payload) {
        Ok(request) => request,
        Err(response) => {
            if let Some(permit) = admissions.try_acquire(TaskClass::Response) {
                spawn_response(tasks, *response, permit, writer);
            }
            return;
        }
    };
    let class = task_class(&request.method);
    let Some(permit) = admissions.try_acquire(class) else {
        if let Some(permit) = admissions.try_acquire(TaskClass::Response) {
            let response = Response::error(
                Some(request.id.clone()),
                RpcError::internal_error("Too many active requests"),
            );
            spawn_response(tasks, response, permit, writer);
        }
        return;
    };
    spawn_request(tasks, request, permit, writer);
}

#[tokio::main]
async fn main() {
    let stdout: WriterHandle = Arc::new(Mutex::new(BufWriter::new(tokio::io::stdout())));

    // Initialize the filesystem watcher for cache invalidation notifications.
    // If this fails (e.g. inotify not available), we continue without watching.
    // NOTE: Do NOT use eprintln! here or anywhere in the server -- SSH forwards
    // the remote process's stderr over the same pipe to Emacs, where it gets
    // mixed with the binary msgpack protocol on stdout and corrupts framing.
    if let Ok(manager) = watcher::WatchManager::new(Arc::clone(&stdout)) {
        watcher::init(manager);
    }

    run_connection(tokio::io::stdin(), stdout).await;
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
    let request: Request = match protocol::from_value(value) {
        Ok(request) => request,
        Err(e) => {
            return Err(Box::new(Response::error(
                None,
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
        assert_eq!(response.error.unwrap().code, RpcError::INVALID_REQUEST);
    }

    #[tokio::test]
    async fn test_blocked_error_responses_do_not_block_control_admission() {
        let (server_writer, _client_reader) = tokio::io::duplex(1);
        let writer = Arc::new(Mutex::new(server_writer));
        let mut tasks = JoinSet::new();
        let admissions = Admissions::default();

        for _ in 0..RESPONSE_TASK_LIMIT + 2 {
            accept_frame(vec![0xc1], &mut tasks, &writer, &admissions);
        }
        assert_eq!(
            admissions.response.load(Ordering::Acquire),
            RESPONSE_TASK_LIMIT
        );

        accept_frame(
            make_request(
                "process.kill",
                Value::Map(vec![
                    (Value::String("pid".into()), Value::Integer(0.into())),
                    (Value::String("signal".into()), Value::Integer(9.into())),
                ]),
            ),
            &mut tasks,
            &writer,
            &admissions,
        );
        assert_eq!(admissions.control.load(Ordering::Acquire), 1);

        tasks.abort_all();
        while tasks.join_next().await.is_some() {}
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
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while !path.exists() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap_or_else(|_| panic!("marker was not created: {}", path.display()));
    }

    #[tokio::test]
    async fn test_connection_handles_fragmented_frame_while_writing_response() {
        let (mut client, server_reader) = tokio::io::duplex(1024);
        let (server_writer, mut client_reader) = tokio::io::duplex(1024);
        let writer = Arc::new(Mutex::new(server_writer));
        let connection = tokio::spawn(run_connection(server_reader, writer));
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
        connection.await.unwrap();
    }

    #[tokio::test]
    async fn test_connection_recovers_admission_after_panicked_tasks() {
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
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
        connection.await.unwrap();
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

    #[tokio::test]
    async fn test_process_run_stdin_write_failure_is_propagated() {
        let params = Value::Map(vec![
            (Value::String("cmd".into()), Value::String("/bin/sh".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![
                    Value::String("-c".into()),
                    Value::String("exec 0<&-; sleep 30".into()),
                ]),
            ),
            (
                Value::String("stdin".into()),
                Value::Binary(vec![b'x'; 1024 * 1024]),
            ),
        ]);
        let response = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            process_request(&make_request("process.run", params)),
        )
        .await
        .expect("stdin failure should kill the child promptly");
        assert_eq!(response.error.unwrap().code, RpcError::PROCESS_ERROR);
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

    #[tokio::test]
    async fn test_connection_eof_sigkills_blocked_pipe_and_pty_requests() {
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
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
        tokio::time::timeout(std::time::Duration::from_secs(3), connection)
            .await
            .expect("EOF cleanup should be bounded")
            .expect("connection task should not panic");
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

    #[tokio::test]
    async fn test_general_overload_preserves_id_and_control_is_reserved() {
        let (mut client, server_reader) = tokio::io::duplex(4096);
        let (server_writer, mut client_reader) = tokio::io::duplex(4096);
        let connection = tokio::spawn(run_connection(
            server_reader,
            Arc::new(Mutex::new(server_writer)),
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

        let first = read_frame(&mut client_reader).await;
        let second = read_frame(&mut client_reader).await;
        let responses = [first, second];
        assert!(responses.iter().any(|response| {
            map_get_id(response) == Some(999)
                && map_get(response, "error").and_then(map_get_code)
                    == Some(RpcError::INTERNAL_ERROR)
        }));
        assert!(
            responses
                .iter()
                .any(|response| map_get_id(response) == Some(1000))
        );

        drop(client);
        connection.await.unwrap();
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
