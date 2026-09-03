// SPDX-License-Identifier: GPL-3.0-or-later

//! Connection-level tests: framing, admission control, and request routing.

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
fn test_individually_oversized_request_is_not_deferred() {
    let request = decode_request(&make_request("file.stat", Value::Nil)).unwrap();
    let mut deferred = VecDeque::new();

    let response = enqueue_deferred(&mut deferred, request, DEFERRED_BYTE_LIMIT + 1)
        .expect_err("an individually oversized request must be rejected");

    assert!(deferred.is_empty());
    assert!(matches!(response.id, Some(RequestId::Number(1))));
    assert_eq!(response.error.unwrap().code, RpcError::INVALID_REQUEST);
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
        DeferredRequest::for_test(Request {
            version: "2.0".into(),
            id: RequestId::Number(1),
            method: "batch".into(),
            params: Value::Nil,
        }),
        DeferredRequest::for_test(Request {
            version: "2.0".into(),
            id: RequestId::Number(2),
            method: "process.status".into(),
            params: Value::Nil,
        }),
        DeferredRequest::for_test(Request {
            version: "2.0".into(),
            id: RequestId::Number(3),
            method: "process.status".into(),
            params: Value::Nil,
        }),
        DeferredRequest::for_test(Request {
            version: "2.0".into(),
            id: RequestId::Number(4),
            method: "process.status".into(),
            params: Value::Nil,
        }),
        DeferredRequest::for_test(Request {
            version: "2.0".into(),
            id: RequestId::Number(5),
            method: "process.status".into(),
            params: Value::Nil,
        }),
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
    let mut deferred = VecDeque::from([DeferredRequest::for_test(Request {
        version: "2.0".into(),
        id: RequestId::Number(1),
        method: "batch".into(),
        params: Value::Nil,
    })]);
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
        "process.write was blocked behind process.read for {elapsed:?}"
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
