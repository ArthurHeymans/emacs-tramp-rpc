// SPDX-License-Identifier: GPL-3.0-or-later

//! Tests for the pipe and PTY process handlers.

use super::pipe::*;
use super::pty::*;
use super::*;
use nix::sys::wait::{WaitPidFlag, waitpid};
use nix::unistd::Pid;
use rmpv::Value;
use std::collections::HashMap;
#[cfg(target_os = "linux")]
use std::os::fd::AsRawFd;
use std::os::unix::process::ExitStatusExt;
use std::path::Path;
use std::process::Command as StdCommand;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use tokio::io::unix::AsyncFd;
use tokio::sync::{Mutex, Notify, Semaphore};

#[test]
fn signal_code_accepts_emacs_signal_names() {
    assert_eq!(
        SignalCode::Name("SIGINT".into()).resolve().unwrap(),
        libc::SIGINT
    );
    assert_eq!(
        SignalCode::Name("term".into()).resolve().unwrap(),
        libc::SIGTERM
    );
    assert_eq!(
        SignalCode::Name("SIGCLD".into()).resolve().unwrap(),
        libc::SIGCHLD
    );
    assert_eq!(
        SignalCode::Name("SIGIOT".into()).resolve().unwrap(),
        libc::SIGABRT
    );
    assert_eq!(
        SignalCode::Name("poll".into()).resolve().unwrap(),
        libc::SIGIO
    );
    assert_eq!(
        SignalCode::Name("SIGUNUSED".into()).resolve().unwrap(),
        libc::SIGSYS
    );
    assert_eq!(SignalCode::Number(0).resolve().unwrap(), 0);
    assert_eq!(
        SignalCode::Name("not-a-signal".into())
            .resolve()
            .expect_err("invalid signal name")
            .code,
        RpcError::INVALID_PARAMS
    );
}

#[cfg(any(target_os = "linux", target_os = "android"))]
#[test]
fn signal_code_accepts_emacs_realtime_signal_names() {
    let minimum = libc::SIGRTMIN();
    let maximum = libc::SIGRTMAX();
    assert_eq!(
        SignalCode::Name("SIGRTMIN".into()).resolve().unwrap(),
        minimum
    );
    assert_eq!(
        SignalCode::Name("rtmin+1".into()).resolve().unwrap(),
        minimum + 1
    );
    assert_eq!(
        SignalCode::Name("SIGRTMAX-1".into()).resolve().unwrap(),
        maximum - 1
    );
    assert_eq!(SignalCode::Number(maximum).resolve().unwrap(), maximum);
    assert!(SignalCode::Name("SIGRTMIN-1".into()).resolve().is_err());
    assert!(SignalCode::Name("SIGRTMAX+1".into()).resolve().is_err());
    assert!(SignalCode::Name("SIGRTMIN+999".into()).resolve().is_err());
    assert!(SignalCode::Name("SIGRTMIN1".into()).resolve().is_err());
    assert!(SignalCode::Name("SIGRTMAX1".into()).resolve().is_err());
}

#[test]
fn process_group_signal_reports_eperm_instead_of_treating_cleanup_as_success() {
    let error = require_process_group_signal(
        Err(std::io::Error::from_raw_os_error(libc::EPERM)),
        "send SIGKILL",
    )
    .expect_err("EPERM can leave a credential-changing descendant alive");

    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    assert!(error.message.contains("Operation not permitted"));
    assert!(
        require_process_group_signal(
            Err(std::io::Error::from_raw_os_error(libc::ESRCH)),
            "send SIGKILL",
        )
        .is_ok()
    );
}

#[tokio::test]
async fn signal_pid_signals_unmanaged_os_process() {
    let mut child = StdCommand::new("sleep")
        .arg("30")
        .spawn()
        .expect("start unmanaged child");
    let pid = child.id();

    let signal_result = signal_pid(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::String("SIGKILL".into()),
        ),
    ]))
    .await;
    if let Err(error) = signal_result {
        let _ = child.kill();
        let _ = child.wait();
        panic!("failed to signal unmanaged process: {error:?}");
    }

    let status = match tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            if let Some(status) = child.try_wait()? {
                return Ok::<_, std::io::Error>(status);
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    })
    .await
    {
        Ok(Ok(status)) => status,
        Ok(Err(error)) => {
            let _ = child.kill();
            let _ = child.wait();
            panic!("failed to query unmanaged child: {error}");
        }
        Err(_) => {
            let _ = child.kill();
            let _ = child.wait();
            panic!("timed out waiting for unmanaged child to exit");
        }
    };
    assert_eq!(status.signal(), Some(libc::SIGKILL));

    let zero_pid = signal_pid(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(0.into())),
        (Value::String("signal".into()), Value::Integer(0.into())),
    ]))
    .await
    .expect_err("PID zero must not signal the server process group");
    assert_eq!(zero_pid.code, RpcError::INVALID_PARAMS);

    let oversized_pid = signal_pid(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(u32::MAX.into())),
        (Value::String("signal".into()), Value::Integer(0.into())),
    ]))
    .await
    .expect_err("oversized PID must not become a negative kill target");
    assert_eq!(oversized_pid.code, RpcError::PROCESS_ERROR);
}

#[tokio::test]
async fn unregistered_pipe_start_guard_kills_child() {
    let _test_lock = test_process_map_lock().await;
    let mut cmd = Command::new("sleep");
    cmd.arg("30");
    cmd.kill_on_drop(true);
    configure_process_group(&mut cmd);
    let child = cmd.spawn().expect("start unregistered pipe child");
    let child_pid = child.id().expect("child PID");
    let guard = ProcessGroupGuard::new(child_pid);

    // This is the cancellation path before insertion into PROCESS_MAP.
    drop(guard);
    drop(child);
    wait_for_process_exit(child_pid as i32).await;
    assert!(!process_is_running(child_pid as i32));
}

#[tokio::test]
async fn cleanup_reports_signal_failure_and_retains_managed_process() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("sleep 30").await;

    set_test_process_group_signal_error(Some(libc::EPERM));
    let result = cleanup_managed_processes().await;
    set_test_process_group_signal_error(None);

    let error = result.expect_err("cleanup must report an unsignalled process group");
    assert!(error.message.contains("Operation not permitted"));
    assert!(get_process_map().lock().await.contains_key(&pid));

    cleanup_managed_processes()
        .await
        .expect("cleanup retry after removing injected error");
    assert!(!get_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn synchronous_output_reader_enforces_shared_limit() {
    let budget = Arc::new(RetainedOutputBudget::new(Arc::new(Semaphore::new(4))));
    let error = read_sync_output(&b"oversized"[..], budget, 4)
        .await
        .expect_err("output above the remaining response budget should fail");
    assert!(error.to_string().contains("output exceeds"));
}

#[tokio::test]
async fn timed_out_child_restores_discarded_output_budget() {
    let remaining = Arc::new(Semaphore::new(16));
    let spec = ChildSpec {
        cmd: "/bin/sh".into(),
        args: vec!["-c".into(), "printf 12345678; sleep 10".into()],
        cwd: None,
        env: None,
        clear_env: false,
        stdin: None,
        merge_stderr: false,
    };

    let result = run_child(
        spec,
        Arc::clone(&remaining),
        16,
        Some(tokio::time::Instant::now() + std::time::Duration::from_millis(100)),
    )
    .await;

    assert!(matches!(result, Err(ChildError::TimedOut)));
    assert_eq!(remaining.available_permits(), 16);
}

#[tokio::test]
async fn process_run_drains_output_when_child_closes_stdin_early() {
    let params = Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(
                    "exec 0<&-; head -c 131072 /dev/zero; \
                     head -c 131072 /dev/zero >&2; exit 3"
                        .into(),
                ),
            ]),
        ),
        (
            Value::String("stdin".into()),
            Value::Binary(vec![b'x'; 1024 * 1024]),
        ),
    ]);

    let result = tokio::time::timeout(std::time::Duration::from_secs(10), run(params))
        .await
        .expect("closed stdin and full output pipes must not hang")
        .expect("benign closed stdin must preserve the child result");
    let stdout = map_get(&result, "stdout")
        .and_then(Value::as_slice)
        .expect("binary stdout");
    let stderr = map_get(&result, "stderr")
        .and_then(Value::as_slice)
        .expect("binary stderr");
    assert_eq!(stdout.len(), 131072);
    assert_eq!(stderr.len(), 131072);
    assert!(stdout.iter().all(|byte| *byte == 0));
    assert!(stderr.iter().all(|byte| *byte == 0));
    assert_eq!(
        map_get(&result, "exit_code").and_then(Value::as_i64),
        Some(3)
    );
}

#[test]
fn merged_output_descriptors_are_close_on_exec() {
    let (read_fd, stdout_fd, stderr_fd) = merged_output_fds().expect("merged output pipe");
    for fd in [&read_fd, &stdout_fd, &stderr_fd] {
        let flags = rustix::io::fcntl_getfd(fd).expect("descriptor flags");
        assert!(flags.contains(rustix::io::FdFlags::CLOEXEC));
    }
}

#[tokio::test]
async fn process_run_merge_stderr_preserves_stream_order() {
    let params = Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("printf stderr >&2; printf stdout".into()),
            ]),
        ),
        (Value::String("merge_stderr".into()), Value::Boolean(true)),
    ]);

    let result = run(params).await.expect("merged process output");
    assert_eq!(
        map_get(&result, "stdout").and_then(Value::as_slice),
        Some(b"stderrstdout".as_slice())
    );
    assert_eq!(
        map_get(&result, "stderr").and_then(Value::as_slice),
        Some([].as_slice())
    );
}

#[tokio::test]
async fn process_run_output_limit_kills_and_reaps_child() {
    let tempdir = tempfile::tempdir().expect("temporary directory");
    let pid_path = tempdir.path().join("child.pid");
    let params = Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("echo $$ > \"$1\"; while :; do printf 0123456789abcdef; done".into()),
                Value::String("sh".into()),
                Value::String(pid_path.to_string_lossy().into_owned().into()),
            ]),
        ),
    ]);

    let error = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        run_with_output_limit(params, 4096),
    )
    .await
    .expect("output-limit failure must not hang")
    .expect_err("oversized process output must fail");
    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    assert!(error.message.contains("output exceeds 4096 byte limit"));

    let pid: i32 = std::fs::read_to_string(&pid_path)
        .expect("child pid file")
        .trim()
        .parse()
        .expect("numeric child pid");
    assert_eq!(
        rustix::process::test_kill_process(RustixPid::from_raw(pid).expect("child pid")),
        Err(rustix::io::Errno::SRCH),
        "limited child must be dead"
    );
    assert_eq!(
        waitpid(Pid::from_raw(pid), Some(WaitPidFlag::WNOHANG)),
        Err(nix::errno::Errno::ECHILD),
        "limited child must already be reaped"
    );
}

fn map_get<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
    value.as_map().and_then(|m| {
        m.iter()
            .find(|(k, _)| k.as_str() == Some(key))
            .map(|(_, v)| v)
    })
}

async fn start_pipe_process(script: &str) -> u32 {
    let result = start(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start pipe process");

    map_get(&result, "pid")
        .and_then(Value::as_u64)
        .expect("process pid") as u32
}

async fn read_pipe_process(pid: u32, max_bytes: usize, timeout_ms: u64) -> Value {
    read(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("max_bytes".into()),
            Value::Integer((max_bytes as u64).into()),
        ),
        (
            Value::String("timeout_ms".into()),
            Value::Integer(timeout_ms.into()),
        ),
    ]))
    .await
    .expect("read pipe process")
}

async fn child_has_exited(pid: u32) -> bool {
    for _ in 0..100 {
        let result = status(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("query pipe process status");
        if map_get(&result, "exited").and_then(Value::as_bool) == Some(true) {
            return true;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }

    false
}

async fn wait_for_child_exit(pid: u32) {
    assert!(child_has_exited(pid).await, "process {pid} did not exit");
}

async fn pipe_streams_at_eof(pid: u32) -> bool {
    let (stdout, stderr) = {
        let processes = get_process_map().lock().await;
        let managed = processes.get(&pid).expect("pipe process");
        (managed.stdout.clone(), managed.stderr.clone())
    };
    stdout.lock().await.is_none() && stderr.lock().await.is_none()
}

async fn remove_pipe_process(pid: u32) {
    let managed = get_process_map().lock().await.remove(&pid);
    if let Some(mut managed) = managed
        && managed.exit_status.is_none()
    {
        let _ = managed.child.start_kill();
        let _ = managed.child.wait().await;
    }
}

async fn pipe_os_pid(pid: u32) -> i32 {
    get_process_map()
        .lock()
        .await
        .get(&pid)
        .map(|managed| managed.child_pid)
        .expect("pipe OS pid") as i32
}

async fn pty_os_pid(pid: u32) -> i32 {
    get_pty_process_map()
        .lock()
        .await
        .get(&pid)
        .expect("PTY process")
        .child_pid
        .as_raw()
}

#[cfg(target_os = "linux")]
fn fd_target_count(target: &Path) -> usize {
    std::fs::read_dir("/proc/self/fd")
        .expect("list open file descriptors")
        .filter_map(Result::ok)
        .filter(|entry| std::fs::read_link(entry.path()).ok().as_deref() == Some(target))
        .count()
}

fn full_nonblocking_pipe() -> (OwnedFd, OwnedFd) {
    let (read_fd, write_fd) = rustix::pipe::pipe().expect("create pipe");
    set_fd_nonblocking(&read_fd).expect("make pipe reader nonblocking");
    set_fd_nonblocking(&write_fd).expect("make pipe writer nonblocking");
    set_fd_cloexec(&read_fd).expect("mark pipe reader close-on-exec");
    set_fd_cloexec(&write_fd).expect("mark pipe writer close-on-exec");
    let bytes = [0_u8; 8192];
    loop {
        match rustix::io::write(&write_fd, &bytes) {
            Ok(_) => continue,
            Err(errno) => {
                assert_eq!(errno, rustix::io::Errno::AGAIN, "fill pipe to EAGAIN");
                return (read_fd, write_fd);
            }
        }
    }
}

async fn install_full_pipe_pty(pid: u32) -> OwnedFd {
    let (read_fd, write_fd) = full_nonblocking_pipe();
    let async_fd = AsyncFd::new(write_fd).expect("monitor full pipe");
    get_pty_process_map()
        .lock()
        .await
        .get_mut(&pid)
        .expect("PTY process")
        .async_fd = async_fd;
    read_fd
}

async fn start_signal_ignoring_pty(marker: &Path) -> u32 {
    let script = format!(
        "import signal,time; signal.signal(signal.SIGHUP, signal.SIG_IGN); signal.signal(signal.SIGTERM, signal.SIG_IGN); open({marker:?}, 'w').close(); time.sleep(30)"
    );
    let result = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start PTY child");
    let pid = map_get(&result, "pid")
        .and_then(Value::as_u64)
        .expect("PTY pid") as u32;
    wait_for_marker(marker).await;
    pid
}

fn assert_reaped(os_pid: i32) {
    assert!(matches!(
        waitpid(Pid::from_raw(os_pid), Some(WaitPidFlag::WNOHANG)),
        Err(nix::errno::Errno::ECHILD)
    ));
}

#[tokio::test]
async fn failed_startup_reaper_spawn_reaps_asynchronously() {
    let child = StdCommand::new("sh")
        .args(["-c", "exit 0"])
        .spawn()
        .expect("start short-lived child");
    let os_pid = child.id() as i32;

    reap_pty_startup_child_with(child, |_shared_child| {
        Err(std::io::Error::other("injected thread creation failure"))
    });

    for _ in 0..200 {
        if rustix::process::test_kill_process(RustixPid::from_raw(os_pid).expect("os pid")).is_err()
        {
            assert_reaped(os_pid);
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
    panic!("asynchronous startup fallback did not reap child {os_pid}");
}

#[tokio::test]
async fn dropped_pty_startup_guard_kills_and_reaps_child() {
    let guard = tokio::task::spawn_blocking(|| {
        do_fork_exec(PtyStartParams {
            cmd: "sh".into(),
            args: vec!["-c".into(), "sleep 30".into()],
            cwd: None,
            env: None,
            clear_env: false,
            rows: 24,
            cols: 80,
        })
    })
    .await
    .expect("startup task")
    .expect("start PTY child");
    let os_pid = guard.child.as_ref().expect("PTY child").id() as i32;

    drop(guard);
    for _ in 0..200 {
        if rustix::process::test_kill_process(RustixPid::from_raw(os_pid).expect("os pid")).is_err()
        {
            assert_reaped(os_pid);
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
    panic!("startup guard did not kill and reap child {os_pid}");
}

async fn collect_pipe_output(pid: u32, max_bytes: usize) -> (Vec<u8>, Vec<u8>, i64) {
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    for _ in 0..64 {
        let result = read_pipe_process(pid, max_bytes, 500).await;
        if let Some(Value::Binary(bytes)) = map_get(&result, "stdout") {
            stdout.extend_from_slice(bytes);
        }
        if let Some(Value::Binary(bytes)) = map_get(&result, "stderr") {
            stderr.extend_from_slice(bytes);
        }

        if map_get(&result, "exited").and_then(Value::as_bool) == Some(true) {
            let exit_code = map_get(&result, "exit_code")
                .and_then(Value::as_i64)
                .expect("exit code");
            remove_pipe_process(pid).await;
            return (stdout, stderr, exit_code);
        }

        if pipe_streams_at_eof(pid).await {
            tokio::task::yield_now().await;
            if !child_has_exited(pid).await {
                remove_pipe_process(pid).await;
                panic!("process {pid} did not exit after pipe EOF");
            }
        }
    }

    remove_pipe_process(pid).await;
    panic!("process {pid} did not reach EOF");
}

#[tokio::test]
async fn write_closed_stdin_is_process_error() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("sleep 1").await;
    close_stdin(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("close stdin");

    let error = write(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("data".into()),
            Value::Binary(b"data".to_vec()),
        ),
    ]))
    .await
    .expect_err("writing closed stdin should fail");
    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    assert!(error.message.contains("Process stdin is closed"));
    assert_eq!(
        error.data,
        Some(Value::Map(vec![(
            Value::String("process_error".into()),
            Value::String("stdin_closed".into()),
        )]))
    );

    let missing = write(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(u32::MAX.into())),
        (
            Value::String("data".into()),
            Value::Binary(b"data".to_vec()),
        ),
    ]))
    .await
    .expect_err("writing to a missing process should fail");
    assert_eq!(missing.code, RpcError::PROCESS_ERROR);
    assert!(missing.message.contains("Process not found"));
    assert_eq!(
        missing.data,
        Some(Value::Map(vec![(
            Value::String("process_error".into()),
            Value::String("not_found".into()),
        )]))
    );

    let missing_close = close_stdin(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(u32::MAX.into()),
    )]))
    .await
    .expect_err("closing stdin for a missing process should fail");
    assert_eq!(missing_close.code, RpcError::PROCESS_ERROR);
    assert_eq!(
        missing_close.data,
        Some(Value::Map(vec![(
            Value::String("process_error".into()),
            Value::String("not_found".into()),
        )]))
    );
    remove_pipe_process(pid).await;
}

#[tokio::test]
async fn process_read_rejects_zero_max_bytes() {
    let error = read(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(1.into())),
        (Value::String("max_bytes".into()), Value::Integer(0.into())),
    ]))
    .await
    .expect_err("zero max_bytes should be rejected");

    assert_eq!(error.code, RpcError::INVALID_PARAMS);
}

#[test]
fn executable_lookup_resolves_relative_path_against_cwd() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let bin = tmp.path().join("bin");
    std::fs::create_dir(&bin).unwrap();
    std::fs::write(bin.join("tool"), b"").unwrap();
    let env = HashMap::from([("PATH".to_string(), "bin".to_string())]);

    assert!(!executable_is_missing_sync(
        "tool",
        tmp.path().to_str(),
        Some(&env),
        true
    ));
}

#[tokio::test]
async fn process_spawn_not_found_preserves_errno() {
    let error = run(Value::Map(vec![(
        Value::String("cmd".into()),
        Value::String("/definitely/not/a/tramp-rpc-command".into()),
    )]))
    .await
    .expect_err("missing command should fail to spawn");

    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    let errno = error
        .data
        .as_ref()
        .and_then(Value::as_map)
        .and_then(|data| {
            data.iter()
                .find(|(key, _)| key.as_str() == Some("os_errno"))
        })
        .and_then(|(_, value)| value.as_i64());
    assert_eq!(errno, Some(i64::from(libc::ENOENT)));
    assert_eq!(
        map_get(error.data.as_ref().unwrap(), "spawn_not_found"),
        Some(&Value::Boolean(true))
    );
}

#[tokio::test]
async fn process_spawn_missing_cwd_is_not_command_not_found() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let missing_cwd = tmp.path().join("missing");
    let error = run(Value::Map(vec![
        (
            Value::String("cmd".into()),
            Value::String("/bin/true".into()),
        ),
        (
            Value::String("cwd".into()),
            Value::String(missing_cwd.to_string_lossy().into_owned().into()),
        ),
    ]))
    .await
    .expect_err("missing cwd should fail to spawn");

    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    assert_eq!(
        map_get(error.data.as_ref().unwrap(), "os_errno").and_then(Value::as_i64),
        Some(i64::from(libc::ENOENT))
    );
    assert_eq!(
        map_get(error.data.as_ref().unwrap(), "spawn_not_found"),
        Some(&Value::Boolean(false))
    );
}

#[tokio::test]
async fn read_size_limit_rejects_oversized_request() {
    let error = read(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(1.into())),
        (
            Value::String("max_bytes".into()),
            Value::Integer(((MAX_PROCESS_READ_BYTES + 1) as u64).into()),
        ),
    ]))
    .await
    .expect_err("oversized process read should be rejected");

    assert_eq!(error.code, RpcError::INVALID_PARAMS);
}

#[tokio::test]
async fn process_read_drains_pipes_after_status_reports_exit() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("printf abc; printf XYZ >&2").await;
    wait_for_child_exit(pid).await;

    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 1).await;
    assert!(!get_process_map().lock().await.contains_key(&pid));
    assert_eq!(stdout, b"abc");
    assert_eq!(stderr, b"XYZ");
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn process_read_waits_for_eof_after_child_exit() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("printf first; sleep 0.1; printf second").await;

    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;
    assert_eq!(stdout, b"firstsecond");
    assert!(stderr.is_empty());
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn process_read_drains_output_larger_than_max_bytes() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("printf 0123456789abcdef").await;
    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 3).await;

    assert_eq!(stdout, b"0123456789abcdef");
    assert!(stderr.is_empty());
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn process_read_returns_stdout_before_idle_stderr_timeout() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("printf stdout; sleep 1").await;

    let started = std::time::Instant::now();
    let first = read_pipe_process(pid, 65_536, 500).await;
    assert!(
        started.elapsed() < std::time::Duration::from_millis(250),
        "stdout was delayed behind the idle stderr timeout"
    );
    assert_eq!(
        map_get(&first, "stdout"),
        Some(&Value::Binary(b"stdout".to_vec()))
    );
    assert_eq!(map_get(&first, "stderr"), Some(&Value::Nil));
    assert_eq!(
        map_get(&first, "exited").and_then(Value::as_bool),
        Some(false)
    );

    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;
    assert!(stdout.is_empty());
    assert!(stderr.is_empty());
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn process_read_delivers_output_written_immediately_before_exit() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("sleep 0.05; printf final").await;
    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;

    assert_eq!(stdout, b"final");
    assert!(stderr.is_empty());
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn process_read_drains_stdout_and_stderr_separately() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("printf stdout; printf stderr >&2").await;
    let (stdout, stderr, exit_code) = collect_pipe_output(pid, 65_536).await;

    assert_eq!(stdout, b"stdout");
    assert_eq!(stderr, b"stderr");
    assert_eq!(exit_code, 0);
}

#[tokio::test]
async fn pipe_sigkill_publishes_status_for_in_flight_read_after_removal() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("sleep 30").await;

    let (stdout, shared_exit_status) = {
        let processes = get_process_map().lock().await;
        let managed = processes.get(&pid).expect("managed pipe process");
        (managed.stdout.clone(), managed.shared_exit_status.clone())
    };
    let stdout_guard = stdout.lock().await;
    let read_task = tokio::spawn(read(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("timeout_ms".into()),
            Value::Integer(500.into()),
        ),
    ])));
    for _ in 0..100 {
        if Arc::strong_count(&shared_exit_status) >= 3 {
            break;
        }
        tokio::task::yield_now().await;
    }
    assert!(
        Arc::strong_count(&shared_exit_status) >= 3,
        "read never captured managed process state"
    );

    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL");
    assert!(!get_process_map().lock().await.contains_key(&pid));
    drop(stdout_guard);

    let read_result = read_task.await.expect("join read").expect("read process");
    assert_eq!(map_get(&read_result, "exited"), Some(&Value::Boolean(true)));
    assert_eq!(
        map_get(&read_result, "exit_code").and_then(Value::as_i64),
        Some(128 + libc::SIGKILL as i64)
    );
}

#[tokio::test]
async fn concurrent_pipe_reads_do_not_lose_consumed_output() {
    let _test_lock = test_process_map_lock().await;
    for _ in 0..25 {
        let pid = start_pipe_process("sleep 0.01; printf final").await;
        let (first, second) = tokio::join!(
            read_pipe_process(pid, 65_536, 500),
            read_pipe_process(pid, 65_536, 500)
        );
        let tail = if get_process_map().lock().await.contains_key(&pid) {
            collect_pipe_output(pid, 65_536).await.0
        } else {
            Vec::new()
        };
        let mut stdout = Vec::new();
        for result in [&first, &second] {
            if let Some(Value::Binary(bytes)) = map_get(result, "stdout") {
                stdout.extend_from_slice(bytes);
            }
        }
        stdout.extend_from_slice(&tail);
        assert_eq!(stdout, b"final");
    }
}

#[tokio::test]
async fn kill_default_term_and_explicit_kill_reap_process_group() {
    let _test_lock = test_process_map_lock().await;
    let start_result = start(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(
                    "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(30)"
                        .into(),
                ),
            ]),
        ),
    ]))
    .await
    .expect("start ignoring process");
    let pid = map_get(&start_result, "pid")
        .and_then(Value::as_u64)
        .unwrap() as u32;
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;

    // Delivery is success even when the child ignores the signal,
    // matching local `signal-process'; the entry stays for escalation.
    kill(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("ignored SIGTERM is still delivered successfully");
    assert!(get_process_map().lock().await.contains_key(&pid));

    let os_pid = pipe_os_pid(pid).await;
    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL");
    assert_reaped(os_pid);
    assert!(!get_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn kill_rejects_unknown_pid_and_signal_zero_returns_promptly() {
    let _test_lock = test_process_map_lock().await;
    let unknown = kill(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(u32::MAX.into()),
    )]))
    .await
    .expect_err("unknown PID must fail");
    assert_eq!(unknown.code, RpcError::PROCESS_ERROR);

    let pid = start_pipe_process("sleep 30").await;
    tokio::time::timeout(
        std::time::Duration::from_millis(250),
        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (Value::String("signal".into()), Value::Integer(0.into())),
        ])),
    )
    .await
    .expect("signal zero must not wait for process exit")
    .expect("signal zero");
    assert!(get_process_map().lock().await.contains_key(&pid));
    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("cleanup process");
}

#[tokio::test]
async fn status_and_list_serialize_with_kill_reaping() {
    let _test_lock = test_process_map_lock().await;
    for _ in 0..25 {
        let pid = start_pipe_process("sleep 30").await;
        let kill_params = Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGTERM as i64).into()),
            ),
        ]);
        let status_params = Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]);
        let (kill_result, status_result, list_result) = tokio::join!(
            kill(kill_params),
            status(status_params),
            list(Value::Map(vec![]))
        );
        kill_result.expect("kill process");
        status_result.expect("status must not race with reaping");
        list_result.expect("list must not race with reaping");
        let _ = collect_pipe_output(pid, 65_536).await;
    }
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
async fn kill_unknown_pid_is_an_error() {
    let _test_lock = test_process_map_lock().await;
    let error = kill(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(4_000_000_000u32.into()),
    )]))
    .await
    .expect_err("kill of unknown pid should fail");
    assert_eq!(error.code, RpcError::PROCESS_ERROR);

    let error = kill_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(4_000_000_000u32.into()),
    )]))
    .await
    .expect_err("kill_pty of unknown pid should fail");
    assert_eq!(error.code, RpcError::PROCESS_ERROR);
}

#[tokio::test]
async fn pipe_and_pty_kill_validate_signals_consistently() {
    let _test_lock = test_process_map_lock().await;
    let pipe_pid = start_pipe_process("sleep 30").await;
    let pty = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start PTY");
    let pty_pid = map_get(&pty, "pid")
        .and_then(Value::as_u64)
        .expect("PTY pid") as u32;

    let pipe_error = kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pipe_pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer(i32::MAX.into()),
        ),
    ]))
    .await
    .expect_err("invalid pipe signal must fail validation");
    let pty_error = kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pty_pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer(i32::MAX.into()),
        ),
    ]))
    .await
    .expect_err("invalid PTY signal must fail validation");
    assert_eq!(pipe_error.code, RpcError::INVALID_PARAMS);
    assert_eq!(pty_error.code, RpcError::INVALID_PARAMS);

    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pipe_pid.into())),
        (Value::String("signal".into()), Value::Integer(0.into())),
    ]))
    .await
    .expect("pipe signal zero");
    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pty_pid.into())),
        (Value::String("signal".into()), Value::Integer(0.into())),
    ]))
    .await
    .expect("PTY signal zero");

    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pipe_pid.into())),
        (
            Value::String("signal".into()),
            Value::String("SIGKILL".into()),
        ),
    ]))
    .await
    .expect("cleanup pipe with named signal");
    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pty_pid.into())),
        (Value::String("signal".into()), Value::String("KILL".into())),
    ]))
    .await
    .expect("cleanup PTY");
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pty_pid.into()),
    )]))
    .await
    .expect("remove PTY after signal validation");
}

#[tokio::test]
async fn repeated_kill_after_reap_preserves_exit_status() {
    let _test_lock = test_process_map_lock().await;
    let pid = start_pipe_process("sleep 30").await;
    tokio::time::sleep(std::time::Duration::from_millis(50)).await;
    for _ in 0..2 {
        kill(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("SIGTERM");
    }
    let (_, _, exit_code) = collect_pipe_output(pid, 65_536).await;
    assert_eq!(exit_code, 128 + libc::SIGTERM as i64);
}

#[tokio::test]
async fn pty_sigint_survival_keeps_subsequent_writes_open() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(
                    "trap '' INT; printf 'ready\\n'; IFS= read -r input; printf 'received:%s\\n' \"$input\""
                        .into(),
                ),
            ]),
        ),
    ]))
    .await
    .expect("start SIGINT-surviving PTY");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;

    let ready = read_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("timeout_ms".into()),
            Value::Integer(1_000.into()),
        ),
    ]))
    .await
    .expect("read PTY readiness");
    assert!(
        matches!(map_get(&ready, "output"), Some(Value::Binary(output)) if output.windows(5).any(|window| window == b"ready"))
    );

    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGINT as i64).into()),
        ),
    ]))
    .await
    .expect("forward SIGINT without escalating");

    let write = write_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("data".into()),
            Value::Binary(b"after-int\n".to_vec()),
        ),
    ]))
    .await
    .expect("write after survivable SIGINT");
    assert_eq!(map_get(&write, "written").and_then(Value::as_u64), Some(10));

    let mut output = Vec::new();
    let mut exited = false;
    for _ in 0..40 {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(100.into()),
            ),
        ]))
        .await
        .expect("read PTY response");
        if let Some(Value::Binary(chunk)) = map_get(&read, "output") {
            output.extend_from_slice(chunk);
        }
        exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
        if exited {
            break;
        }
    }
    assert!(
        output
            .windows(b"received:after-int".len())
            .any(|window| window == b"received:after-int"),
        "missing shell response in PTY output: {:?}",
        String::from_utf8_lossy(&output)
    );
    assert!(exited, "PTY should exit after processing its input");
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn repeated_pty_kill_after_reap_preserves_exit_status() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("sleep 30".into()),
            ]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    tokio::time::sleep(std::time::Duration::from_millis(50)).await;
    for _ in 0..2 {
        kill_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGTERM as i64).into()),
            ),
        ]))
        .await
        .expect("SIGTERM");
    }
    let mut exited = false;
    let mut exit_code = None;
    for _ in 0..40 {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(100.into()),
            ),
        ]))
        .await
        .expect("read pty");
        exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
        if exited {
            exit_code = map_get(&read, "exit_code").and_then(Value::as_i64);
            break;
        }
    }
    assert!(exited);
    assert_eq!(exit_code, Some(128 + libc::SIGTERM as i64));
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn pty_sigkill_publishes_status_for_in_flight_read_after_removal() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start PTY");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    let (lifecycle, io, shared_exit_status) = {
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&pid).expect("managed PTY process");
        (
            managed.lifecycle.clone(),
            managed.io.clone(),
            managed.shared_exit_status.clone(),
        )
    };
    let lifecycle_guard = lifecycle.lock().await;
    let kill_task = tokio::spawn(kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ])));
    for _ in 0..100 {
        if io.is_closed() {
            break;
        }
        tokio::task::yield_now().await;
    }
    assert!(io.is_closed(), "SIGKILL did not publish PTY cancellation");

    let read_task = tokio::spawn(read_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("timeout_ms".into()),
            Value::Integer(500.into()),
        ),
    ])));
    for _ in 0..100 {
        if Arc::strong_count(&shared_exit_status) >= 4 {
            break;
        }
        tokio::task::yield_now().await;
    }
    assert!(
        Arc::strong_count(&shared_exit_status) >= 4,
        "read never captured managed PTY state"
    );
    drop(lifecycle_guard);

    kill_task.await.expect("join SIGKILL").expect("SIGKILL PTY");
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    let read_result = read_task.await.expect("join read").expect("read PTY");
    assert_eq!(map_get(&read_result, "exited"), Some(&Value::Boolean(true)));
    assert_eq!(
        map_get(&read_result, "exit_code").and_then(Value::as_i64),
        Some(128 + libc::SIGKILL as i64)
    );
}

#[tokio::test]
async fn pty_sigkill_retains_status_for_follow_up_read() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start PTY");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;

    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL PTY");
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));

    let read = read_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("read retained PTY status");
    assert_eq!(map_get(&read, "exited"), Some(&Value::Boolean(true)));
    assert_eq!(
        map_get(&read, "exit_code").and_then(Value::as_i64),
        Some(128 + libc::SIGKILL as i64)
    );
    assert_eq!(take_terminated_pty_status(pid), None);
}

#[tokio::test]
async fn pty_kill_ignored_sigterm_then_sigkill_reaps_child() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary PTY directory");
    let marker = temp.path().join("ready");
    let pid = start_signal_ignoring_pty(&marker).await;
    let os_pid = pty_os_pid(pid).await;

    // Delivery is success even when the child ignores the signal.
    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGTERM as i64).into()),
        ),
    ]))
    .await
    .expect("ignored SIGTERM is still delivered successfully");
    assert!(get_pty_process_map().lock().await.contains_key(&pid));

    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL should terminate and reap the PTY child");
    assert_reaped(os_pid);
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    let unknown = kill_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect_err("unknown PTY PID must fail");
    assert_eq!(unknown.code, RpcError::PROCESS_ERROR);
}

#[tokio::test]
async fn pty_list_serializes_with_kill_reaping() {
    let _test_lock = test_process_map_lock().await;
    for _ in 0..25 {
        let start = start_pty(Value::Map(vec![
            (Value::String("cmd".into()), Value::String("sleep".into())),
            (
                Value::String("args".into()),
                Value::Array(vec![Value::String("30".into())]),
            ),
        ]))
        .await
        .expect("start PTY");
        let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
        let (kill_result, list_result) = tokio::join!(
            kill_pty(Value::Map(vec![(
                Value::String("pid".into()),
                Value::Integer(pid.into()),
            )])),
            list_pty(Value::Map(vec![]))
        );
        kill_result.expect("kill PTY");
        list_result.expect("PTY list must not race with reaping");
        close_pty(Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )]))
        .await
        .expect("close PTY");
    }
}

#[tokio::test]
async fn pipe_kill_preserves_output_after_term_and_drains_descendant_fds() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("ready");
    // Spawn python directly: Ubuntu's dash does not exec a single `-c`
    // command, which would leave the shell as the direct child to die
    // with a raw SIGTERM (exit 143) instead of python's graceful handler.
    let script = format!(
        "import os,signal,time; signal.signal(signal.SIGTERM, lambda *_: (os.write(1,b\"final\"), os._exit(0))); os.fork(); open(\"{}\",\"w\").close(); time.sleep(30)",
        marker.display()
    );
    let start_result = start(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start python child");
    let pid = map_get(&start_result, "pid")
        .and_then(Value::as_u64)
        .expect("process pid") as u32;
    wait_for_marker(&marker).await;
    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGTERM as i64).into()),
        ),
    ]))
    .await
    .expect("SIGTERM");

    assert!(get_process_map().lock().await.contains_key(&pid));
    let (stdout, _, exit_code) = collect_pipe_output(pid, 65_536).await;
    assert!(
        stdout
            .windows(b"final".len())
            .any(|chunk| chunk == b"final")
    );
    assert_eq!(exit_code, 0);
    assert!(!get_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn cleanup_kills_pipe_descendants_after_status_reaps_direct_child() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("descendant-pid");
    let script = format!(
        "python3 -c 'import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(30)' & echo $! > '{}'",
        marker.display()
    );
    let pid = start_pipe_process(&script).await;
    wait_for_marker(&marker).await;
    wait_for_child_exit(pid).await;

    let descendant_pid: i32 = std::fs::read_to_string(&marker)
        .expect("read descendant PID")
        .trim()
        .parse()
        .expect("parse descendant PID");
    assert_eq!(
        rustix::process::test_kill_process(
            RustixPid::from_raw(descendant_pid).expect("descendant pid")
        ),
        Ok(())
    );

    cleanup_managed_processes()
        .await
        .expect("cleanup pipe descendants");
    wait_for_process_exit(descendant_pid).await;
}

#[tokio::test]
async fn cleanup_gives_term_ignoring_pty_group_its_full_grace_period() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary PTY marker directory");
    let marker = temp.path().join("ready");
    let pid = start_signal_ignoring_pty(&marker).await;
    let os_pid = pty_os_pid(pid).await;

    let started = tokio::time::Instant::now();
    cleanup_managed_processes()
        .await
        .expect("cleanup PTY group");
    // Direct-child reaping gets one bounded wait and the process group
    // gets a separate one.  Leave scheduling slack while still rejecting
    // the old one-wait escalation behavior.
    assert!(
        started.elapsed() >= std::time::Duration::from_millis(3_500),
        "PTY process-group grace was consumed by direct-child reap wait"
    );
    assert_reaped(os_pid);
    assert!(get_pty_process_map().lock().await.is_empty());
}

#[tokio::test]
async fn cleanup_gives_term_ignoring_pipe_group_its_full_grace_period() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("ready");
    let script = format!(
        "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); open({marker:?}, 'w').close(); time.sleep(30)"
    );
    let start_result = start(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start TERM-ignoring child");
    let pid = map_get(&start_result, "pid")
        .and_then(Value::as_u64)
        .expect("process pid") as u32;
    let os_pid = pipe_os_pid(pid).await;
    wait_for_marker(&marker).await;

    let started = tokio::time::Instant::now();
    cleanup_managed_processes()
        .await
        .expect("cleanup pipe group");
    // Direct-child reaping gets one bounded wait and the process group
    // gets a separate one.  Leave scheduling slack while still rejecting
    // the old one-wait escalation behavior.
    assert!(
        started.elapsed() >= std::time::Duration::from_millis(850),
        "process-group grace was consumed by direct-child reap wait"
    );
    assert_reaped(os_pid);
    assert!(get_process_map().lock().await.is_empty());
}

#[tokio::test]
async fn cleanup_kills_pty_descendants_after_status_reaps_direct_child() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary PTY marker directory");
    let marker = temp.path().join("pty-descendant-pid");
    let script = format!(
        "import os,signal,time; pid=os.fork(); (open({marker:?},'w').write(str(pid)), os._exit(0)) if pid else (signal.signal(signal.SIGHUP, signal.SIG_IGN), signal.signal(signal.SIGTERM, signal.SIG_IGN), time.sleep(30))"
    );
    let start_result = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start PTY parent");
    let pid = map_get(&start_result, "pid")
        .and_then(Value::as_u64)
        .expect("PTY pid") as u32;
    wait_for_marker(&marker).await;

    for _ in 0..100 {
        let exited = {
            let mut processes = get_pty_process_map().lock().await;
            let managed = processes.get_mut(&pid).expect("managed PTY");
            check_exit_status(managed).0
        };
        if exited {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
    assert!(
        get_pty_process_map()
            .lock()
            .await
            .get(&pid)
            .and_then(|managed| managed.exit_status)
            .is_some(),
        "direct PTY child should be reaped before cleanup"
    );

    let descendant_pid: i32 = std::fs::read_to_string(&marker)
        .expect("read PTY descendant PID")
        .trim()
        .parse()
        .expect("parse PTY descendant PID");
    assert_eq!(
        rustix::process::test_kill_process(
            RustixPid::from_raw(descendant_pid).expect("descendant pid")
        ),
        Ok(())
    );

    cleanup_managed_processes()
        .await
        .expect("cleanup PTY descendants");
    wait_for_process_exit(descendant_pid).await;
}

#[tokio::test]
async fn pipe_sigkill_discards_unread_output_after_reaping_direct_child() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("ready");
    let script = format!(
        "python3 -c 'import os,time; os.write(1,b\"pending\"); os.fork(); open(\"{}\",\"w\").close(); time.sleep(30)'",
        marker.display()
    );
    let pid = start_pipe_process(&script).await;
    wait_for_marker(&marker).await;
    let os_pid = pipe_os_pid(pid).await;
    kill(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL");

    assert_reaped(os_pid);
    assert!(!get_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn pty_kill_preserves_output_until_terminal_eof() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("ready");
    let script = format!(
        "trap 'printf final; exit 0' TERM; sleep 30 & child=$!; touch '{}'; wait \"$child\"",
        marker.display()
    );
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String(script.into()),
            ]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    wait_for_marker(&marker).await;
    let os_pid = {
        get_pty_process_map()
            .lock()
            .await
            .get(&pid)
            .expect("pty process")
            .child_pid
            .as_raw()
    };

    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGTERM as i64).into()),
        ),
    ]))
    .await
    .expect("SIGTERM");
    assert!(get_pty_process_map().lock().await.contains_key(&pid));

    let mut output = Vec::new();
    let mut exited = false;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
    while tokio::time::Instant::now() < deadline {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(100.into()),
            ),
        ]))
        .await
        .expect("read pty");
        if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
            output.extend_from_slice(bytes);
        }
        exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
        if exited {
            break;
        }
        // Some PTY backends report immediate readability while process
        // teardown is still in progress; avoid exhausting a fixed poll count
        // before the exit status becomes observable.
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    }
    assert!(exited);
    assert!(
        output
            .windows(b"final".len())
            .any(|chunk| chunk == b"final")
    );
    // Reaping is asserted only after the terminal read: kill's own reap
    // is a bounded opportunistic wait that a slow runner can outlast.
    assert_reaped(os_pid);
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn pty_read_removes_entry_after_terminal_eof() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("printf final".into()),
            ]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    let mut output = Vec::new();
    let mut exited = false;
    for _ in 0..40 {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(100.into()),
            ),
        ]))
        .await
        .expect("read pty");
        if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
            output.extend_from_slice(bytes);
        }
        exited = map_get(&read, "exited").and_then(Value::as_bool) == Some(true);
        if exited {
            break;
        }
    }
    assert!(exited);
    // Idle and terminal reads must return exactly the child's bytes:
    // a full-length zeroed buffer leaking through pads every response
    // with `max_bytes` NUL bytes.
    assert_eq!(output, b"final");
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn read_pty_idle_poll_returns_no_output() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary marker directory");
    let marker = temp.path().join("ready");
    let pid = start_signal_ignoring_pty(&marker).await;

    // The child produces no output; both the immediate and the blocking
    // poll must report empty output, not NUL padding.
    for timeout_ms in [0i64, 50] {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(timeout_ms.into()),
            ),
        ]))
        .await
        .expect("read pty");
        assert!(
            matches!(map_get(&read, "output"), None | Some(Value::Nil)),
            "idle PTY read must not fabricate output: {read:?}"
        );
        assert_eq!(
            map_get(&read, "exited").and_then(Value::as_bool),
            Some(false)
        );
    }

    kill_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("signal".into()),
            Value::Integer((libc::SIGKILL as i64).into()),
        ),
    ]))
    .await
    .expect("SIGKILL");
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("close PTY");
}

#[tokio::test]
async fn pty_read_drains_output_larger_than_max_bytes_after_exit() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("python3".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("import os; os.write(1, b'x' * 131072)".into()),
            ]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    let mut output = Vec::new();
    let mut exited = false;
    // macOS TTYs hand out at most ~1 KiB per read, so draining 128 KiB
    // takes well over a hundred reads; size the loop for that, not for
    // Linux's larger PTY chunks.
    for _ in 0..512 {
        let result = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("max_bytes".into()),
                Value::Integer(4096.into()),
            ),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(500.into()),
            ),
        ]))
        .await
        .expect("read pty output");
        if let Some(Value::Binary(bytes)) = map_get(&result, "output") {
            output.extend_from_slice(bytes);
        }
        exited = map_get(&result, "exited").and_then(Value::as_bool) == Some(true);
        if exited {
            break;
        }
    }
    assert!(exited);
    assert_eq!(output.len(), 131072);
    assert!(output.iter().all(|byte| *byte == b'x'));
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn pty_read_errors_are_terminal_process_errors() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;

    let (_read_end, write_end) = rustix::pipe::pipe().expect("create pipe");
    set_fd_nonblocking(&write_end).expect("make test fd nonblocking");
    let async_fd = AsyncFd::new(write_end).expect("monitor test fd");
    get_pty_process_map()
        .lock()
        .await
        .get_mut(&pid)
        .expect("PTY process")
        .async_fd = async_fd;

    let error = read_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect_err("reading a write-only descriptor should fail");
    assert_eq!(error.code, RpcError::PROCESS_ERROR);
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("close broken PTY");
}

#[tokio::test]
async fn close_pty_is_idempotent_and_reaps_child() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("printf pending; sleep 30".into()),
            ]),
        ),
    ]))
    .await
    .expect("start pty");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("first close");
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("second close");
    assert!(!get_pty_process_map().lock().await.contains_key(&pid));
}

#[tokio::test]
async fn pty_writes_are_serialized_in_acquisition_order() {
    let io = Arc::new(PtyIoState {
        write_lock: Mutex::new(()),
        syscall_lock: StdMutex::new(()),
        closed: AtomicBool::new(false),
        cancelled: Notify::new(),
    });
    let first_acquired = Arc::new(Notify::new());
    let release_first = Arc::new(Notify::new());
    let order = Arc::new(Mutex::new(Vec::new()));

    let first = {
        let io = io.clone();
        let first_acquired = first_acquired.clone();
        let release_first = release_first.clone();
        let order = order.clone();
        tokio::spawn(async move {
            let _guard = io.write_lock.lock().await;
            order.lock().await.push(1);
            first_acquired.notify_one();
            release_first.notified().await;
            order.lock().await.push(2);
        })
    };
    first_acquired.notified().await;

    let second = {
        let io = io.clone();
        let order = order.clone();
        tokio::spawn(async move {
            let _guard = io.write_lock.lock().await;
            order.lock().await.push(3);
        })
    };
    tokio::task::yield_now().await;
    assert_eq!(*order.lock().await, vec![1]);
    release_first.notify_one();
    first.await.expect("first writer");
    second.await.expect("second writer");
    assert_eq!(*order.lock().await, vec![1, 2, 3]);
}

#[test]
fn pty_write_loop_handles_partial_eintr_and_eagain() {
    let mut offset = 0;
    let mut readiness_waits = 0;
    let script = [
        Ok(2),
        Err(std::io::Error::from_raw_os_error(libc::EINTR)),
        Err(std::io::Error::from_raw_os_error(libc::EAGAIN)),
        Ok(3),
    ];
    for result in script {
        match apply_pty_write(&mut offset, 5, result).expect("scripted write") {
            PtyWriteAction::Progress => {}
            PtyWriteAction::Retry => readiness_waits += 1,
        }
    }
    assert_eq!(offset, 5);
    assert_eq!(readiness_waits, 2);
}

#[tokio::test]
async fn pty_fd_duplication_survives_concurrent_read_write_close() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start PTY");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    let read = tokio::spawn(read_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("timeout_ms".into()),
            Value::Integer(1_000.into()),
        ),
    ])));
    let write = tokio::spawn(write_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (
            Value::String("data".into()),
            Value::Binary(b"concurrent".to_vec()),
        ),
    ])));
    tokio::task::yield_now().await;
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("close PTY");

    let read_result = tokio::time::timeout(std::time::Duration::from_secs(1), read)
        .await
        .expect("duplicated read should finish")
        .expect("read task should join");
    let write_result = tokio::time::timeout(std::time::Duration::from_secs(1), write)
        .await
        .expect("duplicated write should finish")
        .expect("write task should join");
    assert!(matches!(
        read_result,
        Ok(_)
            | Err(RpcError {
                code: RpcError::PROCESS_ERROR,
                ..
            })
    ));
    assert!(matches!(
        write_result,
        Ok(_)
            | Err(RpcError {
                code: RpcError::PROCESS_ERROR,
                ..
            })
    ));
    assert!(test_managed_maps_empty().await);
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn pty_read_close_race_does_not_leak_duplicated_fds() {
    let _test_lock = test_process_map_lock().await;

    for iteration in 0..100 {
        let pid = u32::MAX - iteration;
        let (read_end, write_end) = rustix::pipe::pipe().expect("create pipe");
        drop(read_end);
        set_fd_nonblocking(&write_end).expect("make test fd nonblocking");
        let fd_target = std::fs::read_link(format!("/proc/self/fd/{}", write_end.as_raw_fd()))
            .expect("resolve test fd target");

        let lifecycle = Arc::new(Mutex::new(()));
        let lifecycle_guard = lifecycle.lock().await;
        get_pty_process_map().lock().await.insert(
            pid,
            ManagedPtyProcess {
                async_fd: AsyncFd::new(write_end).expect("monitor test fd"),
                lifecycle: lifecycle.clone(),
                io: Arc::new(PtyIoState {
                    write_lock: Mutex::new(()),
                    syscall_lock: StdMutex::new(()),
                    closed: AtomicBool::new(false),
                    cancelled: Notify::new(),
                }),
                child_pid: Pid::from_raw(-1),
                cmd: String::new(),
                exit_status: None,
                shared_exit_status: Arc::new(StdMutex::new(None)),
                output_eof: false,
            },
        );
        assert_eq!(fd_target_count(&fd_target), 1);
        let reader = tokio::spawn(read_pty_now(pid, 1));

        for _ in 0..100 {
            if fd_target_count(&fd_target) == 2 {
                break;
            }
            tokio::task::yield_now().await;
        }
        assert_eq!(
            fd_target_count(&fd_target),
            2,
            "read did not duplicate the PTY descriptor"
        );
        assert!(get_pty_process_map().lock().await.remove(&pid).is_some());
        drop(lifecycle_guard);

        let result = tokio::time::timeout(std::time::Duration::from_secs(1), reader)
            .await
            .expect("read should finish after PTY removal")
            .expect("read task should join")
            .expect("read should report removed PTY");
        assert!(result.exited);
        assert_eq!(
            fd_target_count(&fd_target),
            0,
            "iteration {iteration} leaked an fd"
        );
    }
}

#[tokio::test]
async fn pty_write_completes_large_data_under_backpressure() {
    let _test_lock = test_process_map_lock().await;
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("sleep".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![Value::String("30".into())]),
        ),
    ]))
    .await
    .expect("start backpressure PTY");
    let pid = map_get(&start, "pid").and_then(Value::as_u64).unwrap() as u32;
    let read_fd = install_full_pipe_pty(pid).await;

    // Remove the synthetic fill without sleeping; the writer starts with
    // no available capacity and must therefore exercise partial writes.
    let mut scratch = [0u8; 8192];
    loop {
        if let Err(errno) = rustix::io::read(&read_fd, scratch.as_mut_slice()) {
            assert_eq!(errno, rustix::io::Errno::AGAIN);
            break;
        }
    }

    let reader_fd = AsyncFd::new(read_fd).expect("monitor backpressure reader");
    let data = vec![b'z'; 256 * 1024];
    let expected = data.clone();
    let reader_expected = expected.clone();
    let reader = tokio::spawn(async move {
        let mut output = Vec::with_capacity(reader_expected.len());
        while output.len() < reader_expected.len() {
            let mut guard = reader_fd
                .readable()
                .await
                .expect("backpressure reader readiness");
            match guard.try_io(|inner| {
                let mut chunk = [0u8; 8192];
                let read = rustix::io::read(inner.get_ref(), chunk.as_mut_slice())?;
                Ok(chunk[..read].to_vec())
            }) {
                Ok(Ok(chunk)) => output.extend_from_slice(&chunk),
                Ok(Err(error)) if error.kind() == ErrorKind::Interrupted => {}
                Ok(Err(error)) => panic!("backpressure reader: {error}"),
                Err(_) => {}
            }
        }
        output
    });
    let writer = tokio::spawn(write_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(pid.into())),
        (Value::String("data".into()), Value::Binary(data)),
    ])));

    let result = tokio::time::timeout(std::time::Duration::from_secs(2), writer)
        .await
        .expect("large PTY write should complete")
        .expect("large PTY writer should join")
        .expect("large PTY write should succeed");
    assert_eq!(
        map_get(&result, "written").and_then(Value::as_u64),
        Some(262144)
    );
    let output = tokio::time::timeout(std::time::Duration::from_secs(2), reader)
        .await
        .expect("backpressure reader should complete")
        .expect("backpressure reader should join");
    assert_eq!(output, expected);
    close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await
    .expect("close backpressure PTY");
}

#[tokio::test]
async fn pty_blocked_write_does_not_hold_registry_for_kill_or_close() {
    let _test_lock = test_process_map_lock().await;
    let temp = tempfile::tempdir().expect("temporary PTY directory");

    for close in [false, true] {
        let marker = temp
            .path()
            .join(if close { "close-ready" } else { "kill-ready" });
        let pid = start_signal_ignoring_pty(&marker).await;
        let os_pid = pty_os_pid(pid).await;
        // Keep the read end open so the full synthetic write end remains
        // at EAGAIN while write_pty waits for WRITABLE readiness.
        let _pipe_reader = install_full_pipe_pty(pid).await;
        let mut blocked_write = tokio::spawn(write_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (Value::String("data".into()), Value::Binary(vec![b'x'])),
        ])));
        let write_wait =
            tokio::time::timeout(std::time::Duration::from_millis(100), &mut blocked_write).await;

        let lifecycle_result = if close {
            tokio::time::timeout(
                std::time::Duration::from_secs(1),
                close_pty(Value::Map(vec![(
                    Value::String("pid".into()),
                    Value::Integer(pid.into()),
                )])),
            )
            .await
        } else {
            tokio::time::timeout(
                std::time::Duration::from_secs(1),
                kill_pty(Value::Map(vec![
                    (Value::String("pid".into()), Value::Integer(pid.into())),
                    (
                        Value::String("signal".into()),
                        Value::Integer((libc::SIGKILL as i64).into()),
                    ),
                ])),
            )
            .await
        };

        let writer_result =
            tokio::time::timeout(std::time::Duration::from_secs(1), &mut blocked_write).await;
        // Remove the drain-preserving entry left by kill.  This is also
        // harmless after close, which is deliberately idempotent.
        let close_result = tokio::time::timeout(
            std::time::Duration::from_secs(1),
            close_pty(Value::Map(vec![(
                Value::String("pid".into()),
                Value::Integer(pid.into()),
            )])),
        )
        .await;

        assert!(write_wait.is_err(), "PTY write should wait for readiness");
        lifecycle_result
            .expect("PTY lifecycle operation should not wait for write")
            .expect("PTY lifecycle operation should succeed");
        match writer_result.expect("cancelled writer should join") {
            Ok(Err(error)) => assert_eq!(error.code, RpcError::PROCESS_ERROR),
            Err(_) => {}
            Ok(Ok(_)) => panic!("writer should be cancelled"),
        }
        close_result
            .expect("PTY cleanup should be bounded")
            .expect("PTY cleanup should succeed");
        assert_reaped(os_pid);
        assert!(!get_pty_process_map().lock().await.contains_key(&pid));
    }
}

#[tokio::test]
async fn read_pty_rejects_oversized_max_bytes() {
    let error = read_pty(Value::Map(vec![
        (Value::String("pid".into()), Value::Integer(1.into())),
        (
            Value::String("max_bytes".into()),
            Value::Integer(((MAX_PROCESS_READ_BYTES + 1) as u64).into()),
        ),
    ]))
    .await
    .expect_err("oversized PTY read should be rejected");

    assert_eq!(error.code, RpcError::INVALID_PARAMS);
}

#[tokio::test]
async fn start_pty_applies_env_without_mutating_process_env() {
    let _test_lock = test_process_map_lock().await;
    let parent_value = std::env::var("TRAMP_RPC_PTY_TEST").ok();
    let start = start_pty(Value::Map(vec![
        (Value::String("cmd".into()), Value::String("/bin/sh".into())),
        (
            Value::String("args".into()),
            Value::Array(vec![
                Value::String("-c".into()),
                Value::String("printf %s \"$TRAMP_RPC_PTY_TEST\"; read _".into()),
            ]),
        ),
        (Value::String("clear_env".into()), Value::Boolean(true)),
        (
            Value::String("env".into()),
            Value::Map(vec![(
                Value::String("TRAMP_RPC_PTY_TEST".into()),
                Value::String("ok".into()),
            )]),
        ),
    ]))
    .await
    .expect("start pty");

    let pid = map_get(&start, "pid").and_then(Value::as_u64).expect("pid") as u32;
    let mut output = Vec::new();

    for _ in 0..5 {
        let read = read_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("timeout_ms".into()),
                Value::Integer(1_000.into()),
            ),
        ]))
        .await
        .expect("read pty");

        if let Some(Value::Binary(bytes)) = map_get(&read, "output") {
            output.extend_from_slice(bytes);
        }
        if !output.is_empty() {
            break;
        }
    }

    let _ = close_pty(Value::Map(vec![(
        Value::String("pid".into()),
        Value::Integer(pid.into()),
    )]))
    .await;

    assert_eq!(String::from_utf8_lossy(&output), "ok");
    assert_eq!(std::env::var("TRAMP_RPC_PTY_TEST").ok(), parent_value);
}
