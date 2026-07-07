//! Process execution operations

use crate::WriterHandle;
use crate::msgpack_map;
use crate::protocol::{Notification, ProcessResult, RpcError, from_value};
use nix::pty::{OpenptyResult, openpty};
use nix::sys::signal::Signal;
use nix::sys::termios::{LocalFlags, OutputFlags, SetArg, tcgetattr, tcsetattr};
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::{Pid, tcgetpgrp};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command as StdCommand, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex as StdMutex, OnceLock};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command};
use tokio::sync::{Mutex, Notify, Semaphore};
use tokio::task::JoinHandle;

use super::HandlerResult;

const MAX_PROCESS_READ_BYTES: usize = 1024 * 1024;
// Bound each notification frame so process output cannot monopolize the shared
// response stream behind one large write.
const PUSH_READ_MAX_BYTES: usize = 65_536;
const PUSH_READ_TIMEOUT_MS: u64 = 200;
const _: () = assert!(PUSH_READ_MAX_BYTES <= 64 * 1024);
const _: () = assert!(PUSH_READ_MAX_BYTES < crate::MAX_FRAME_SIZE);

/// A child that exits or closes stdin before consuming all input is normal
/// shell behavior (`head`, `grep -q', a failing filter).  `tramp-sh' runs
/// `command <infile', where the resulting SIGPIPE/EPIPE is invisible to
/// Emacs, so these must not turn into RPC errors.
pub(crate) fn is_benign_stdin_error(error: &std::io::Error) -> bool {
    matches!(
        error.kind(),
        ErrorKind::BrokenPipe | ErrorKind::WriteZero | ErrorKind::ConnectionReset
    )
}

pub(crate) async fn read_sync_output<R>(
    mut reader: R,
    remaining: Arc<Semaphore>,
    output_limit: usize,
) -> std::io::Result<Vec<u8>>
where
    R: AsyncRead + Unpin,
{
    let mut output = Vec::new();
    let mut buffer = [0u8; 8192];
    loop {
        let read = reader.read(&mut buffer).await?;
        if read == 0 {
            return Ok(output);
        }
        let permits = u32::try_from(read).expect("output buffer length fits in u32");
        let permit = remaining.try_acquire_many(permits).map_err(|_| {
            std::io::Error::other(format!("Process output exceeds {output_limit} byte limit"))
        })?;
        // Consumed permits represent bytes retained in the response buffers.
        permit.forget();
        output.extend_from_slice(&buffer[..read]);
    }
}

fn command_path(command: &str, cwd: Option<&str>) -> PathBuf {
    let path = Path::new(command);
    if path.is_relative() && command.contains('/') {
        cwd.map_or_else(
            || path.to_path_buf(),
            |dir| PathBuf::from(super::expand_tilde(dir)).join(path),
        )
    } else {
        path.to_path_buf()
    }
}

fn executable_is_missing_sync(
    command: &str,
    cwd: Option<&str>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    if cwd.is_some_and(|dir| !Path::new(&super::expand_tilde(dir)).is_dir()) {
        return false;
    }

    if command.contains('/') {
        return matches!(
            std::fs::metadata(command_path(command, cwd)),
            Err(error) if error.kind() == ErrorKind::NotFound
        );
    }

    let path = env
        .and_then(|variables| variables.get("PATH").cloned())
        .or_else(|| (!clear_env).then(|| std::env::var("PATH").ok()).flatten())
        .unwrap_or_else(|| "/usr/bin:/bin".to_string());
    let cwd = cwd.map(|dir| PathBuf::from(super::expand_tilde(dir)));
    std::env::split_paths(&path).all(|dir| {
        let dir = if dir.is_relative() {
            cwd.as_ref().map_or(dir.clone(), |cwd| cwd.join(dir))
        } else {
            dir
        };
        !dir.join(command).is_file()
    })
}

async fn executable_is_missing(
    command: &str,
    cwd: Option<&str>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    let command = command.to_owned();
    let cwd = cwd.map(str::to_owned);
    let env = env.cloned();
    tokio::task::spawn_blocking(move || {
        executable_is_missing_sync(&command, cwd.as_deref(), env.as_ref(), clear_env)
    })
    .await
    .unwrap_or(false)
}

fn spawn_error(error: std::io::Error, executable_missing: bool) -> RpcError {
    let mut data = Vec::new();
    if let Some(errno) = error.raw_os_error() {
        data.push((
            Value::String("os_errno".into()),
            Value::Integer(errno.into()),
        ));
    }
    data.push((
        Value::String("spawn_not_found".into()),
        Value::Boolean(error.kind() == ErrorKind::NotFound && executable_missing),
    ));
    RpcError {
        code: RpcError::PROCESS_ERROR,
        message: format!("Failed to spawn process: {error}"),
        data: Some(Value::Map(data)),
    }
}

/// Own a newly spawned child process group until its request finishes.
///
/// Request tasks are aborted when their RPC transport disappears.  Tokio can
/// kill the direct child on drop, but descendants would otherwise survive, so
/// this guard synchronously kills the whole group when the request future is
/// cancelled.  It is disarmed immediately after the direct child is reaped.
pub(crate) struct ProcessGroupGuard {
    pgid: Option<u32>,
}

impl ProcessGroupGuard {
    pub(crate) fn new(pgid: u32) -> Self {
        Self { pgid: Some(pgid) }
    }

    pub(crate) fn disarm(&mut self) {
        self.pgid = None;
    }
}

impl Drop for ProcessGroupGuard {
    fn drop(&mut self) {
        if let Some(pgid) = self.pgid {
            // Best effort only: Drop cannot report an error and ESRCH means the
            // group already exited.  Negating PGID targets the whole group.
            unsafe {
                libc::kill(-(pgid as i32), libc::SIGKILL);
            }
        }
    }
}

pub(crate) fn configure_process_group(cmd: &mut Command) {
    // Keep descendants in a group owned by this request, separate from the
    // server and unrelated processes.
    unsafe {
        cmd.pre_exec(|| {
            if libc::setpgid(0, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
}

#[cfg(test)]
pub(crate) async fn wait_for_process_exit(pid: i32) {
    for _ in 0..100 {
        // Reap direct children ourselves when Tokio's best-effort drop reaper
        // has not done so yet.
        let _ = waitpid(Pid::from_raw(pid), Some(WaitPidFlag::WNOHANG));
        if !process_is_running(pid) {
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
    panic!("process {pid} is still running");
}

#[cfg(test)]
fn process_is_running(pid: i32) -> bool {
    #[cfg(target_os = "linux")]
    if let Ok(stat) = std::fs::read_to_string(format!("/proc/{pid}/stat")) {
        // A zombie is visible to kill(pid, 0), but cannot execute or survive.
        let state = stat
            .rsplit_once(") ")
            .and_then(|(_, rest)| rest.chars().next());
        return !matches!(state, Some('Z' | 'X'));
    }

    (unsafe { libc::kill(pid, 0) == 0 })
        || std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
}

// ============================================================================
// Process management for async processes
// ============================================================================

// Production starts one server OS process per RPC transport connection.  These
// process-local maps therefore cannot mix processes from separate connections;
// connection cleanup drains the maps when that one transport ends.  Tests
// serialize connection loops with `test_process_map_lock` for the same reason.
static PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedProcess>>> = OnceLock::new();
static PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();
static PROCESS_NOTIFICATION_WRITER: OnceLock<WriterHandle> = OnceLock::new();

#[cfg(test)]
static PROCESS_TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

#[cfg(test)]
pub(crate) async fn test_process_map_lock() -> tokio::sync::MutexGuard<'static, ()> {
    PROCESS_TEST_LOCK
        .get_or_init(|| Mutex::new(()))
        .lock()
        .await
}

#[cfg(test)]
pub(crate) async fn test_managed_maps_empty() -> bool {
    get_process_map().lock().await.is_empty()
        && get_pty_process_map().lock().await.is_empty()
        && TERMINATED_PTY_STATUSES
            .get_or_init(|| StdMutex::new(HashMap::new()))
            .lock()
            .expect("terminated PTY status lock")
            .is_empty()
}

#[cfg(test)]
pub(crate) async fn test_managed_os_pids() -> Vec<i32> {
    let mut pids: Vec<_> = get_process_map()
        .lock()
        .await
        .values()
        .map(|managed| managed.child_pid as i32)
        .collect();
    pids.extend(
        get_pty_process_map()
            .lock()
            .await
            .values()
            .map(|managed| managed.child_pid.as_raw()),
    );
    pids
}

fn get_process_map() -> &'static Mutex<HashMap<u32, ManagedProcess>> {
    PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

async fn get_next_pid() -> u32 {
    let counter = PID_COUNTER.get_or_init(|| Mutex::new(1));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

struct ManagedProcess {
    child: Child,
    child_pid: u32,
    lifecycle: Arc<Mutex<()>>,
    read_lock: Arc<Mutex<()>>,
    exit_status: Option<ExitStatus>,
    shared_exit_status: Arc<StdMutex<Option<ExitStatus>>>,
    stdin: Arc<Mutex<Option<ChildStdin>>>,
    stdout: Arc<Mutex<Option<ChildStdout>>>,
    stderr: Arc<Mutex<Option<ChildStderr>>>,
    cmd: String,
    push_subscription: Option<PushSubscription>,
    subscription_requested: bool,
    terminating: bool,
}

struct PushSubscription {
    stop: Arc<AtomicBool>,
    wake: Arc<Notify>,
    task: JoinHandle<()>,
}

async fn stop_push_subscription(subscription: PushSubscription) {
    subscription.stop.store(true, Ordering::Release);
    subscription.wake.notify_one();
    let _ = subscription.task.await;
}

pub fn init_notification_writer(writer: WriterHandle) {
    let _ = PROCESS_NOTIFICATION_WRITER.set(writer);
}

async fn send_process_notification(method: &str, params: Value) -> Result<(), RpcError> {
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

fn response_field<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
    value
        .as_map()?
        .iter()
        .find_map(|(candidate, value)| (candidate.as_str() == Some(key)).then_some(value))
}

fn spawn_push_subscription_task(pid: u32, stop: Arc<AtomicBool>) -> JoinHandle<()> {
    // NOTE: No wake/Notify is passed here.  The pipe read includes a bounded
    // PUSH_READ_TIMEOUT_MS timeout, so the task exits within ~200 ms of stop
    // being set.  Cancelling an in-flight read is not safe because it may
    // have consumed bytes before waiting for lifecycle ownership.
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            // Let an in-flight read finish: it may have consumed bytes before
            // waiting for lifecycle ownership, so cancellation is not safe.
            let result = read(msgpack_map! {
                "pid" => pid,
                "max_bytes" => PUSH_READ_MAX_BYTES as u64,
                "timeout_ms" => PUSH_READ_TIMEOUT_MS
            })
            .await;
            let Ok(result) = result else {
                // A read error (e.g. waitpid failure) means we cannot deliver
                // further output or a real exit code.  Send a synthetic exit
                // so the client relay is not left hanging indefinitely.
                let _ = send_process_notification(
                    "process.exit",
                    msgpack_map! { "pid" => pid, "exit_code" => -1i64 },
                )
                .await;
                break;
            };

            let stdout = response_field(&result, "stdout")
                .and_then(Value::as_slice)
                .map_or_else(Vec::new, ToOwned::to_owned);
            let stderr = response_field(&result, "stderr")
                .and_then(Value::as_slice)
                .map_or_else(Vec::new, ToOwned::to_owned);
            if !stdout.is_empty() || !stderr.is_empty() {
                let _ = send_process_notification(
                    "process.output",
                    msgpack_map! {
                        "pid" => pid,
                        "stdout" => if stdout.is_empty() { Value::Nil } else { Value::Binary(stdout) },
                        "stderr" => if stderr.is_empty() { Value::Nil } else { Value::Binary(stderr) }
                    },
                )
                .await;
                // Yield after releasing the shared writer lock so other RPC
                // response tasks can interleave between notification frames.
                tokio::task::yield_now().await;
            }

            if response_field(&result, "exited").and_then(Value::as_bool) == Some(true) {
                let exit_code = response_field(&result, "exit_code")
                    .and_then(Value::as_i64)
                    .unwrap_or(-1);
                let _ = send_process_notification(
                    "process.exit",
                    msgpack_map! {
                        "pid" => pid,
                        "exit_code" => exit_code
                    },
                )
                .await;
                break;
            }
        }
    })
}

fn spawn_pty_push_subscription_task(
    pid: u32,
    stop: Arc<AtomicBool>,
    wake: Arc<Notify>,
) -> JoinHandle<()> {
    tokio::spawn(async move {
        while !stop.load(Ordering::Acquire) {
            let result = read_pty_now(pid, PUSH_READ_MAX_BYTES).await;
            let Ok(result) = result else {
                // A read error means we cannot deliver further PTY output or a
                // real exit code.  Send a synthetic exit so the client relay is
                // not left hanging indefinitely.
                let _ = send_process_notification(
                    "process.pty_exit",
                    msgpack_map! { "pid" => pid, "exit_code" => -1i64 },
                )
                .await;
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
                // Yield after releasing the shared writer lock so other RPC
                // response tasks can interleave between PTY notification frames.
                // Without this yield a fast producer (e.g. `yes` in vterm) can
                // monopolise the writer and stall every RPC response.
                tokio::task::yield_now().await;
            }

            if result.exited {
                let _ = send_process_notification(
                    "process.pty_exit",
                    msgpack_map! {
                        "pid" => pid,
                        "exit_code" => result.exit_code.unwrap_or(-1)
                    },
                )
                .await;
                break;
            }
        }
    })
}

fn new_pipe_subscription(pid: u32) -> PushSubscription {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_push_subscription_task(pid, Arc::clone(&stop));
    PushSubscription { stop, wake, task }
}

fn new_pty_subscription(pid: u32) -> PushSubscription {
    let stop = Arc::new(AtomicBool::new(false));
    let wake = Arc::new(Notify::new());
    let task = spawn_pty_push_subscription_task(pid, Arc::clone(&stop), Arc::clone(&wake));
    PushSubscription { stop, wake, task }
}

async fn restore_pipe_after_failed_termination(pid: u32) {
    if let Some(managed) = get_process_map().lock().await.get_mut(&pid) {
        managed.terminating = false;
        if managed.subscription_requested && managed.push_subscription.is_none() {
            managed.push_subscription = Some(new_pipe_subscription(pid));
        }
    }
}

async fn restore_pty_after_failed_termination(pid: u32) {
    if let Some(managed) = get_pty_process_map().lock().await.get_mut(&pid) {
        managed.terminating = false;
        if managed.subscription_requested && managed.push_subscription.is_none() {
            managed.push_subscription = Some(new_pty_subscription(pid));
        }
    }
}

// ============================================================================
// Synchronous process execution (but async-friendly)
// ============================================================================

/// Run a command and wait for it to complete
pub async fn run(params: Value) -> HandlerResult {
    run_with_output_limit(params, crate::MAX_RESPONSE_OUTPUT_BYTES).await
}

async fn run_with_output_limit(params: Value, output_limit: usize) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Command to run
        cmd: String,
        /// Arguments
        #[serde(default)]
        args: Vec<String>,
        /// Working directory
        #[serde(default)]
        cwd: Option<String>,
        /// Environment variables to set
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        /// Stdin input as binary
        #[serde(default, with = "serde_bytes")]
        stdin: Option<Vec<u8>>,
        /// Clear environment before setting env vars
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        for (key, value) in env {
            cmd.env(key, value);
        }
    }

    // Never let a synchronous child consume the RPC transport.  A pipe is
    // only needed when the caller supplied input.
    cmd.stdin(if params.stdin.is_some() {
        Stdio::piped()
    } else {
        Stdio::null()
    });

    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    cmd.kill_on_drop(true);
    configure_process_group(&mut cmd);

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(error) => {
            let executable_missing = executable_is_missing(
                &params.cmd,
                params.cwd.as_deref(),
                params.env.as_ref(),
                params.clear_env,
            )
            .await;
            return Err(spawn_error(error, executable_missing));
        }
    };
    let child_pid = child
        .id()
        .ok_or_else(|| RpcError::process_error("Spawned process has no PID"))?;
    let mut process_group = ProcessGroupGuard::new(child_pid);

    // Drive stdin, bounded output drains, and child exit concurrently.  A
    // genuine pipe or size error cancels the other operations and kills the
    // child; a broken stdin pipe does not, because a child that stops reading
    // early (`head`, `grep -q', ...) is normal and its output must survive.
    let stdin_data = params.stdin;
    let mut stdin = child.stdin.take();
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| RpcError::process_error("Failed to capture process stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| RpcError::process_error("Failed to capture process stderr"))?;
    let write_stdin = async move {
        if let Some(data) = stdin_data
            && let Some(mut stdin) = stdin.take()
            && let Err(error) = stdin.write_all(&data).await
            && !is_benign_stdin_error(&error)
        {
            return Err(std::io::Error::other(format!(
                "Failed to write stdin: {error}"
            )));
        }
        Ok::<(), std::io::Error>(())
    };
    // This budget is shared by stdout and stderr for this request.  It is
    // intentionally per-run rather than server-wide, so admission permits up
    // to GENERAL_TASK_LIMIT concurrent allocations of this size.
    let remaining = Arc::new(Semaphore::new(output_limit));
    let result = tokio::try_join!(
        write_stdin,
        read_sync_output(stdout, Arc::clone(&remaining), output_limit),
        read_sync_output(stderr, remaining, output_limit),
        async {
            child
                .wait()
                .await
                .map_err(|e| std::io::Error::other(format!("Failed to wait for process: {e}")))
        }
    );
    let ((), stdout, stderr, status) = match result {
        Ok(result) => result,
        Err(error) => {
            let _ = child.kill().await;
            let _ = child.wait().await;
            return Err(RpcError::process_error(error.to_string()));
        }
    };
    process_group.disarm();

    // Return binary data directly (no encoding needed!)
    let exit_code = crate::protocol::exit_code_from_status(status);
    let result = ProcessResult {
        exit_code,
        stdout,
        stderr,
    };

    Ok(result.to_value())
}

// ============================================================================
// Asynchronous process management
// ============================================================================

/// Start an async process
pub async fn start(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        cmd: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        cwd: Option<String>,
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        for (key, value) in env {
            cmd.env(key, value);
        }
    }

    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    // If the request is cancelled before registry ownership transfers, dropping
    // the child must terminate the direct process while the group guard below
    // terminates descendants.
    cmd.kill_on_drop(true);

    configure_process_group(&mut cmd);

    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(error) => {
            let executable_missing = executable_is_missing(
                &params.cmd,
                params.cwd.as_deref(),
                params.env.as_ref(),
                params.clear_env,
            )
            .await;
            return Err(spawn_error(error, executable_missing));
        }
    };
    let child_pid = child
        .id()
        .ok_or_else(|| RpcError::process_error("Spawned process has no PID"))?;
    let mut process_group = ProcessGroupGuard::new(child_pid);

    let pid = get_next_pid().await;

    let managed = ManagedProcess {
        lifecycle: Arc::new(Mutex::new(())),
        read_lock: Arc::new(Mutex::new(())),
        exit_status: None,
        shared_exit_status: Arc::new(StdMutex::new(None)),
        stdin: Arc::new(Mutex::new(child.stdin.take())),
        stdout: Arc::new(Mutex::new(child.stdout.take())),
        stderr: Arc::new(Mutex::new(child.stderr.take())),
        child,
        child_pid,
        cmd: params.cmd.clone(),
        push_subscription: None,
        subscription_requested: false,
        terminating: false,
    };

    get_process_map().lock().await.insert(pid, managed);
    // The registry now owns termination and reaping.  There are no await points
    // between insertion and disarming, so cancellation cannot strand the child.
    process_group.disarm();

    Ok(msgpack_map! {
        "pid" => pid
    })
}

/// Subscribe to server-pushed output and exit notifications.
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

/// Stop server-pushed notifications without terminating the process.
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

/// Write to an async process's stdin
pub async fn write(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    let stdin = {
        let processes = get_process_map().lock().await;
        processes
            .get(&params.pid)
            .ok_or_else(|| {
                RpcError::process_error_with_kind(
                    format!("Process not found: {}", params.pid),
                    "not_found",
                )
            })?
            .stdin
            .clone()
    };

    let mut stdin_guard = stdin.lock().await;
    let Some(stdin) = stdin_guard.as_mut() else {
        return Err(RpcError::process_error_with_kind(
            format!("Process stdin is closed: {}", params.pid),
            "stdin_closed",
        ));
    };
    stdin
        .write_all(&data)
        .await
        .map_err(|e| RpcError::process_error(format!("Failed to write to stdin: {}", e)))?;

    Ok(msgpack_map! {
        "written" => data.len()
    })
}

/// Read from an async process's stdout/stderr
pub async fn read(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Maximum bytes to read
        #[serde(default = "default_max_read")]
        max_bytes: usize,
        /// Timeout in milliseconds to wait for data. If 0 or not specified, returns immediately.
        #[serde(default)]
        timeout_ms: Option<u64>,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.max_bytes == 0 || params.max_bytes > MAX_PROCESS_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_PROCESS_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);

    let (stdout, stderr, lifecycle, read_lock, shared_exit_status) = {
        let processes = get_process_map().lock().await;
        let managed = processes
            .get(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        (
            managed.stdout.clone(),
            managed.stderr.clone(),
            managed.lifecycle.clone(),
            managed.read_lock.clone(),
            managed.shared_exit_status.clone(),
        )
    };

    // A read owns output consumption through the terminal map-removal decision.
    // This prevents a concurrent EOF reader from removing bytes another request
    // has already consumed but not yet returned.
    let _read_guard = read_lock.lock().await;

    // Try to read stdout/stderr (with optional blocking timeout) without
    // holding the global process map lock.  `process.read` is long-polled by
    // the Emacs client; holding that lock here makes concurrent
    // `process.write` calls wait behind the read timeout, which turns LSP
    // typing into a synchronous round-trip bottleneck.
    let (stdout_result, stderr_result) =
        try_read_streams(stdout, stderr, params.max_bytes, timeout).await?;

    let stdout_eof = matches!(stdout_result, ReadResult::Eof);
    let stderr_eof = matches!(stderr_result, ReadResult::Eof);
    let stdout_data = stdout_result.into_data();
    let stderr_data = stderr_result.into_data();

    // Serialize terminal reads with kill/close without holding the global map
    // lock across the wait.
    let _lifecycle_guard = lifecycle.lock().await;

    // Check if process has exited.  Reacquire the map briefly; do not hold it
    // across any await points above.
    let mut exit_status = {
        let mut processes = get_process_map().lock().await;
        if let Some(managed) = processes.get_mut(&params.pid) {
            poll_exit_status(managed).map_err(|e| {
                RpcError::process_error(format!("Failed to query process status: {e}"))
            })?
        } else {
            *shared_exit_status
                .lock()
                .expect("shared pipe exit status lock")
        }
    };

    // When both pipes are at EOF the child has already closed all its file
    // descriptors, which means it has exited (or is in the act of exiting).
    // There is a tiny race on Linux between the child closing its fds and
    // the kernel updating its wait table: try_wait() may return None for a
    // brief window even though the pipes are done.  Yield to the runtime a
    // few times so it can process the SIGCHLD notification, then retry.
    if exit_status.is_none() && stdout_eof && stderr_eof {
        for _ in 0..5 {
            tokio::task::yield_now().await;
            let mut processes = get_process_map().lock().await;
            if let Some(managed) = processes.get_mut(&params.pid) {
                exit_status = poll_exit_status(managed).map_err(|e| {
                    RpcError::process_error(format!("Failed to query process status: {e}"))
                })?;
                if exit_status.is_some() {
                    break;
                }
            } else {
                exit_status = *shared_exit_status
                    .lock()
                    .expect("shared pipe exit status lock");
                break;
            }
        }
    }

    // Child exit and pipe EOF are separate events.  A child can exit after a
    // read returns data while additional bytes are still buffered in either
    // pipe.  Only report the terminal state after both streams have returned
    // EOF so the client continues issuing process.read requests until all
    // output has been delivered.
    let exited = exit_status.is_some() && stdout_eof && stderr_eof;

    // The terminal read is also the ownership handoff: the child has already
    // been reaped by poll_exit_status and both pipes have reached EOF.
    if exited {
        get_process_map().lock().await.remove(&params.pid);
    }

    // Return binary data directly (no encoding!)
    let stdout_val = if stdout_data.is_empty() {
        Value::Nil
    } else {
        Value::Binary(stdout_data)
    };

    let stderr_val = if stderr_data.is_empty() {
        Value::Nil
    } else {
        Value::Binary(stderr_data)
    };

    let exit_code = if exited {
        exit_status
            .map(crate::protocol::exit_code_from_status)
            .map(|code| Value::Integer(code.into()))
            .unwrap_or(Value::Nil)
    } else {
        Value::Nil
    };

    Ok(msgpack_map! {
        "stdout" => stdout_val,
        "stderr" => stderr_val,
        "exited" => exited,
        "exit_code" => exit_code
    })
}

enum ReadResult {
    Data(Vec<u8>),
    Pending,
    Eof,
}

impl ReadResult {
    fn into_data(self) -> Vec<u8> {
        match self {
            Self::Data(data) => data,
            Self::Pending | Self::Eof => Vec::new(),
        }
    }
}

fn poll_exit_status(managed: &mut ManagedProcess) -> std::io::Result<Option<ExitStatus>> {
    if managed.exit_status.is_none() {
        managed.exit_status = managed.child.try_wait()?;
        if managed.exit_status.is_some() {
            *managed
                .shared_exit_status
                .lock()
                .expect("shared pipe exit status lock") = managed.exit_status;
        }
    }
    Ok(managed.exit_status)
}

/// Read both output streams until either produces data or the shared timeout expires.
async fn try_read_streams<ROut, RErr>(
    stdout: Arc<Mutex<Option<ROut>>>,
    stderr: Arc<Mutex<Option<RErr>>>,
    max_bytes: usize,
    timeout_ms: u64,
) -> Result<(ReadResult, ReadResult), RpcError>
where
    ROut: AsyncRead + Unpin,
    RErr: AsyncRead + Unpin,
{
    let stdout_read = async {
        try_read_optional_stream(stdout, max_bytes)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stdout: {e}")))
    };
    let stderr_read = async {
        try_read_optional_stream(stderr, max_bytes)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stderr: {e}")))
    };
    let deadline = tokio::time::sleep(std::time::Duration::from_millis(if timeout_ms == 0 {
        1
    } else {
        timeout_ms
    }));

    tokio::pin!(stdout_read, stderr_read, deadline);

    // AsyncReadExt::read is cancellation-safe: when one stream produces data,
    // dropping the other branch cannot consume bytes from the idle stream.
    tokio::select! {
        stdout_result = &mut stdout_read => {
            let stdout_result = stdout_result?;
            if matches!(stdout_result, ReadResult::Data(_)) {
                return Ok((stdout_result, ReadResult::Pending));
            }

            let stderr_result = tokio::select! {
                stderr_result = &mut stderr_read => stderr_result?,
                _ = &mut deadline => ReadResult::Pending,
            };
            Ok((stdout_result, stderr_result))
        }
        stderr_result = &mut stderr_read => {
            let stderr_result = stderr_result?;
            if matches!(stderr_result, ReadResult::Data(_)) {
                return Ok((ReadResult::Pending, stderr_result));
            }

            let stdout_result = tokio::select! {
                stdout_result = &mut stdout_read => stdout_result?,
                _ = &mut deadline => ReadResult::Pending,
            };
            Ok((stdout_result, stderr_result))
        }
        _ = &mut deadline => Ok((ReadResult::Pending, ReadResult::Pending)),
    }
}

/// Try to read from an optional async reader.
async fn try_read_optional_stream<R>(
    stream: Arc<Mutex<Option<R>>>,
    max_bytes: usize,
) -> std::io::Result<ReadResult>
where
    R: AsyncRead + Unpin,
{
    let mut stream_guard = stream.lock().await;
    if let Some(reader) = stream_guard.as_mut() {
        let result = try_read_async(reader, max_bytes).await?;
        if matches!(result, ReadResult::Eof) {
            *stream_guard = None;
        }
        Ok(result)
    } else {
        Ok(ReadResult::Eof)
    }
}

/// Try to read from an async reader.
async fn try_read_async<R: AsyncRead + Unpin>(
    reader: &mut R,
    max_bytes: usize,
) -> std::io::Result<ReadResult> {
    let mut buf = vec![0u8; max_bytes];

    match reader.read(&mut buf).await {
        Ok(0) => Ok(ReadResult::Eof),
        Ok(n) => {
            buf.truncate(n);
            Ok(ReadResult::Data(buf))
        }
        Err(e) if e.kind() == ErrorKind::WouldBlock => Ok(ReadResult::Pending),
        Err(e) => Err(e),
    }
}

/// Close the stdin of an async process (signals EOF)
pub async fn close_stdin(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let stdin = {
        let processes = get_process_map().lock().await;
        processes
            .get(&params.pid)
            .ok_or_else(|| {
                RpcError::process_error_with_kind(
                    format!("Process not found: {}", params.pid),
                    "not_found",
                )
            })?
            .stdin
            .clone()
    };

    // Flush any buffered data before closing stdin, then drop to close the pipe.
    // This is a defensive measure: the client should drain its write queue before
    // calling close_stdin, but flushing here guards against data loss if a
    // concurrent process.write task wrote data that hasn't been flushed yet.
    let mut stdin_guard = stdin.lock().await;
    if let Some(mut stdin) = stdin_guard.take() {
        let _ = stdin.flush().await;
        // stdin is dropped here, closing the pipe
    }

    Ok(Value::Boolean(true))
}

#[cfg(test)]
static TEST_PROCESS_GROUP_SIGNAL_ERROR: OnceLock<StdMutex<Option<i32>>> = OnceLock::new();

#[cfg(test)]
fn set_test_process_group_signal_error(error: Option<i32>) {
    *TEST_PROCESS_GROUP_SIGNAL_ERROR
        .get_or_init(|| StdMutex::new(None))
        .lock()
        .expect("test process-group signal error lock") = error;
}

fn signal_process_group(pid: u32, signal: i32) -> std::io::Result<()> {
    #[cfg(test)]
    if let Some(error) = *TEST_PROCESS_GROUP_SIGNAL_ERROR
        .get_or_init(|| StdMutex::new(None))
        .lock()
        .expect("test process-group signal error lock")
    {
        return Err(std::io::Error::from_raw_os_error(error));
    }

    let result = unsafe { libc::kill(-(pid as libc::pid_t), signal) };
    if result == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

#[cfg(target_os = "macos")]
fn signal_process(pid: u32, signal: i32) -> std::io::Result<()> {
    let result = unsafe { libc::kill(pid as libc::pid_t, signal) };
    if result == 0 {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

fn signal_pty_process_group(pid: u32, signal: i32, action: &str) -> Result<(), RpcError> {
    match signal_process_group(pid, signal) {
        Ok(()) => Ok(()),
        #[cfg(target_os = "macos")]
        Err(error) if error.raw_os_error() == Some(libc::EPERM) => {
            // Darwin can reject a process-group signal with EPERM even when
            // the PTY leader remains signalable by this server.  Signal the
            // leader directly and continue through the normal reap/group-exit
            // checks.  If an unsignalable descendant really remains, the
            // later group check still observes it and reports the EPERM.
            require_process_group_signal(signal_process(pid, signal), action)
        }
        Err(error) => require_process_group_signal(Err(error), action),
    }
}

fn validate_signal(signal: i32) -> Result<(), RpcError> {
    if signal == 0 || Signal::try_from(signal).is_ok() {
        Ok(())
    } else {
        Err(RpcError::invalid_params(format!(
            "Invalid signal: {signal}"
        )))
    }
}

fn require_process_group_signal(result: std::io::Result<()>, action: &str) -> Result<(), RpcError> {
    match result {
        Ok(()) => Ok(()),
        // ESRCH positively establishes that the process group has already
        // exited.  In contrast, EPERM can mean a credential-changing
        // descendant remains alive and cannot be signalled by this server.
        Err(error) if error.raw_os_error() == Some(libc::ESRCH) => Ok(()),
        Err(error) => Err(RpcError::process_error(format!(
            "Failed to {action}: {error}"
        ))),
    }
}

async fn wait_pipe_child(os_pid: u32) -> Result<Option<ExitStatus>, nix::errno::Errno> {
    loop {
        match waitpid(Pid::from_raw(os_pid as i32), Some(WaitPidFlag::WNOHANG)) {
            Ok(status @ (WaitStatus::Exited(_, _) | WaitStatus::Signaled(_, _, _))) => {
                return Ok(Some(exit_status_from_wait_status(status)));
            }
            Ok(WaitStatus::StillAlive) => {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
            // Another status poll can only have consumed the status while the
            // lifecycle lock is not held.  The map entry remains until its
            // streams reach EOF, even in that case.
            Err(nix::errno::Errno::ECHILD) => return Ok(None),
            Err(nix::errno::Errno::EINTR) => continue,
            Err(error) => return Err(error),
            Ok(_) => return Err(nix::errno::Errno::EINVAL),
        }
    }
}

fn exit_status_from_wait_status(status: WaitStatus) -> ExitStatus {
    match status {
        WaitStatus::Exited(_, code) => ExitStatus::from_raw(code << 8),
        WaitStatus::Signaled(_, signal, core_dumped) => {
            ExitStatus::from_raw(signal as i32 | if core_dumped { 0x80 } else { 0 })
        }
        _ => ExitStatus::from_raw(0),
    }
}

const MANAGED_CHILD_WAIT: std::time::Duration = std::time::Duration::from_millis(500);
// PTY signal handlers may need a scheduler turn to flush their final output
// before exit; retain that output before escalating to SIGKILL.
const MANAGED_PTY_CHILD_WAIT: std::time::Duration = std::time::Duration::from_secs(2);

fn process_group_exists(pgid: u32) -> bool {
    let result = unsafe { libc::kill(-(pgid as i32), 0) };
    result == 0 || std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
}

async fn wait_for_process_group_exit(pgid: u32, deadline: tokio::time::Instant) -> bool {
    while tokio::time::Instant::now() < deadline {
        if !process_group_exists(pgid) {
            return false;
        }
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    }
    process_group_exists(pgid)
}

async fn terminate_pipe_process(pid: u32, signal: i32, escalate: bool) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle, shared_exit_status)) = ({
        let processes = get_process_map().lock().await;
        processes.get(&pid).map(|managed| {
            (
                managed.child_pid,
                managed.lifecycle.clone(),
                managed.shared_exit_status.clone(),
            )
        })
    }) else {
        return Err(RpcError::process_error(format!("Process not found: {pid}")));
    };

    let _lifecycle_guard = lifecycle.lock().await;
    require_process_group_signal(signal_process_group(os_pid, signal), "send signal")?;

    if matches!(signal, 0 | libc::SIGSTOP | libc::SIGCONT) {
        return Ok(true);
    }

    let cached = get_process_map()
        .lock()
        .await
        .get(&pid)
        .and_then(|managed| managed.exit_status);
    if cached.is_some() && !escalate {
        if signal == libc::SIGKILL {
            // Keep the handoff invariant even when a concurrent status()
            // poll reaped the child before this call acquired the lifecycle.
            *shared_exit_status
                .lock()
                .expect("shared pipe exit status lock") = cached;
            get_process_map().lock().await.remove(&pid);
        }
        return Ok(true);
    }

    // Only wait for death on signals expected to terminate the child.  A
    // forwarded interactive signal (SIGINT to a shell, SIGUSR1, ...) often
    // leaves the child running; stalling here for the full wait budget would
    // block the client and, via the lifecycle lock, concurrent reads.  A
    // child that does die from such a signal is reaped by the next status or
    // read poll.
    if !(escalate || signal == libc::SIGTERM || signal == libc::SIGKILL) {
        return Ok(true);
    }

    let mut reap = if cached.is_some() {
        cached
    } else {
        tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(os_pid))
            .await
            .ok()
            .and_then(Result::ok)
            .flatten()
    };
    // Give the whole process group its own grace period after the bounded
    // direct-child reap wait.  Starting this deadline before that wait would
    // make a TERM-ignoring direct child consume all of its descendants' grace.
    if escalate
        && wait_for_process_group_exit(os_pid, tokio::time::Instant::now() + MANAGED_CHILD_WAIT)
            .await
    {
        // The direct child may already have exited while a TERM-ignoring
        // descendant remains in its process group.  Cleanup must therefore
        // escalate based on the group, not only on the direct child's wait.
        require_process_group_signal(signal_process_group(os_pid, libc::SIGKILL), "send SIGKILL")?;
    }
    if reap.is_none() && escalate {
        reap = tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(os_pid))
            .await
            .map_err(|_| {
                RpcError::process_error(format!("Timed out reaping process {pid} after SIGKILL"))
            })?
            .map_err(|error| {
                RpcError::process_error(format!("Failed to reap process {pid}: {error}"))
            })?;
    }
    if let Some(exit_status) = reap {
        // Publish before any destructive cleanup so a read which captured the
        // streams before SIGKILL removed the entry can still report its exit.
        *shared_exit_status
            .lock()
            .expect("shared pipe exit status lock") = Some(exit_status);
        if let Some(managed) = get_process_map().lock().await.get_mut(&pid) {
            managed.exit_status = Some(exit_status);
        }
        if signal == libc::SIGKILL {
            // Explicit SIGKILL is the caller's opt-out from output draining.
            get_process_map().lock().await.remove(&pid);
        }
        return Ok(true);
    }
    if signal == libc::SIGKILL {
        // Explicit SIGKILL is the caller's opt-out from drain-preserving
        // ownership.  Always remove it, whether status() won the reap race or
        // this call did; already in-flight reads retain the shared state.
        get_process_map().lock().await.remove(&pid);
    }

    // Signal delivery is the success criterion, matching local
    // `signal-process': a signal is a request, not a guarantee that the
    // process exits within the bounded wait.  The entry stays reachable so
    // the caller can escalate (e.g. retry with SIGKILL) and later reads
    // still drain buffered output; the reap above is purely opportunistic.
    Ok(true)
}

/// Kill an async process and reap its direct child.
pub async fn kill(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Signal to send (default: SIGTERM)
        #[serde(default = "default_signal")]
        signal: i32,
    }

    fn default_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    validate_signal(params.signal)?;
    // Signaling an unknown pid is an error; internal cleanup paths call
    // terminate_pipe_process directly and tolerate vanished entries.
    let (subscription, shared_exit_status) = {
        let mut processes = get_process_map().lock().await;
        let managed = processes
            .get_mut(&params.pid)
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        if managed.terminating {
            return Err(RpcError::process_error(format!(
                "Process is already terminating: {}",
                params.pid
            )));
        }
        if params.signal == libc::SIGKILL {
            managed.terminating = true;
            (
                managed.push_subscription.take(),
                Some(managed.shared_exit_status.clone()),
            )
        } else {
            (None, None)
        }
    };
    let subscribed = subscription.is_some();
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    if let Err(error) = terminate_pipe_process(params.pid, params.signal, false).await {
        restore_pipe_after_failed_termination(params.pid).await;
        return Err(error);
    }
    if params.signal == libc::SIGKILL && subscribed {
        // Prefer the real exit status reaped by terminate_pipe_process over the
        // synthetic 128+signal value.  A process that exited cleanly just before
        // the kill should report 0, not 137.
        let exit_code = shared_exit_status
            .as_ref()
            .and_then(|arc| *arc.lock().expect("shared pipe exit status lock"))
            .map(crate::protocol::exit_code_from_status)
            .map(i64::from)
            .unwrap_or_else(|| i64::from(128 + params.signal));
        let _ = send_process_notification(
            "process.exit",
            msgpack_map! {
                "pid" => params.pid,
                "exit_code" => exit_code
            },
        )
        .await;
    }
    Ok(Value::Boolean(true))
}

/// Return status of an async process without consuming stdout/stderr.
pub async fn status(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let lifecycle = {
        let processes = get_process_map().lock().await;
        processes
            .get(&params.pid)
            .map(|managed| managed.lifecycle.clone())
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?
    };
    let _lifecycle_guard = lifecycle.lock().await;
    let mut processes = get_process_map().lock().await;
    let managed = processes
        .get_mut(&params.pid)
        .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
    let exit_status = poll_exit_status(managed)
        .map_err(|e| RpcError::process_error(format!("Failed to query process status: {e}")))?;

    Ok(msgpack_map! {
        "exited" => exit_status.is_some(),
        "exit_code" => exit_status.map(crate::protocol::exit_code_from_status).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

/// List all managed async processes
pub async fn list(_params: Value) -> HandlerResult {
    let entries: Vec<(u32, Arc<Mutex<()>>)> = {
        let processes = get_process_map().lock().await;
        processes
            .iter()
            .map(|(pid, managed)| (*pid, managed.lifecycle.clone()))
            .collect()
    };
    let mut list = Vec::with_capacity(entries.len());
    for (pid, lifecycle) in entries {
        let _lifecycle_guard = lifecycle.lock().await;
        let mut processes = get_process_map().lock().await;
        let Some(managed) = processes.get_mut(&pid) else {
            continue;
        };
        let exited = poll_exit_status(managed)
            .map_err(|e| RpcError::process_error(format!("Failed to query process status: {e}")))?;
        list.push(msgpack_map! {
            "pid" => pid,
            "os_pid" => Value::Integer((managed.child_pid as i64).into()),
            "cmd" => managed.cmd.clone(),
            "exited" => exited.is_some(),
            "exit_code" => exited.map(crate::protocol::exit_code_from_status).map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
        });
    }

    Ok(Value::Array(list))
}

// ============================================================================
// PTY (Pseudo-Terminal) Process Management
// ============================================================================

use std::os::unix::io::{FromRawFd, OwnedFd};
use std::os::unix::process::CommandExt;
use tokio::io::Interest;
use tokio::io::unix::AsyncFd;

static PTY_PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedPtyProcess>>> = OnceLock::new();
static TERMINATED_PTY_STATUSES: OnceLock<StdMutex<HashMap<u32, i32>>> = OnceLock::new();
static PTY_PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

fn get_pty_process_map() -> &'static Mutex<HashMap<u32, ManagedPtyProcess>> {
    PTY_PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

fn record_terminated_pty_status(pid: u32, exit_code: i32) {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .insert(pid, exit_code);
}

fn take_terminated_pty_status(pid: u32) -> Option<i32> {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .remove(&pid)
}

fn discard_terminated_pty_status(pid: u32) {
    let _ = take_terminated_pty_status(pid);
}

fn clear_terminated_pty_statuses() {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .clear();
}

async fn get_next_pty_pid() -> u32 {
    let counter = PTY_PID_COUNTER.get_or_init(|| Mutex::new(10000));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

struct PtyIoState {
    // The write lock covers the whole logical input stream, not just one
    // syscall, so concurrent requests cannot interleave their bytes.
    write_lock: Mutex<()>,
    // Serialize the close transition with the final nonblocking write syscall.
    syscall_lock: StdMutex<()>,
    // This is retained state: cancellation publishes `closed` before the
    // permit notification.  There is at most one waiter because writes are
    // serialized, so notify_one cannot lose a wakeup.
    closed: AtomicBool,
    cancelled: Notify,
}

impl PtyIoState {
    /// Publish cancellation after any in-flight write syscall finishes.
    fn cancel(&self) {
        let _syscall_guard = self.syscall_lock.lock().expect("PTY syscall lock");
        if !self.closed.swap(true, Ordering::AcqRel) {
            self.cancelled.notify_one();
        }
    }

    fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire)
    }
}

struct ManagedPtyProcess {
    async_fd: AsyncFd<OwnedFd>,
    lifecycle: Arc<Mutex<()>>,
    io: Arc<PtyIoState>,
    child_pid: Pid,
    cmd: String,
    exit_status: Option<i32>,
    // Retain an observed terminal status for a read that captured the PTY
    // before explicit SIGKILL removes its registry entry.
    shared_exit_status: Arc<StdMutex<Option<i32>>>,
    output_eof: bool,
    push_subscription: Option<PushSubscription>,
    subscription_requested: bool,
    terminating: bool,
}

fn checked_fcntl(result: libc::c_int) -> Result<libc::c_int, std::io::Error> {
    if result < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(result)
    }
}

fn set_fd_nonblocking(fd: RawFd) -> Result<(), std::io::Error> {
    let flags = checked_fcntl(unsafe { libc::fcntl(fd, libc::F_GETFL) })?;
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) })?;
    Ok(())
}

fn set_fd_cloexec(fd: RawFd) -> Result<(), std::io::Error> {
    let flags = checked_fcntl(unsafe { libc::fcntl(fd, libc::F_GETFD) })?;
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) })?;
    Ok(())
}

fn dup_cloexec(fd: RawFd) -> Result<RawFd, std::io::Error> {
    checked_fcntl(unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0) })
}

fn set_window_size(fd: RawFd, rows: u16, cols: u16) -> Result<(), std::io::Error> {
    let ws = libc::winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    let result = unsafe { libc::ioctl(fd, libc::TIOCSWINSZ as _, &ws) };
    if result < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

#[derive(Clone)]
struct PtyStartParams {
    cmd: String,
    args: Vec<String>,
    cwd: Option<String>,
    env: Option<HashMap<String, String>>,
    clear_env: bool,
    rows: u16,
    cols: u16,
}

struct PtyStartupGuard {
    master_fd: Option<OwnedFd>,
    child: Option<std::process::Child>,
    tty_name: String,
}

impl PtyStartupGuard {
    fn master_fd(&self) -> RawFd {
        self.master_fd.as_ref().expect("PTY master fd").as_raw_fd()
    }

    fn take_master_fd(&mut self) -> OwnedFd {
        self.master_fd.take().expect("PTY master fd")
    }

    fn disarm(mut self) -> (Pid, String) {
        let child = self.child.take().expect("PTY child");
        let child_pid = Pid::from_raw(child.id() as i32);
        // Child does not kill or reap on Drop.  Once registered, lifecycle
        // handlers own reaping through waitpid.
        drop(child);
        (child_pid, std::mem::take(&mut self.tty_name))
    }
}

fn spawn_async_pty_startup_reaper(mut child: std::process::Child) {
    if let Ok(runtime) = tokio::runtime::Handle::try_current() {
        runtime.spawn(async move {
            loop {
                match child.try_wait() {
                    Ok(Some(_)) | Err(_) => return,
                    Ok(None) => tokio::time::sleep(std::time::Duration::from_millis(10)).await,
                }
            }
        });
    } else {
        // Production startup runs inside Tokio.  During runtime teardown the
        // server process itself is exiting, so make only a nonblocking reap
        // attempt rather than risking an indefinite wait in Drop.
        let _ = child.try_wait();
    }
}

fn reap_pty_startup_child_with<F>(child: std::process::Child, spawn_reaper: F)
where
    F: FnOnce(Arc<StdMutex<Option<std::process::Child>>>) -> std::io::Result<()>,
{
    // Keep ownership outside the reaper closure until thread creation has
    // succeeded.  Builder::spawn consumes and drops its closure on failure;
    // moving Child directly into that closure would then leave a zombie.
    let shared_child = Arc::new(StdMutex::new(Some(child)));
    if spawn_reaper(Arc::clone(&shared_child)).is_err()
        && let Some(child) = shared_child.lock().expect("PTY startup child lock").take()
    {
        // Thread exhaustion must not turn Drop into an unbounded child.wait().
        // Poll from the existing Tokio runtime instead, retaining Child until
        // it has been reaped without occupying a blocking worker.
        spawn_async_pty_startup_reaper(child);
    }
}

fn spawn_pty_startup_reaper(child: std::process::Child) {
    reap_pty_startup_child_with(child, |shared_child| {
        std::thread::Builder::new()
            .name("tramp-rpc-pty-startup-reaper".into())
            .spawn(move || {
                if let Some(mut child) = shared_child.lock().expect("PTY startup child lock").take()
                {
                    let _ = child.wait();
                }
            })
            .map(drop)
    });
}

impl Drop for PtyStartupGuard {
    fn drop(&mut self) {
        let Some(child) = self.child.take() else {
            return;
        };
        let child_pid = child.id() as i32;
        // Dropping an aborted spawn_blocking result must not orphan its child.
        // Reap on a detached thread so a pathological child cannot block a
        // Tokio worker while the master fd closes through OwnedFd.
        unsafe {
            libc::kill(-child_pid, libc::SIGKILL);
        }
        spawn_pty_startup_reaper(child);
    }
}

fn do_fork_exec(params: PtyStartParams) -> Result<PtyStartupGuard, RpcError> {
    let OpenptyResult { master, slave } = openpty(None, None)
        .map_err(|e| RpcError::process_error(format!("Failed to open PTY: {}", e)))?;

    // Emacs inserts interactive input into comint buffers itself.  Disable
    // kernel echo to avoid delivering every input line twice, and preserve LF
    // output instead of the terminal driver's CRLF conversion.
    let mut termios = tcgetattr(&slave)
        .map_err(|e| RpcError::process_error(format!("Failed to read PTY termios: {e}")))?;
    termios
        .local_flags
        .remove(LocalFlags::ECHO | LocalFlags::ECHONL);
    termios.output_flags.remove(OutputFlags::ONLCR);
    tcsetattr(&slave, SetArg::TCSANOW, &termios)
        .map_err(|e| RpcError::process_error(format!("Failed to configure PTY termios: {e}")))?;

    set_fd_cloexec(master.as_raw_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {}", e)))?;
    set_fd_cloexec(slave.as_raw_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {}", e)))?;

    let tty_name = {
        let mut buf = vec![0u8; 256];
        let ret = unsafe {
            libc::ttyname_r(
                slave.as_raw_fd(),
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
            )
        };
        if ret != 0 {
            return Err(RpcError::process_error(format!(
                "Failed to get tty name: {}",
                std::io::Error::from_raw_os_error(ret)
            )));
        }
        let nul_pos = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
        String::from_utf8_lossy(&buf[..nul_pos]).into_owned()
    };

    set_window_size(master.as_raw_fd(), params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to set window size: {}", e)))?;

    let mut cmd = StdCommand::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(super::expand_tilde(cwd));
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        cmd.envs(env);
    }

    let slave_fd = slave.as_raw_fd();
    let master_fd = master.as_raw_fd();
    // Wrap every successful duplicate immediately so a later duplication
    // failure closes the descriptors already acquired.
    let stdin_fd = unsafe {
        OwnedFd::from_raw_fd(
            dup_cloexec(slave_fd)
                .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?,
        )
    };
    let stdout_fd = unsafe {
        OwnedFd::from_raw_fd(
            dup_cloexec(slave_fd)
                .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?,
        )
    };
    let stderr_fd = unsafe {
        OwnedFd::from_raw_fd(
            dup_cloexec(slave_fd)
                .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?,
        )
    };

    cmd.stdin(Stdio::from(stdin_fd));
    cmd.stdout(Stdio::from(stdout_fd));
    cmd.stderr(Stdio::from(stderr_fd));

    // SAFETY: the pre-exec hook only calls async-signal-safe libc syscalls.
    unsafe {
        cmd.pre_exec(move || {
            libc::close(master_fd);
            if libc::setsid() < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if libc::ioctl(slave_fd, libc::TIOCSCTTY as _, 0) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if slave_fd > 2 {
                libc::close(slave_fd);
            }
            Ok(())
        });
    }

    let child = cmd
        .spawn()
        .map_err(|e| RpcError::process_error(format!("Failed to spawn PTY process: {}", e)))?;
    drop(slave);

    Ok(PtyStartupGuard {
        master_fd: Some(master),
        child: Some(child),
        tty_name,
    })
}

/// Start a process with a PTY (pseudo-terminal)
pub async fn start_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        cmd: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        cwd: Option<String>,
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        #[serde(default)]
        clear_env: bool,
        #[serde(default = "default_rows")]
        rows: u16,
        #[serde(default = "default_cols")]
        cols: u16,
    }

    fn default_rows() -> u16 {
        24
    }
    fn default_cols() -> u16 {
        80
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let start_params = PtyStartParams {
        cmd: params.cmd.clone(),
        args: params.args,
        cwd: params.cwd,
        env: params.env,
        clear_env: params.clear_env,
        rows: params.rows,
        cols: params.cols,
    };

    let mut startup = tokio::task::spawn_blocking(move || do_fork_exec(start_params))
        .await
        .map_err(|e| RpcError::process_error(format!("Task join error: {}", e)))??;

    set_fd_nonblocking(startup.master_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to set non-blocking: {}", e)))?;

    let async_fd = AsyncFd::new(startup.take_master_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to create AsyncFd: {}", e)))?;

    let our_pid = get_next_pty_pid().await;
    let mut processes = get_pty_process_map().lock().await;
    // Disarm only after the final await.  Cancellation anywhere before this
    // point drops STARTUP, closes the PTY, and kills/reaps the unregistered child.
    let (child_pid, tty_name) = startup.disarm();

    let managed = ManagedPtyProcess {
        async_fd,
        lifecycle: Arc::new(Mutex::new(())),
        io: Arc::new(PtyIoState {
            write_lock: Mutex::new(()),
            syscall_lock: StdMutex::new(()),
            closed: AtomicBool::new(false),
            cancelled: Notify::new(),
        }),
        child_pid,
        cmd: params.cmd.clone(),
        exit_status: None,
        shared_exit_status: Arc::new(StdMutex::new(None)),
        output_eof: false,
        push_subscription: None,
        subscription_requested: false,
        terminating: false,
    };

    processes.insert(our_pid, managed);
    drop(processes);

    Ok(msgpack_map! {
        "pid" => our_pid,
        "os_pid" => child_pid.as_raw(),
        "tty_name" => tty_name
    })
}

/// Subscribe to server-pushed PTY output and exit notifications.
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

/// Stop server-pushed PTY notifications without terminating the process.
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

/// Resize a PTY terminal
pub async fn resize_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        rows: u16,
        cols: u16,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let lifecycle = {
        let processes = get_pty_process_map().lock().await;
        processes
            .get(&params.pid)
            .map(|managed| managed.lifecycle.clone())
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?
    };
    let _lifecycle_guard = lifecycle.lock().await;
    let (fd, child_pid, io) = {
        // Re-check the registry after acquiring lifecycle ownership: close or
        // terminal read may have removed the process while resize was waiting.
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (fd, managed.child_pid, managed.io.clone())
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY process is closed: {}",
            params.pid
        )));
    }

    set_window_size(owned_fd.as_raw_fd(), params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to resize PTY: {}", e)))?;

    match tcgetpgrp(&owned_fd) {
        Ok(fg_pgrp) => {
            let _ = nix::sys::signal::kill(Pid::from_raw(-fg_pgrp.as_raw()), Signal::SIGWINCH);
        }
        Err(_) => {
            let _ = nix::sys::signal::kill(Pid::from_raw(-child_pid.as_raw()), Signal::SIGWINCH);
        }
    }

    Ok(Value::Boolean(true))
}

/// Read from a PTY process with optional blocking
pub async fn read_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_max_read")]
        max_bytes: usize,
        #[serde(default)]
        timeout_ms: Option<u64>,
    }

    fn default_max_read() -> usize {
        65536
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.max_bytes == 0 || params.max_bytes > MAX_PROCESS_READ_BYTES {
        return Err(RpcError::invalid_params(format!(
            "max_bytes must be between 1 and {MAX_PROCESS_READ_BYTES}"
        )));
    }

    let timeout = params.timeout_ms.unwrap_or(0);
    let mut result = read_pty_now(params.pid, params.max_bytes).await?;
    if result.pending && timeout > 0 {
        let _ = tokio::time::timeout(
            std::time::Duration::from_millis(timeout),
            wait_for_pty_readable(params.pid),
        )
        .await;
        result = read_pty_now(params.pid, params.max_bytes).await?;
    }

    let output = if result.output.is_empty() {
        Value::Nil
    } else {
        Value::Binary(result.output)
    };
    Ok(msgpack_map! {
        "output" => output,
        "exited" => result.exited,
        "exit_code" => result.exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
    })
}

struct PtyReadResult {
    output: Vec<u8>,
    pending: bool,
    exited: bool,
    exit_code: Option<i32>,
}

async fn read_pty_now(pid: u32, max_bytes: usize) -> Result<PtyReadResult, RpcError> {
    // Take an owned descriptor before awaiting either lifecycle lock or
    // readiness.  Registry removal can then safely close the original fd
    // without invalidating this read or allowing fd-number reuse to target a
    // newly-created PTY.
    let (lifecycle, io, shared_exit_status, fd) = {
        let processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get(&pid) else {
            return Ok(PtyReadResult {
                output: Vec::new(),
                pending: false,
                exited: true,
                exit_code: take_terminated_pty_status(pid),
            });
        };
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (
            managed.lifecycle.clone(),
            managed.io.clone(),
            managed.shared_exit_status.clone(),
            fd,
        )
    };
    // SAFETY: dup_cloexec returned a fresh descriptor owned by this read.
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let _lifecycle_guard = lifecycle.lock().await;
    let mut processes = get_pty_process_map().lock().await;
    let Some(managed) = processes.get_mut(&pid) else {
        // Explicit SIGKILL intentionally discards output and removes registry
        // ownership.  A read already in flight still owns this status handle,
        // so report the terminal remote result rather than local success.
        let shared_status = *shared_exit_status
            .lock()
            .expect("shared PTY exit status lock");
        let retained_status = take_terminated_pty_status(pid);
        return Ok(PtyReadResult {
            output: Vec::new(),
            pending: false,
            exited: true,
            exit_code: shared_status.or(retained_status),
        });
    };

    let mut output = vec![0u8; max_bytes];
    let (pending, eof) = match unsafe {
        libc::read(
            owned_fd.as_raw_fd(),
            output.as_mut_ptr() as *mut libc::c_void,
            output.len(),
        )
    } {
        n if n > 0 => {
            output.truncate(n as usize);
            (false, false)
        }
        0 => {
            output.clear();
            (false, true)
        }
        -1 if matches!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(errno) if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK
        ) =>
        {
            output.clear();
            (true, false)
        }
        // Linux reports PTY master EOF as EIO after the slave closes.
        -1 if std::io::Error::last_os_error().raw_os_error() == Some(libc::EIO) => {
            output.clear();
            (false, true)
        }
        -1 => {
            return Err(RpcError::process_error(format!(
                "Failed to read PTY: {}",
                std::io::Error::last_os_error()
            )));
        }
        _ => unreachable!(),
    };
    if eof {
        managed.output_eof = true;
    }

    let (child_exited, exit_code) = check_exit_status(managed);
    let exited = child_exited && managed.output_eof;
    drop(processes);
    if exited {
        // Do not hold the registry lock while waiting for an in-flight write
        // syscall to finish.  The lifecycle guard keeps close/kill ordered
        // with this terminal read.
        io.cancel();
        get_pty_process_map().lock().await.remove(&pid);
    }
    Ok(PtyReadResult {
        output,
        pending,
        exited,
        exit_code: exited.then_some(exit_code).flatten(),
    })
}

fn check_exit_status(managed: &mut ManagedPtyProcess) -> (bool, Option<i32>) {
    if managed.exit_status.is_some() {
        (true, managed.exit_status)
    } else {
        match waitpid(managed.child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Exited(_, code)) => {
                managed.exit_status = Some(code);
                *managed
                    .shared_exit_status
                    .lock()
                    .expect("shared PTY exit status lock") = Some(code);
                (true, Some(code))
            }
            Ok(WaitStatus::Signaled(_, signal, _)) => {
                let code = 128 + signal as i32;
                managed.exit_status = Some(code);
                *managed
                    .shared_exit_status
                    .lock()
                    .expect("shared PTY exit status lock") = Some(code);
                (true, Some(code))
            }
            Ok(WaitStatus::StillAlive) => (false, None),
            _ => (false, None),
        }
    }
}

async fn wait_for_pty_readable(pid: u32) -> bool {
    // Wait on a duplicate so close/kill can remove the registry entry and
    // close the real master without racing an in-flight readiness wait.
    let fd = {
        let processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get(&pid) else {
            return false;
        };
        match dup_cloexec(managed.async_fd.get_ref().as_raw_fd()) {
            Ok(fd) => fd,
            Err(_) => return false,
        }
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let async_fd = match AsyncFd::new(owned_fd) {
        Ok(fd) => fd,
        Err(_) => return false,
    };
    async_fd.readable().await.is_ok()
}

enum PtyWriteAction {
    Progress,
    Retry,
}

fn apply_pty_write(
    offset: &mut usize,
    total: usize,
    result: std::io::Result<usize>,
) -> Result<PtyWriteAction, RpcError> {
    match result {
        Ok(written) if written > 0 => {
            *offset += written;
            Ok(PtyWriteAction::Progress)
        }
        Ok(_) => Err(RpcError::process_error("PTY write returned zero bytes")),
        Err(error) if matches!(error.kind(), ErrorKind::Interrupted | ErrorKind::WouldBlock) => {
            debug_assert!(*offset <= total);
            Ok(PtyWriteAction::Retry)
        }
        Err(error) => Err(RpcError::process_error(format!(
            "Failed to write to PTY: {error}"
        ))),
    }
}

/// Write to a PTY process (async)
pub async fn write_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        /// Binary data to write
        #[serde(with = "serde_bytes")]
        data: Vec<u8>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Data is already binary, no decoding needed!
    let data = params.data;

    // Capture the descriptor and cancellation state while briefly holding the
    // registry lock.  All waits and writes happen on owned state afterwards.
    let (fd, io) = {
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        let fd = dup_cloexec(managed.async_fd.get_ref().as_raw_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {}", e)))?;
        (fd, managed.io.clone())
    };
    let owned_fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let async_fd = AsyncFd::new(owned_fd)
        .map_err(|e| RpcError::process_error(format!("Failed to monitor PTY: {}", e)))?;
    let _write_guard = io.write_lock.lock().await;
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY write cancelled: {}",
            params.pid
        )));
    }

    let mut offset = 0;
    while offset < data.len() {
        let cancelled = io.cancelled.notified();
        if io.is_closed() {
            return Err(RpcError::process_error(format!(
                "PTY write cancelled: {}",
                params.pid
            )));
        }
        let mut guard = tokio::select! {
            result = async_fd.ready(Interest::WRITABLE) => result
                .map_err(|e| RpcError::process_error(format!("Failed to wait for writable: {e}")))?,
            _ = cancelled => {
                return Err(RpcError::process_error(format!(
                    "PTY write cancelled: {}", params.pid
                )));
            }
        };

        // Cancellation and the final closed check are atomic with the
        // nonblocking syscall: no write can begin after close is published.
        let _syscall_guard = io.syscall_lock.lock().expect("PTY syscall lock");
        if io.is_closed() {
            return Err(RpcError::process_error(format!(
                "PTY write cancelled: {}",
                params.pid
            )));
        }
        let result = guard.try_io(|inner| {
            let n = unsafe {
                libc::write(
                    inner.get_ref().as_raw_fd(),
                    data[offset..].as_ptr() as *const libc::c_void,
                    data.len() - offset,
                )
            };
            if n >= 0 {
                Ok(n as usize)
            } else {
                Err(std::io::Error::last_os_error())
            }
        });
        match result {
            Ok(result) => {
                apply_pty_write(&mut offset, data.len(), result)?;
            }
            // `AsyncFd` clears stale writable readiness before retrying.
            Err(_would_block) => {}
        }
    }

    Ok(msgpack_map! {
        "written" => data.len()
    })
}

async fn wait_pty_pid(child_pid: Pid) -> Result<Option<i32>, nix::errno::Errno> {
    loop {
        match waitpid(child_pid, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Exited(_, code)) => return Ok(Some(code)),
            Ok(WaitStatus::Signaled(_, signal, _)) => return Ok(Some(128 + signal as i32)),
            Ok(WaitStatus::StillAlive) => {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
            Err(nix::errno::Errno::ECHILD) => return Ok(None),
            Err(nix::errno::Errno::EINTR) => continue,
            Err(error) => return Err(error),
            Ok(_) => return Err(nix::errno::Errno::EINVAL),
        }
    }
}

async fn terminate_pty_process(
    pid: u32,
    signal: i32,
    escalate: bool,
    remove: bool,
    retain_removed_status: bool,
) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle, io, shared_exit_status)) = ({
        let processes = get_pty_process_map().lock().await;
        processes.get(&pid).map(|managed| {
            (
                managed.child_pid.as_raw() as u32,
                managed.lifecycle.clone(),
                managed.io.clone(),
                managed.shared_exit_status.clone(),
            )
        })
    }) else {
        return if remove {
            Ok(true)
        } else {
            Err(RpcError::process_error(format!(
                "PTY process not found: {pid}"
            )))
        };
    };
    // Explicit teardown and SIGKILL must wake a writer blocked on readiness
    // immediately.  Other forwarded signals (notably SIGINT) are survivable
    // for an interactive shell, so leave the PTY I/O state usable until a
    // terminal exit has actually been confirmed.
    if remove || signal == libc::SIGKILL {
        io.cancel();
    }
    let _lifecycle_guard = lifecycle.lock().await;
    signal_pty_process_group(os_pid, signal, "send signal")?;

    if matches!(signal, 0 | libc::SIGSTOP | libc::SIGCONT) {
        return Ok(true);
    }

    let cached = get_pty_process_map()
        .lock()
        .await
        .get(&pid)
        .and_then(|managed| managed.exit_status);
    if let Some(cached_exit_code) = cached
        && !escalate
    {
        // A previously reaped direct child is confirmed terminal death, even
        // when a caller used a non-fatal signal for this request.
        io.cancel();
        if remove {
            *shared_exit_status
                .lock()
                .expect("shared PTY exit status lock") = Some(cached_exit_code);
            if retain_removed_status {
                record_terminated_pty_status(pid, cached_exit_code);
            }
            get_pty_process_map().lock().await.remove(&pid);
        }
        return Ok(true);
    }

    // As with pipe children: only wait for death on signals expected to
    // terminate the child; a surviving SIGINT/SIGUSR1 must not stall the
    // client for the wait budget.  Exits are reaped by later read polls.
    if !(escalate || signal == libc::SIGTERM || signal == libc::SIGKILL) {
        return Ok(true);
    }

    let mut reap = if cached.is_some() {
        cached
    } else {
        tokio::time::timeout(
            MANAGED_PTY_CHILD_WAIT,
            wait_pty_pid(Pid::from_raw(os_pid as i32)),
        )
        .await
        .ok()
        .and_then(Result::ok)
        .flatten()
    };
    // Start the process-group grace only after the direct-child reap wait.
    // Otherwise a TERM-ignoring child consumes all of its descendants' grace.
    let escalation_deadline = tokio::time::Instant::now()
        + if signal == libc::SIGKILL {
            std::time::Duration::ZERO
        } else {
            MANAGED_PTY_CHILD_WAIT
        };
    if escalate && wait_for_process_group_exit(os_pid, escalation_deadline).await {
        // A reaped PTY leader does not prove that TERM-ignoring descendants
        // left the process group.  Escalate the whole group during cleanup.
        signal_pty_process_group(os_pid, libc::SIGKILL, "send SIGKILL")?;
    }
    if reap.is_none() && escalate {
        reap = tokio::time::timeout(
            MANAGED_CHILD_WAIT,
            wait_pty_pid(Pid::from_raw(os_pid as i32)),
        )
        .await
        .map_err(|_| {
            RpcError::process_error(format!("Timed out reaping PTY process {pid} after SIGKILL"))
        })?
        .map_err(|error| {
            RpcError::process_error(format!("Failed to reap PTY process {pid}: {error}"))
        })?;
    }
    if let Some(exit_code) = reap {
        // Reaping confirms terminal death, so no further PTY input can be
        // admitted.  This also wakes a writer that was waiting for readiness.
        io.cancel();
        // Kill reaps the direct child but deliberately leaves the PTY master
        // registered until read_pty observes terminal EOF, unless this is an
        // explicit close.
        let mut processes = get_pty_process_map().lock().await;
        if remove {
            // Publish before discarding registry ownership.  An in-flight
            // read holds this Arc and must report the killed remote process,
            // not nil/zero local relay success.
            *shared_exit_status
                .lock()
                .expect("shared PTY exit status lock") = Some(exit_code);
            if retain_removed_status {
                record_terminated_pty_status(pid, exit_code);
            }
            processes.remove(&pid);
        } else if let Some(managed) = processes.get_mut(&pid) {
            managed.exit_status = Some(exit_code);
        }
        return Ok(true);
    }

    if remove {
        // SIGKILL was delivered but the bounded reap did not observe a status
        // (for example, another waiter consumed it).  The explicit kill still
        // has deterministic abnormal process semantics for an in-flight read.
        let exit_code = {
            let mut status = shared_exit_status
                .lock()
                .expect("shared PTY exit status lock");
            *status.get_or_insert(128 + libc::SIGKILL)
        };
        if retain_removed_status {
            record_terminated_pty_status(pid, exit_code);
        }
        get_pty_process_map().lock().await.remove(&pid);
        return Ok(true);
    }

    // As for pipe children: delivery is success; termination is not
    // guaranteed within the bounded wait and the entry stays for escalation
    // and draining.
    Ok(true)
}

/// Kill a PTY process group and reap its direct child.
pub async fn kill_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_pty_signal")]
        signal: i32,
    }

    fn default_pty_signal() -> i32 {
        libc::SIGTERM
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    validate_signal(params.signal)?;
    // Signaling an unknown pid is an error; close_pty stays idempotent by
    // calling terminate_pty_process directly.
    let (subscription, shared_exit_status) = {
        let mut processes = get_pty_process_map().lock().await;
        let managed = processes.get_mut(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        if managed.terminating {
            return Err(RpcError::process_error(format!(
                "PTY process is already terminating: {}",
                params.pid
            )));
        }
        if params.signal == libc::SIGKILL {
            managed.terminating = true;
            (
                managed.push_subscription.take(),
                Some(managed.shared_exit_status.clone()),
            )
        } else {
            (None, None)
        }
    };
    let subscribed = subscription.is_some();
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    // Match local signal-process semantics: forward the requested signal
    // without turning a survivable signal such as SIGINT into SIGKILL.
    // Explicit close and connection cleanup retain escalation authority.
    // Explicit SIGKILL also opts out of output draining.
    if let Err(error) = terminate_pty_process(
        params.pid,
        params.signal,
        false,
        params.signal == libc::SIGKILL,
        params.signal == libc::SIGKILL,
    )
    .await
    {
        restore_pty_after_failed_termination(params.pid).await;
        return Err(error);
    }
    if params.signal == libc::SIGKILL && subscribed {
        // Prefer the real exit status reaped by terminate_pty_process over the
        // synthetic 128+signal value.  A process that exited cleanly just before
        // the kill should report 0, not 137.
        let exit_code = shared_exit_status
            .as_ref()
            .and_then(|arc| *arc.lock().expect("shared PTY exit status lock"))
            .map(i64::from)
            .unwrap_or_else(|| i64::from(128 + params.signal));
        let _ = send_process_notification(
            "process.pty_exit",
            msgpack_map! {
                "pid" => params.pid,
                "exit_code" => exit_code
            },
        )
        .await;
    }
    Ok(Value::Boolean(true))
}

/// Close a PTY process and discard buffered output.  Repeating close is harmless.
pub async fn close_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let subscription = {
        let mut processes = get_pty_process_map().lock().await;
        match processes.get_mut(&params.pid) {
            Some(managed) if managed.terminating => {
                return Err(RpcError::process_error(format!(
                    "PTY process is already terminating: {}",
                    params.pid
                )));
            }
            Some(managed) => {
                managed.terminating = true;
                managed.push_subscription.take()
            }
            None => None,
        }
    };
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    // Explicit close is the opt-out from kill's drain-preserving ownership.
    if let Err(error) = terminate_pty_process(params.pid, libc::SIGKILL, true, true, false).await {
        restore_pty_after_failed_termination(params.pid).await;
        return Err(error);
    }
    discard_terminated_pty_status(params.pid);
    Ok(Value::Boolean(true))
}

/// List all PTY processes
pub async fn list_pty(_params: Value) -> HandlerResult {
    let entries: Vec<(u32, Arc<Mutex<()>>)> = {
        let processes = get_pty_process_map().lock().await;
        processes
            .iter()
            .map(|(pid, managed)| (*pid, managed.lifecycle.clone()))
            .collect()
    };
    let mut list = Vec::with_capacity(entries.len());
    for (pid, lifecycle) in entries {
        let _lifecycle_guard = lifecycle.lock().await;
        let mut processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get_mut(&pid) else {
            continue;
        };
        let (exited, exit_code) = check_exit_status(managed);
        list.push(msgpack_map! {
            "pid" => pid,
            "os_pid" => managed.child_pid.as_raw(),
            "cmd" => managed.cmd.clone(),
            "exited" => exited,
            "exit_code" => exit_code.map(|c| Value::Integer(c.into())).unwrap_or(Value::Nil)
        });
    }

    Ok(Value::Array(list))
}

/// Stop and reap managed children after the transport is gone.
pub async fn cleanup_managed_processes() -> Result<(), RpcError> {
    // No notification can be delivered after transport EOF.  Stop readers
    // before process termination so no detached task retains stream/fd state.
    let mut subscriptions = Vec::new();
    {
        let mut processes = get_process_map().lock().await;
        for managed in processes.values_mut() {
            managed.terminating = true;
            subscriptions.extend(managed.push_subscription.take());
        }
    }
    {
        let mut processes = get_pty_process_map().lock().await;
        for managed in processes.values_mut() {
            managed.terminating = true;
            subscriptions.extend(managed.push_subscription.take());
        }
    }
    futures::future::join_all(subscriptions.into_iter().map(stop_push_subscription)).await;

    let pipe_pids: Vec<(u32, u32)> = {
        let processes = get_process_map().lock().await;
        processes
            .iter()
            .map(|(pid, managed)| (*pid, managed.child_pid))
            .collect()
    };
    let pty_pids: Vec<(u32, u32)> = {
        let processes = get_pty_process_map().lock().await;
        processes
            .iter()
            .map(|(pid, managed)| (*pid, managed.child_pid.as_raw() as u32))
            .collect()
    };

    let pipe_reaps = pipe_pids.into_iter().map(|(pid, _)| async move {
        (pid, terminate_pipe_process(pid, libc::SIGTERM, true).await)
    });
    let pty_reaps = pty_pids.into_iter().map(|(pid, _)| async move {
        (
            pid,
            terminate_pty_process(pid, libc::SIGTERM, true, true, false).await,
        )
    });
    let (pipe_results, pty_results) = tokio::join!(
        futures::future::join_all(pipe_reaps),
        futures::future::join_all(pty_reaps)
    );

    // The transport is gone, so successful reaps may discard unread pipes.
    // Failed entries stay registered for the final cleanup pass to retry.
    let mut failures = Vec::new();
    {
        let mut processes = get_process_map().lock().await;
        for (pid, result) in pipe_results {
            match result {
                Ok(_) => {
                    processes.remove(&pid);
                }
                Err(error) => failures.push(format!("pipe process {pid}: {}", error.message)),
            }
        }
    }
    for (pid, result) in pty_results {
        if let Err(error) = result {
            failures.push(format!("PTY process {pid}: {}", error.message));
        }
    }
    // The transport is gone, so no client can consume retained terminal
    // statuses.  Do not keep tombstones alive for the lifetime of the server.
    clear_terminated_pty_statuses();

    if failures.is_empty() {
        Ok(())
    } else {
        Err(RpcError::process_error(format!(
            "Managed process cleanup failed: {}",
            failures.join("; ")
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let error = read_sync_output(&b"oversized"[..], Arc::new(Semaphore::new(4)), 4)
            .await
            .expect_err("output above the remaining response budget should fail");
        assert!(error.to_string().contains("output exceeds"));
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
                    Value::String(
                        "echo $$ > \"$1\"; while :; do printf 0123456789abcdef; done".into(),
                    ),
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

        let pid: libc::pid_t = std::fs::read_to_string(&pid_path)
            .expect("child pid file")
            .trim()
            .parse()
            .expect("numeric child pid");
        assert_eq!(unsafe { libc::kill(pid, 0) }, -1);
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ESRCH),
            "limited child must be dead"
        );
        let mut status = 0;
        assert_eq!(
            unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ECHILD),
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
        let mut fds = [-1; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "create pipe");
        let read_fd = unsafe { OwnedFd::from_raw_fd(fds[0]) };
        let write_fd = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        set_fd_nonblocking(read_fd.as_raw_fd()).expect("make pipe reader nonblocking");
        set_fd_nonblocking(write_fd.as_raw_fd()).expect("make pipe writer nonblocking");
        set_fd_cloexec(read_fd.as_raw_fd()).expect("mark pipe reader close-on-exec");
        set_fd_cloexec(write_fd.as_raw_fd()).expect("mark pipe writer close-on-exec");
        let bytes = [0_u8; 8192];
        loop {
            let written = unsafe {
                libc::write(
                    write_fd.as_raw_fd(),
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                )
            };
            if written >= 0 {
                continue;
            }
            assert_eq!(
                std::io::Error::last_os_error().raw_os_error(),
                Some(libc::EAGAIN),
                "fill pipe to EAGAIN"
            );
            return (read_fd, write_fd);
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
            "import signal,time; signal.signal(signal.SIGHUP, signal.SIG_IGN); signal.signal(signal.SIGTERM, signal.SIG_IGN); open({:?}, 'w').close(); time.sleep(30)",
            marker
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
            if unsafe { libc::kill(os_pid, 0) } < 0 {
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
            if unsafe { libc::kill(os_pid, 0) } < 0 {
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
        for _ in 0..100 {
            if path.exists() {
                return;
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
        panic!("marker was not created: {}", path.display());
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
    async fn pty_subscription_can_be_stopped_without_closing_process() {
        let _test_lock = test_process_map_lock().await;
        let result = start_pty(Value::Map(vec![(
            Value::String("cmd".into()),
            Value::String("cat".into()),
        )]))
        .await
        .expect("start PTY");
        let pid = map_get(&result, "pid")
            .and_then(Value::as_u64)
            .expect("PTY pid") as u32;
        let params = || {
            Value::Map(vec![(
                Value::String("pid".into()),
                Value::Integer(pid.into()),
            )])
        };

        subscribe_pty(params()).await.expect("subscribe PTY");
        assert!(
            get_pty_process_map()
                .lock()
                .await
                .get(&pid)
                .is_some_and(|managed| {
                    managed.subscription_requested && managed.push_subscription.is_some()
                })
        );
        unsubscribe_pty(params()).await.expect("unsubscribe PTY");
        assert!(
            get_pty_process_map()
                .lock()
                .await
                .get(&pid)
                .is_some_and(|managed| {
                    !managed.subscription_requested && managed.push_subscription.is_none()
                })
        );
        close_pty(params()).await.expect("close PTY");
    }

    #[tokio::test]
    async fn connection_cleanup_stops_pipe_and_pty_subscriptions() {
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
        let pid_params = |pid: u32| {
            Value::Map(vec![(
                Value::String("pid".into()),
                Value::Integer(pid.into()),
            )])
        };

        subscribe(pid_params(pipe_pid))
            .await
            .expect("subscribe pipe");
        subscribe_pty(pid_params(pty_pid))
            .await
            .expect("subscribe PTY");
        cleanup_managed_processes()
            .await
            .expect("cleanup subscribed processes");
        assert!(test_managed_maps_empty().await);
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
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("cleanup pipe");
        kill_pty(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pty_pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
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
        assert_eq!(unsafe { libc::kill(descendant_pid, 0) }, 0);

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
            "import os,signal,time; pid=os.fork(); (open({:?},'w').write(str(pid)), os._exit(0)) if pid else (signal.signal(signal.SIGHUP, signal.SIG_IGN), signal.signal(signal.SIGTERM, signal.SIG_IGN), time.sleep(30))",
            marker
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
        assert_eq!(unsafe { libc::kill(descendant_pid, 0) }, 0);

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

        let mut fds = [-1; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        let _read_end = unsafe { OwnedFd::from_raw_fd(fds[0]) };
        let write_end = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        set_fd_nonblocking(write_end.as_raw_fd()).expect("make test fd nonblocking");
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
            let mut fds = [-1; 2];
            assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0, "create pipe");
            let read_end = unsafe { OwnedFd::from_raw_fd(fds[0]) };
            let write_end = unsafe { OwnedFd::from_raw_fd(fds[1]) };
            drop(read_end);
            set_fd_nonblocking(write_end.as_raw_fd()).expect("make test fd nonblocking");
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
            let read = unsafe {
                libc::read(
                    read_fd.as_raw_fd(),
                    scratch.as_mut_ptr() as *mut libc::c_void,
                    scratch.len(),
                )
            };
            if read < 0 {
                assert_eq!(
                    std::io::Error::last_os_error().raw_os_error(),
                    Some(libc::EAGAIN)
                );
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
                    let read = unsafe {
                        libc::read(
                            inner.get_ref().as_raw_fd(),
                            chunk.as_mut_ptr() as *mut libc::c_void,
                            chunk.len(),
                        )
                    };
                    if read >= 0 {
                        Ok(chunk[..read as usize].to_vec())
                    } else {
                        Err(std::io::Error::last_os_error())
                    }
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
                tokio::time::timeout(std::time::Duration::from_millis(100), &mut blocked_write)
                    .await;

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

    // =========================================================================
    // Subscriber model stress tests
    //
    // These tests verify that the subscribe/unsubscribe lifecycle is correct
    // under concurrent and high-load conditions.  Because unit tests do not
    // initialise PROCESS_NOTIFICATION_WRITER, notification *delivery* is not
    // checked here; see test/tramp-rpc-stress-tests.el for end-to-end coverage
    // of the full notification path through a live server binary.
    // =========================================================================

    fn pid_param(pid: u32) -> Value {
        Value::Map(vec![(
            Value::String("pid".into()),
            Value::Integer(pid.into()),
        )])
    }

    async fn subscription_is_active(pid: u32) -> bool {
        get_process_map()
            .lock()
            .await
            .get(&pid)
            .map(|m| m.push_subscription.is_some())
            .unwrap_or(false)
    }

    async fn subscription_task_finished(pid: u32) -> bool {
        get_process_map()
            .lock()
            .await
            .get(&pid)
            .and_then(|m| m.push_subscription.as_ref())
            .map(|s| s.task.is_finished())
            .unwrap_or(true) // no subscription means nothing is running
    }

    // Subscribe to PID and return the subscription's JoinHandle clone via
    // a polled-flag so we can wait for the task to finish independently.
    async fn subscribe_pid(pid: u32) {
        subscribe(pid_param(pid))
            .await
            .expect("subscribe should succeed");
    }

    async fn unsubscribe_pid(pid: u32) {
        unsubscribe(pid_param(pid))
            .await
            .expect("unsubscribe should succeed");
    }

    /// Subscribe all PIDs in `pids`, returning after all subscribe RPCs
    /// succeed.
    async fn subscribe_all(pids: &[u32]) {
        for &pid in pids {
            subscribe_pid(pid).await;
        }
    }

    /// Collect PIDs from a set of `process.start` results.
    fn extract_pid(result: &Value) -> u32 {
        map_get(result, "pid")
            .and_then(Value::as_u64)
            .expect("process pid") as u32
    }

    // -------------------------------------------------------------------------
    // stress_subscribe_many_concurrent_processes_cleanup
    //
    // Start N short-lived processes, subscribe to all of them, let them exit
    // naturally, then run connection cleanup.  Verifies:
    //   - No deadlock under concurrent subscription tasks.
    //   - All map entries are removed after cleanup.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_subscribe_many_concurrent_processes_cleanup() {
        let _test_lock = test_process_map_lock().await;
        const N: usize = 20;

        // Start processes that write a small amount then exit quickly.
        let starts = futures::future::join_all((0..N).map(|i| {
            start(Value::Map(vec![
                (Value::String("cmd".into()), Value::String("/bin/sh".into())),
                (
                    Value::String("args".into()),
                    Value::Array(vec![
                        Value::String("-c".into()),
                        Value::String(format!("printf 'p{i}'; exit 0").into()),
                    ]),
                ),
            ]))
        }))
        .await;

        let pids: Vec<u32> = starts
            .into_iter()
            .map(|r| extract_pid(&r.expect("start")))
            .collect();

        // Subscribe to every process – this spawns N subscription tasks.
        subscribe_all(&pids).await;

        // Wait until all subscription tasks finish on their own (processes exit
        // quickly, so each task detects exit and breaks within its 200 ms read
        // window).
        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
        for &pid in &pids {
            loop {
                if subscription_task_finished(pid).await {
                    break;
                }
                assert!(
                    tokio::time::Instant::now() < deadline,
                    "subscription task for pid {pid} did not finish before deadline"
                );
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        }

        // Connection cleanup must remove all entries with no errors.
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            cleanup_managed_processes(),
        )
        .await
        .expect("cleanup should not hang")
        .expect("cleanup should succeed");

        assert!(
            test_managed_maps_empty().await,
            "process maps should be empty after cleanup"
        );
    }

    // -------------------------------------------------------------------------
    // stress_subscribe_unsubscribe_cycle_no_task_leak
    //
    // Repeatedly subscribe and unsubscribe to a long-running process.  Each
    // unsubscribe must stop the previous task before returning.  Verifies:
    //   - No orphaned tasks accumulate.
    //   - No deadlock in the subscribe/unsubscribe lock path.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_subscribe_unsubscribe_cycle_no_task_leak() {
        let _test_lock = test_process_map_lock().await;
        const CYCLES: usize = 10;

        let pid = start_pipe_process("sleep 30").await;

        for _ in 0..CYCLES {
            subscribe_pid(pid).await;
            assert!(
                subscription_is_active(pid).await,
                "subscription must be active after subscribe"
            );
            unsubscribe_pid(pid).await;
            // After unsubscribe the subscription slot must be cleared (the
            // task was awaited and stopped inside unsubscribe).
            assert!(
                !subscription_is_active(pid).await,
                "subscription must be inactive after unsubscribe"
            );
        }

        // Verify no task handle leaked – the process should still be alive.
        assert!(
            get_process_map().lock().await.contains_key(&pid),
            "process should still be registered"
        );

        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("cleanup SIGKILL");
    }

    // -------------------------------------------------------------------------
    // stress_concurrent_subscribe_and_write_no_deadlock
    //
    // Subscribe to a process while writing to its stdin concurrently.  The
    // write queue and the subscription task both acquire process-map locks;
    // this test verifies they do not deadlock each other.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_concurrent_subscribe_and_write_no_deadlock() {
        let _test_lock = test_process_map_lock().await;
        // cat reads stdin and echoes to stdout.
        let pid = start_pipe_process("cat").await;

        let payload = Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("data".into()),
                Value::Binary(vec![b'x'; 4096]),
            ),
        ]);

        // Interleave subscribe and write calls concurrently.
        let (sub_result, write_result) = tokio::join!(subscribe(pid_param(pid)), write(payload));
        sub_result.expect("subscribe should not fail");
        write_result.expect("write should not fail");

        // Close stdin so cat exits and the subscription task can terminate.
        close_stdin(pid_param(pid)).await.expect("close stdin");

        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(5);
        loop {
            if subscription_task_finished(pid).await {
                break;
            }
            assert!(
                tokio::time::Instant::now() < deadline,
                "subscription task did not finish after stdin close"
            );
            tokio::time::sleep(std::time::Duration::from_millis(20)).await;
        }

        cleanup_managed_processes()
            .await
            .expect("cleanup after concurrent subscribe+write");
    }

    // -------------------------------------------------------------------------
    // stress_concurrent_subscribe_and_kill_multiple_processes
    //
    // Start N processes, subscribe to all, then SIGKILL them all concurrently.
    // Verifies that concurrent kill+cleanup does not deadlock or leave leaked
    // subscription tasks.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_concurrent_subscribe_and_kill_multiple_processes() {
        let _test_lock = test_process_map_lock().await;
        const N: usize = 10;

        let pids: Vec<u32> = futures::future::join_all(
            (0..N).map(|_| async { start_pipe_process("sleep 30").await }),
        )
        .await;

        subscribe_all(&pids).await;

        // Kill all processes concurrently.
        let kill_futures: Vec<_> = pids
            .iter()
            .map(|&pid| {
                kill(Value::Map(vec![
                    (Value::String("pid".into()), Value::Integer(pid.into())),
                    (
                        Value::String("signal".into()),
                        Value::Integer((libc::SIGKILL as i64).into()),
                    ),
                ]))
            })
            .collect();

        let results = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            futures::future::join_all(kill_futures),
        )
        .await
        .expect("concurrent kills must not hang");

        for (i, result) in results.into_iter().enumerate() {
            result.unwrap_or_else(|e| panic!("kill pid index {i} failed: {e:?}"));
        }

        // All entries should be gone (SIGKILL removes from the map).
        for &pid in &pids {
            assert!(
                !get_process_map().lock().await.contains_key(&pid),
                "pid {pid} should be removed after SIGKILL"
            );
        }

        assert!(
            test_managed_maps_empty().await,
            "process maps must be empty after concurrent SIGKILL"
        );
    }

    // -------------------------------------------------------------------------
    // stress_subscribe_task_stops_after_natural_process_exit
    //
    // Subscribe to a process that exits immediately.  The subscription task
    // must detect the exit via `process.read` and terminate within the read
    // timeout window (PUSH_READ_TIMEOUT_MS = 200 ms).  Verifies:
    //   - Task finishes without an explicit unsubscribe call.
    //   - Elapsed time is bounded (no infinite loop).
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_subscribe_task_stops_after_natural_process_exit() {
        let _test_lock = test_process_map_lock().await;
        const N: usize = 8;

        for _ in 0..N {
            let pid = start_pipe_process("exit 0").await;
            subscribe_pid(pid).await;

            let deadline = tokio::time::Instant::now() + std::time::Duration::from_millis(2_000);
            loop {
                if subscription_task_finished(pid).await {
                    break;
                }
                assert!(
                    tokio::time::Instant::now() < deadline,
                    "subscription task did not detect process exit within deadline"
                );
                tokio::time::sleep(std::time::Duration::from_millis(20)).await;
            }

            // Clean up the process map entry.
            cleanup_managed_processes()
                .await
                .expect("cleanup after natural exit");
        }
    }

    // -------------------------------------------------------------------------
    // stress_idempotent_subscribe_does_not_spawn_extra_tasks
    //
    // Calling subscribe multiple times for the same PID must not create
    // multiple background tasks — the server is supposed to reuse the existing
    // subscription when one is already active.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_idempotent_subscribe_does_not_spawn_extra_tasks() {
        let _test_lock = test_process_map_lock().await;
        let pid = start_pipe_process("sleep 30").await;

        // Subscribe three times in a row.
        for _ in 0..3 {
            subscribe_pid(pid).await;
        }

        // There must still be exactly one subscription (not three).
        // We verify indirectly: unsubscribe once and the slot is clear.
        unsubscribe_pid(pid).await;
        assert!(
            !subscription_is_active(pid).await,
            "subscription must be gone after a single unsubscribe"
        );

        kill(Value::Map(vec![
            (Value::String("pid".into()), Value::Integer(pid.into())),
            (
                Value::String("signal".into()),
                Value::Integer((libc::SIGKILL as i64).into()),
            ),
        ]))
        .await
        .expect("cleanup SIGKILL");
    }

    // -------------------------------------------------------------------------
    // stress_high_throughput_output_via_subscribe
    //
    // A process that writes a large volume of data.  The subscription task
    // must drain all output without hanging.  Output data is not delivered
    // (no notification writer in unit tests) but the task must still exit
    // cleanly after the process finishes.
    // -------------------------------------------------------------------------
    #[tokio::test]
    async fn stress_high_throughput_output_via_subscribe() {
        let _test_lock = test_process_map_lock().await;
        // Generate ~2 MiB of output to stress the read loop.
        let pid = start_pipe_process("dd if=/dev/zero bs=4096 count=512 2>/dev/null | cat").await;
        subscribe_pid(pid).await;

        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(15);
        loop {
            if subscription_task_finished(pid).await {
                break;
            }
            assert!(
                tokio::time::Instant::now() < deadline,
                "subscription task did not finish draining 2 MiB output"
            );
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }

        cleanup_managed_processes()
            .await
            .expect("cleanup after high-throughput subscribe");
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
}
