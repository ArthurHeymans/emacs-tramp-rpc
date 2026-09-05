// SPDX-License-Identifier: GPL-3.0-or-later

//! Pipe-backed process handlers: one-shot `process.run` and the managed
//! asynchronous `process.start`/`read`/`write`/`kill` family.

use crate::msgpack_map;
use crate::protocol::{ProcessResult, RpcError, from_value};
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::Pid;
use rmpv::Value;
use rustix::io::fcntl_dupfd_cloexec;
#[cfg(target_vendor = "apple")]
use rustix::pipe::pipe;
#[cfg(not(target_vendor = "apple"))]
use rustix::pipe::{PipeFlags, pipe_with};
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::OwnedFd;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex as StdMutex, OnceLock};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command};
use tokio::sync::{Mutex, Semaphore};

use super::super::HandlerResult;
use super::super::system::expand_tilde;
#[cfg(target_vendor = "apple")]
use super::set_fd_cloexec;
use super::subscription::{
    PushSubscription, new_pipe_subscription, send_process_notification, stop_push_subscription,
};
use super::{
    MANAGED_CHILD_WAIT, MAX_PROCESS_READ_BYTES, ProcessGroupGuard, SignalCode,
    configure_process_group, is_benign_stdin_error, require_process_group_signal, signal_process,
    signal_process_group, wait_for_process_group_exit,
};

pub(super) fn merged_output_fds() -> std::io::Result<(OwnedFd, OwnedFd, OwnedFd)> {
    #[cfg(target_vendor = "apple")]
    let (read_fd, write_fd) = {
        let (read_fd, write_fd) = pipe()?;
        set_fd_cloexec(&read_fd)?;
        set_fd_cloexec(&write_fd)?;
        (read_fd, write_fd)
    };
    #[cfg(not(target_vendor = "apple"))]
    let (read_fd, write_fd) = pipe_with(PipeFlags::CLOEXEC)?;

    let stderr_fd = fcntl_dupfd_cloexec(&write_fd, 0)?;
    Ok((read_fd, write_fd, stderr_fd))
}

pub(super) fn merged_output_pipe() -> std::io::Result<(Stdio, Stdio, tokio::fs::File)> {
    let (read_fd, write_fd, stderr_fd) = merged_output_fds()?;
    let reader = tokio::fs::File::from_std(std::fs::File::from(read_fd));

    Ok((Stdio::from(write_fd), Stdio::from(stderr_fd), reader))
}

pub(super) struct RetainedOutputBudget {
    remaining: Arc<Semaphore>,
    retained: AtomicUsize,
    committed: AtomicBool,
}

impl RetainedOutputBudget {
    pub(super) fn new(remaining: Arc<Semaphore>) -> Self {
        Self {
            remaining,
            retained: AtomicUsize::new(0),
            committed: AtomicBool::new(false),
        }
    }

    fn retain(&self, bytes: usize, output_limit: usize) -> std::io::Result<()> {
        let permits = u32::try_from(bytes).expect("output buffer length fits in u32");
        let permit = self.remaining.try_acquire_many(permits).map_err(|_| {
            std::io::Error::other(format!("Process output exceeds {output_limit} byte limit"))
        })?;
        permit.forget();
        self.retained.fetch_add(bytes, Ordering::Relaxed);
        Ok(())
    }

    fn commit(&self) {
        self.committed.store(true, Ordering::Relaxed);
    }
}

impl Drop for RetainedOutputBudget {
    fn drop(&mut self) {
        if !self.committed.load(Ordering::Relaxed) {
            self.remaining
                .add_permits(self.retained.load(Ordering::Relaxed));
        }
    }
}

pub(super) async fn read_sync_output<R>(
    mut reader: R,
    budget: Arc<RetainedOutputBudget>,
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
        // Consumed permits represent bytes retained in the response buffers.
        budget.retain(read, output_limit)?;
        output.extend_from_slice(&buffer[..read]);
    }
}

pub(super) fn command_path(command: &str, cwd: Option<&str>) -> PathBuf {
    let path = Path::new(command);
    if path.is_relative() && command.contains('/') {
        cwd.map_or_else(
            || path.to_path_buf(),
            |dir| PathBuf::from(expand_tilde(dir)).join(path),
        )
    } else {
        path.to_path_buf()
    }
}

pub(super) fn executable_is_missing_sync(
    command: &str,
    cwd: Option<&str>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    if cwd.is_some_and(|dir| !Path::new(&expand_tilde(dir)).is_dir()) {
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
    let cwd = cwd.map(|dir| PathBuf::from(expand_tilde(dir)));
    std::env::split_paths(&path).all(|dir| {
        let dir = if dir.is_relative() {
            cwd.as_ref().map_or(dir.clone(), |cwd| cwd.join(dir))
        } else {
            dir
        };
        !dir.join(command).is_file()
    })
}

pub(super) async fn executable_is_missing(
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

pub(super) fn spawn_error(error: std::io::Error, executable_missing: bool) -> RpcError {
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

// ============================================================================
// Process management for async processes
// ============================================================================

// Production starts one server OS process per RPC transport connection.  These
// process-local maps therefore cannot mix processes from separate connections;
// connection cleanup drains the maps when that one transport ends.  Tests
// serialize connection loops with `test_process_map_lock` for the same reason.
pub(super) static PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedProcess>>> = OnceLock::new();
pub(super) static PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

pub(super) fn get_process_map() -> &'static Mutex<HashMap<u32, ManagedProcess>> {
    PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

pub(super) async fn get_next_pid() -> u32 {
    let counter = PID_COUNTER.get_or_init(|| Mutex::new(1));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

pub(super) struct ManagedProcess {
    pub(super) child: Child,
    pub(super) child_pid: u32,
    pub(super) lifecycle: Arc<Mutex<()>>,
    pub(super) read_lock: Arc<Mutex<()>>,
    pub(super) exit_status: Option<ExitStatus>,
    pub(super) shared_exit_status: Arc<StdMutex<Option<ExitStatus>>>,
    pub(super) stdin: Arc<Mutex<Option<ChildStdin>>>,
    pub(super) stdout: Arc<Mutex<Option<ChildStdout>>>,
    pub(super) stderr: Arc<Mutex<Option<ChildStderr>>>,
    pub(super) cmd: String,
    pub(super) push_subscription: Option<PushSubscription>,
    pub(super) subscription_requested: bool,
    pub(super) terminating: bool,
}

// ============================================================================
// Synchronous process execution (but async-friendly)
// ============================================================================

/// Everything needed to spawn one pipe-backed child.
pub(crate) struct ChildSpec {
    pub(crate) cmd: String,
    pub(crate) args: Vec<String>,
    pub(crate) cwd: Option<String>,
    /// Shared so `commands.run_parallel` can apply one environment to every
    /// entry without cloning it per child.
    pub(crate) env: Option<Arc<HashMap<String, String>>>,
    pub(crate) clear_env: bool,
    pub(crate) stdin: Option<Vec<u8>>,
    /// Capture stdout and stderr through one ordered pipe; `stderr` in the
    /// result is then always empty.
    pub(crate) merge_stderr: bool,
}

/// Why `run_child` did not produce a `ProcessResult`.
pub(crate) enum ChildError {
    /// `spawn` itself failed; the child never started.
    Spawn(std::io::Error),
    /// Pipe or descriptor setup failed before or right after the spawn.
    Setup(std::io::Error),
    /// Stdin, output drain, or wait failed; the child has been killed.
    Io(std::io::Error),
    /// The deadline passed; the child has been killed.
    TimedOut,
}

/// Expand a tilde working directory off the Tokio workers.
///
/// `~user` needs a passwd lookup that can block on NSS, so it runs on the
/// blocking pool; plain paths and `~/...` (HOME) never leave the worker.
async fn expand_cwd(cwd: Option<String>) -> std::io::Result<Option<PathBuf>> {
    match cwd {
        None => Ok(None),
        Some(cwd) if !cwd.starts_with('~') => Ok(Some(PathBuf::from(cwd))),
        Some(cwd) => tokio::task::spawn_blocking(move || PathBuf::from(expand_tilde(&cwd)))
            .await
            .map(Some)
            .map_err(std::io::Error::other),
    }
}

impl ChildSpec {
    /// Build the command with CWD already resolved by `expand_cwd`.
    fn command(&self, cwd: Option<&Path>) -> Command {
        let mut cmd = Command::new(&self.cmd);
        cmd.args(&self.args);
        if let Some(cwd) = cwd {
            cmd.current_dir(cwd);
        }
        if self.clear_env {
            cmd.env_clear();
        }
        if let Some(env) = &self.env {
            cmd.envs(env.iter());
        }
        // Never let a synchronous child consume the RPC transport.  A pipe is
        // only needed when the caller supplied input.
        cmd.stdin(if self.stdin.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        });
        cmd.kill_on_drop(true);
        configure_process_group(&mut cmd);
        cmd
    }
}

/// Spawn a child, feed its stdin, drain its output within `remaining` and
/// wait for it, killing the whole process group on any failure or when
/// `deadline` passes.  Shared by `process.run` and `commands.run_parallel`.
///
/// `remaining` is the retained-output budget for this child; callers that run
/// several children against one response share a single semaphore.
pub(crate) async fn run_child(
    spec: ChildSpec,
    remaining: Arc<Semaphore>,
    output_limit: usize,
    deadline: Option<tokio::time::Instant>,
) -> Result<ProcessResult, ChildError> {
    let output_budget = Arc::new(RetainedOutputBudget::new(remaining));
    let cwd = expand_cwd(spec.cwd.clone())
        .await
        .map_err(ChildError::Setup)?;
    let mut cmd = spec.command(cwd.as_deref());
    let merged_reader = if spec.merge_stderr {
        let (stdout, stderr, reader) = merged_output_pipe().map_err(ChildError::Setup)?;
        cmd.stdout(stdout);
        cmd.stderr(stderr);
        Some(reader)
    } else {
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        None
    };

    let mut child = cmd.spawn().map_err(ChildError::Spawn)?;
    // Tokio's Command retains custom Stdio handles after spawn.  Drop it so
    // the parent does not keep the merged pipe's write ends open and prevent
    // the reader from observing EOF after the child exits.
    drop(cmd);
    let child_pid = child
        .id()
        .ok_or_else(|| ChildError::Setup(std::io::Error::other("Spawned process has no PID")))?;
    let mut process_group = ProcessGroupGuard::new(child_pid);

    // Drive stdin, bounded output drains, and child exit concurrently.  A
    // genuine pipe or size error cancels the other operations and kills the
    // child; a broken stdin pipe does not, because a child that stops reading
    // early (`head`, `grep -q', ...) is normal and its output must survive.
    let stdin_data = spec.stdin;
    let mut stdin = child.stdin.take();
    let separate_streams = if merged_reader.is_none() {
        Some((
            child.stdout.take().ok_or_else(|| {
                ChildError::Setup(std::io::Error::other("Failed to capture process stdout"))
            })?,
            child.stderr.take().ok_or_else(|| {
                ChildError::Setup(std::io::Error::other("Failed to capture process stderr"))
            })?,
        ))
    } else {
        None
    };
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
    let wait_child = async {
        child
            .wait()
            .await
            .map_err(|e| std::io::Error::other(format!("Failed to wait for process: {e}")))
    };
    let run = async {
        if let Some(reader) = merged_reader {
            tokio::try_join!(
                write_stdin,
                read_sync_output(reader, Arc::clone(&output_budget), output_limit),
                wait_child
            )
            .map(|((), stdout, status)| (stdout, Vec::new(), status))
        } else {
            let (stdout, stderr) =
                separate_streams.expect("separate streams are captured when not merged");
            tokio::try_join!(
                write_stdin,
                read_sync_output(stdout, Arc::clone(&output_budget), output_limit),
                read_sync_output(stderr, Arc::clone(&output_budget), output_limit),
                wait_child
            )
            .map(|((), stdout, stderr, status)| (stdout, stderr, status))
        }
    };
    // The optional deadline isolates hung commands: a stuck child fails on
    // its own instead of stalling its whole request.
    let result = match deadline {
        Some(deadline) => match tokio::time::timeout_at(deadline, run).await {
            Ok(result) => result.map_err(ChildError::Io),
            Err(_elapsed) => Err(ChildError::TimedOut),
        },
        None => run.await.map_err(ChildError::Io),
    };
    let (stdout, stderr, status) = match result {
        Ok(result) => result,
        Err(error) => {
            let _ = child.kill().await;
            let _ = child.wait().await;
            return Err(error);
        }
    };
    process_group.disarm();
    output_budget.commit();

    // Return binary data directly (no encoding needed!)
    Ok(ProcessResult {
        exit_code: crate::protocol::exit_code_from_status(status),
        stdout,
        stderr,
    })
}

/// Run a command and wait for it to complete
pub async fn run(params: Value) -> HandlerResult {
    run_with_output_limit(params, crate::MAX_RESPONSE_OUTPUT_BYTES).await
}

pub(super) async fn run_with_output_limit(params: Value, output_limit: usize) -> HandlerResult {
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
        /// Capture stdout and stderr through one ordered pipe
        #[serde(default)]
        merge_stderr: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let env = params.env.map(Arc::new);
    let spec = ChildSpec {
        cmd: params.cmd,
        args: params.args,
        cwd: params.cwd,
        env: env.clone(),
        clear_env: params.clear_env,
        stdin: params.stdin,
        merge_stderr: params.merge_stderr,
    };
    // Spawn failure diagnostics need the command again, so keep what the
    // missing-executable probe consults.
    let probe = (spec.cmd.clone(), spec.cwd.clone(), params.clear_env);

    // This budget is shared by stdout and stderr for this request.  It is
    // intentionally per-run rather than server-wide, so admission permits up
    // to GENERAL_TASK_LIMIT concurrent allocations of this size.
    let remaining = Arc::new(Semaphore::new(output_limit));
    match run_child(spec, remaining, output_limit, None).await {
        Ok(result) => Ok(result.to_value()),
        Err(ChildError::Spawn(error)) => {
            let (cmd, cwd, clear_env) = probe;
            let executable_missing =
                executable_is_missing(&cmd, cwd.as_deref(), env.as_deref(), clear_env).await;
            Err(spawn_error(error, executable_missing))
        }
        Err(ChildError::Setup(error) | ChildError::Io(error)) => {
            Err(RpcError::process_error(error.to_string()))
        }
        Err(ChildError::TimedOut) => Err(RpcError::process_error("Command timed out")),
    }
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

    let cwd = expand_cwd(params.cwd.clone())
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {e}")))?;

    let mut cmd = Command::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &cwd {
        cmd.current_dir(cwd);
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
        .map_err(|e| RpcError::process_error(format!("Failed to write to stdin: {e}")))?;

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

pub(super) enum ReadResult {
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

pub(super) fn poll_exit_status(
    managed: &mut ManagedProcess,
) -> std::io::Result<Option<ExitStatus>> {
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
pub(super) async fn try_read_streams<ROut, RErr>(
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
pub(super) async fn try_read_optional_stream<R>(
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
pub(super) async fn try_read_async<R: AsyncRead + Unpin>(
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

pub(super) async fn wait_pipe_child(os_pid: u32) -> Result<Option<ExitStatus>, nix::errno::Errno> {
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

pub(super) fn exit_status_from_wait_status(status: WaitStatus) -> ExitStatus {
    match status {
        WaitStatus::Exited(_, code) => ExitStatus::from_raw(code << 8),
        WaitStatus::Signaled(_, signal, core_dumped) => {
            ExitStatus::from_raw(signal as i32 | if core_dumped { 0x80 } else { 0 })
        }
        _ => ExitStatus::from_raw(0),
    }
}

pub(super) async fn terminate_pipe_process(
    pid: u32,
    signal: i32,
    escalate: bool,
) -> Result<bool, RpcError> {
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
        signal: SignalCode,
    }

    fn default_signal() -> SignalCode {
        SignalCode::Number(libc::SIGTERM)
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let signal = params.signal.resolve()?;
    // A subscription owns the output read loop.  Stop it before destructive
    // SIGKILL so it cannot race registry removal or consume final output.
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
        if signal == libc::SIGKILL {
            managed.terminating = true;
            (
                managed.push_subscription.take(),
                Some(Arc::clone(&managed.shared_exit_status)),
            )
        } else {
            (None, None)
        }
    };
    let subscribed = subscription.is_some();
    if let Some(subscription) = subscription {
        stop_push_subscription(subscription).await;
    }
    if let Err(error) = terminate_pipe_process(params.pid, signal, false).await {
        if let Some(managed) = get_process_map().lock().await.get_mut(&params.pid) {
            managed.terminating = false;
            if managed.subscription_requested && managed.push_subscription.is_none() {
                managed.push_subscription = Some(new_pipe_subscription(params.pid));
            }
        }
        return Err(error);
    }
    if signal == libc::SIGKILL && subscribed {
        let exit_code = shared_exit_status
            .as_ref()
            .and_then(|status| *status.lock().expect("shared pipe exit status lock"))
            .map(crate::protocol::exit_code_from_status)
            .map(i64::from)
            .unwrap_or_else(|| i64::from(128 + signal));
        let _ = send_process_notification(
            "process.exit",
            msgpack_map! { "pid" => params.pid, "exit_code" => exit_code },
        )
        .await;
    }
    Ok(Value::Boolean(true))
}

/// Signal an arbitrary operating-system process by PID.
pub async fn signal_pid(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        signal: SignalCode,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    if params.pid == 0 {
        return Err(RpcError::invalid_params("PID must be greater than zero"));
    }
    let signal = params.signal.resolve()?;
    signal_process(params.pid, signal)
        .map_err(|error| RpcError::process_error(format!("Failed to signal process: {error}")))?;
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
