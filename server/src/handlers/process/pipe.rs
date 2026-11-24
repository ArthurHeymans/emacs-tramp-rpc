// SPDX-License-Identifier: GPL-3.0-or-later

//! Pipe-backed process handlers: one-shot `process.run` and the managed
//! asynchronous `process.start`/`read`/`write`/`kill` family.

use crate::msgpack_map;
use crate::protocol::{ProcessResult, RpcError, RpcPath, from_value};
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
use std::sync::{Arc, Mutex as StdMutex};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use tokio::net::unix::pipe::Receiver as PipeReceiver;
use tokio::process::Command;
use tokio::sync::{Mutex, Notify, Semaphore};

use super::super::HandlerResult;
#[cfg(test)]
use super::MAX_PROCESS_READ_BYTES;
use super::push::{new_pipe_push, send_exit_notification, stop_output_push};
use super::{
    MANAGED_CHILD_WAIT, ManagedProcess, PipeBackend, ProcessBackend, ProcessGroupGuard, SignalCode,
    configure_process_group, is_benign_stdin_error, registry, require_process_group_signal,
    signal_process, signal_process_group, wait_for_process_group_exit,
};
#[cfg(target_vendor = "apple")]
use super::{set_fd_cloexec, set_fd_nonblocking};

pub(super) fn merged_output_fds() -> std::io::Result<(OwnedFd, OwnedFd, OwnedFd)> {
    #[cfg(target_vendor = "apple")]
    let (read_fd, write_fd) = {
        let (read_fd, write_fd) = pipe()?;
        set_fd_cloexec(&read_fd)?;
        set_fd_cloexec(&write_fd)?;
        set_fd_nonblocking(&read_fd)?;
        (read_fd, write_fd)
    };
    #[cfg(not(target_vendor = "apple"))]
    let (read_fd, write_fd) = {
        let (read_fd, write_fd) = pipe_with(PipeFlags::CLOEXEC)?;
        super::set_fd_nonblocking(&read_fd)?;
        (read_fd, write_fd)
    };

    let stderr_fd = fcntl_dupfd_cloexec(&write_fd, 0)?;
    Ok((read_fd, write_fd, stderr_fd))
}

pub(super) fn merged_output_pipe() -> std::io::Result<(Stdio, Stdio, PipeReceiver)> {
    let (read_fd, write_fd, stderr_fd) = merged_output_fds()?;
    let reader = PipeReceiver::from_owned_fd(read_fd)?;

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

pub(super) fn command_path(command: &str, cwd: Option<&Path>) -> PathBuf {
    let path = Path::new(command);
    if path.is_relative() && command.contains('/') {
        cwd.map_or_else(|| path.to_path_buf(), |dir| dir.join(path))
    } else {
        path.to_path_buf()
    }
}

pub(super) fn executable_is_missing_sync(
    command: &str,
    cwd: Option<&Path>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    if cwd.is_some_and(|dir| !dir.is_dir()) {
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
    std::env::split_paths(&path).all(|dir| {
        let dir = if dir.is_relative() {
            cwd.map_or(dir.clone(), |cwd| cwd.join(dir))
        } else {
            dir
        };
        !dir.join(command).is_file()
    })
}

pub(super) async fn executable_is_missing(
    command: &str,
    cwd: Option<PathBuf>,
    env: Option<&HashMap<String, String>>,
    clear_env: bool,
) -> bool {
    let command = command.to_owned();
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
// Synchronous process execution (but async-friendly)
// ============================================================================

/// Everything needed to spawn one pipe-backed child.
pub(crate) struct ChildSpec {
    pub(crate) cmd: String,
    pub(crate) args: Vec<String>,
    pub(crate) cwd: Option<RpcPath>,
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
async fn expand_cwd(cwd: Option<&RpcPath>) -> std::io::Result<Option<PathBuf>> {
    match cwd {
        None => Ok(None),
        Some(cwd) => cwd
            .resolve()
            .await
            .map(Some)
            .map_err(|e| std::io::Error::other(e.message)),
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
    let cwd = expand_cwd(spec.cwd.as_ref())
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
        status,
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
        cwd: Option<RpcPath>,
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
        Ok(result) => Ok(result.into_value()),
        Err(ChildError::Spawn(error)) => {
            let (cmd, cwd, clear_env) = probe;
            let resolved_cwd = expand_cwd(cwd.as_ref()).await.ok().flatten();
            let executable_missing =
                executable_is_missing(&cmd, resolved_cwd, env.as_deref(), clear_env).await;
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

/// Start an async process, returning both the wire value and its registered PID.
pub(crate) async fn start_outcome(params: Value) -> Result<(Value, u32), RpcError> {
    #[derive(Deserialize)]
    struct Params {
        cmd: String,
        #[serde(default)]
        args: Vec<String>,
        #[serde(default)]
        cwd: Option<RpcPath>,
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        #[serde(default)]
        clear_env: bool,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let cwd = expand_cwd(params.cwd.as_ref())
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
            let executable_missing =
                executable_is_missing(&params.cmd, cwd, params.env.as_ref(), params.clear_env)
                    .await;
            return Err(spawn_error(error, executable_missing));
        }
    };
    let child_pid = child
        .id()
        .ok_or_else(|| RpcError::process_error("Spawned process has no PID"))?;
    let mut process_group = ProcessGroupGuard::new(child_pid);

    let pid = registry().next_pid();

    let managed = ManagedProcess {
        child_pid,
        lifecycle: Arc::new(Mutex::new(())),
        exit_status: None,
        shared_exit_status: Arc::new(StdMutex::new(None)),
        output_push: None,
        terminating: false,
        backend: ProcessBackend::Pipe(PipeBackend {
            stdin: Arc::new(Mutex::new(child.stdin.take())),
            stdout: Arc::new(Mutex::new(child.stdout.take())),
            stderr: Arc::new(Mutex::new(child.stderr.take())),
            child,
        }),
    };

    registry().lock().insert(pid, managed);
    // The registry now owns termination and reaping.  There are no await points
    // between insertion and disarming, so cancellation cannot strand the child.
    process_group.disarm();

    Ok((msgpack_map! { "pid" => pid }, pid))
}

/// Start an async process
pub async fn start(params: Value) -> HandlerResult {
    start_outcome(params).await.map(|(value, _)| value)
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

    let data = params.data;

    let stdin = {
        let processes = registry().lock();
        processes
            .get(&params.pid)
            .and_then(ManagedProcess::as_pipe)
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

/// Output collected by one [`read_output`] step.
pub(super) struct PipeRead {
    pub(super) stdout: Vec<u8>,
    pub(super) stderr: Vec<u8>,
    /// Set once both streams reached EOF and the child was reaped; the
    /// registry entry has then been removed.
    pub(super) exited: bool,
    pub(super) exit: Option<ExitStatus>,
}

/// Read from an async process's stdout/stderr (test-only wire shape).
#[cfg(test)]
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

    let result = read_output(params.pid, params.max_bytes, params.timeout_ms.unwrap_or(0)).await?;
    let bytes_or_nil = |data: Vec<u8>| {
        if data.is_empty() {
            Value::Nil
        } else {
            Value::Binary(data)
        }
    };
    let mut pairs = vec![
        (Value::String("stdout".into()), bytes_or_nil(result.stdout)),
        (Value::String("stderr".into()), bytes_or_nil(result.stderr)),
        (
            Value::String("exited".into()),
            Value::Boolean(result.exited),
        ),
    ];
    pairs.extend(crate::protocol::exit_fields(result.exit));
    Ok(Value::Map(pairs))
}

/// Read available stdout/stderr of PID, waiting up to TIMEOUT_MS for data.
#[cfg(test)]
pub(super) async fn read_output(
    pid: u32,
    max_bytes: usize,
    timeout: u64,
) -> Result<PipeRead, RpcError> {
    let mut stdout_buf = vec![0u8; max_bytes];
    let mut stderr_buf = vec![0u8; max_bytes];
    read_output_into(pid, &mut stdout_buf, &mut stderr_buf, timeout, None).await
}

/// Read available stdout/stderr of PID using caller-owned scratch buffers and an
/// optional wakeup handle so push teardown cancels idle waits immediately.
pub(super) async fn read_output_into(
    pid: u32,
    stdout_buf: &mut [u8],
    stderr_buf: &mut [u8],
    timeout: u64,
    wake: Option<&Notify>,
) -> Result<PipeRead, RpcError> {
    let (stdout, stderr, lifecycle, shared_exit_status) = {
        let processes = registry().lock();
        let managed = processes
            .get(&pid)
            .filter(|m| m.is_pipe())
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {pid}")))?;
        let pipe = managed.as_pipe().expect("pipe backend");
        (
            pipe.stdout.clone(),
            pipe.stderr.clone(),
            managed.lifecycle.clone(),
            managed.shared_exit_status.clone(),
        )
    };
    // Try to read stdout/stderr (with optional blocking timeout) without
    // holding the global process map lock.
    let (stdout_result, stderr_result) =
        try_read_streams_into(stdout, stderr, stdout_buf, stderr_buf, timeout, wake).await?;

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
        let mut processes = registry().lock();
        if let Some(managed) = processes.get_mut(&pid) {
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
    // Yield to the runtime a few times so it can process SIGCHLD, then retry.
    if exit_status.is_none() && stdout_eof && stderr_eof {
        for _ in 0..5 {
            tokio::task::yield_now().await;
            let mut processes = registry().lock();
            if let Some(managed) = processes.get_mut(&pid) {
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

    // Child exit and pipe EOF are separate events.  Only report the terminal
    // state after both streams have returned EOF so the client receives all
    // buffered output before process.exit.
    let exited = exit_status.is_some() && stdout_eof && stderr_eof;

    // The terminal read is also the ownership handoff: the child has already
    // been reaped by poll_exit_status and both pipes have reached EOF.
    if exited {
        registry().lock().remove(&pid);
    }

    Ok(PipeRead {
        stdout: stdout_data,
        stderr: stderr_data,
        exited,
        exit: exit_status.filter(|_| exited),
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
    if managed.exit_status.is_none()
        && let Some(pipe) = managed.as_pipe_mut()
    {
        managed.exit_status = pipe.child.try_wait()?;
        if managed.exit_status.is_some() {
            *managed
                .shared_exit_status
                .lock()
                .expect("shared pipe exit status lock") = managed.exit_status;
        }
    }
    Ok(managed.exit_status)
}

async fn try_read_streams_into<ROut, RErr>(
    stdout: Arc<Mutex<Option<ROut>>>,
    stderr: Arc<Mutex<Option<RErr>>>,
    stdout_buf: &mut [u8],
    stderr_buf: &mut [u8],
    timeout_ms: u64,
    wake: Option<&Notify>,
) -> Result<(ReadResult, ReadResult), RpcError>
where
    ROut: AsyncRead + Unpin,
    RErr: AsyncRead + Unpin,
{
    let stdout_read = async {
        try_read_optional_stream_into(stdout, stdout_buf)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stdout: {e}")))
    };
    let stderr_read = async {
        try_read_optional_stream_into(stderr, stderr_buf)
            .await
            .map_err(|e| RpcError::process_error(format!("Failed to read stderr: {e}")))
    };
    let deadline = tokio::time::sleep(std::time::Duration::from_millis(if timeout_ms == 0 {
        1
    } else {
        timeout_ms
    }));
    let woken = async {
        match wake {
            Some(notify) => notify.notified().await,
            None => std::future::pending::<()>().await,
        }
    };

    tokio::pin!(stdout_read, stderr_read, deadline, woken);

    // AsyncReadExt::read is cancellation-safe: when one stream produces data or
    // wake fires before data arrives, dropping the other branch cannot lose bytes.
    tokio::select! {
        stdout_result = &mut stdout_read => {
            let stdout_result = stdout_result?;
            if matches!(stdout_result, ReadResult::Data(_)) {
                return Ok((stdout_result, ReadResult::Pending));
            }

            let stderr_result = tokio::select! {
                stderr_result = &mut stderr_read => stderr_result?,
                _ = &mut deadline => ReadResult::Pending,
                _ = &mut woken => ReadResult::Pending,
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
                _ = &mut woken => ReadResult::Pending,
            };
            Ok((stdout_result, stderr_result))
        }
        _ = &mut deadline => Ok((ReadResult::Pending, ReadResult::Pending)),
        _ = &mut woken => Ok((ReadResult::Pending, ReadResult::Pending)),
    }
}

async fn try_read_optional_stream_into<R>(
    stream: Arc<Mutex<Option<R>>>,
    buf: &mut [u8],
) -> std::io::Result<ReadResult>
where
    R: AsyncRead + Unpin,
{
    let mut stream_guard = stream.lock().await;
    if let Some(reader) = stream_guard.as_mut() {
        let result = try_read_async_into(reader, buf).await?;
        if matches!(result, ReadResult::Eof) {
            *stream_guard = None;
        }
        Ok(result)
    } else {
        Ok(ReadResult::Eof)
    }
}

async fn try_read_async_into<R: AsyncRead + Unpin>(
    reader: &mut R,
    buf: &mut [u8],
) -> std::io::Result<ReadResult> {
    match reader.read(buf).await {
        Ok(0) => Ok(ReadResult::Eof),
        Ok(n) => Ok(ReadResult::Data(buf[..n].to_vec())),
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
        let processes = registry().lock();
        processes
            .get(&params.pid)
            .and_then(ManagedProcess::as_pipe)
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
    let mut stdin_guard = stdin.lock().await;
    if let Some(mut stdin) = stdin_guard.take() {
        let _ = stdin.flush().await;
    }

    Ok(Value::Boolean(true))
}

/// Wait until the managed child behind registry PID is reaped.
pub(super) async fn wait_pipe_child(pid: u32) -> Result<Option<ExitStatus>, RpcError> {
    loop {
        {
            let mut processes = registry().lock();
            let Some(managed) = processes.get_mut(&pid) else {
                // Another poll consumed the status and removed the entry.
                return Ok(None);
            };
            if let Some(status) = poll_exit_status(managed).map_err(|error| {
                RpcError::process_error(format!("Failed to reap process {pid}: {error}"))
            })? {
                return Ok(Some(status));
            }
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
}

/// Remove PID from the registry, reaping the direct child in the background
/// when it was not confirmed dead.
pub(super) async fn retire_pipe_process(pid: u32) {
    let removed = registry().lock().remove(&pid);
    if let Some(managed) = removed
        && managed.exit_status.is_none()
        && let ProcessBackend::Pipe(mut pipe) = managed.backend
    {
        tokio::spawn(async move {
            let _ = pipe.child.wait().await;
        });
    }
}

/// Return whether this call found and handled the registry entry.
/// For explicit `SIGKILL`, a missing entry is an idempotent natural-exit race
/// and returns `Ok(false)` (mirroring `terminate_pty_process`).
pub(super) async fn terminate_pipe_process(
    pid: u32,
    signal: i32,
    escalate: bool,
) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle, shared_exit_status)) = ({
        let processes = registry().lock();
        processes.get(&pid).filter(|m| m.is_pipe()).map(|managed| {
            (
                managed.child_pid,
                managed.lifecycle.clone(),
                managed.shared_exit_status.clone(),
            )
        })
    }) else {
        return if signal == libc::SIGKILL || escalate {
            Ok(false)
        } else {
            Err(RpcError::process_error(format!("Process not found: {pid}")))
        };
    };

    let _lifecycle_guard = lifecycle.lock().await;
    require_process_group_signal(signal_process_group(os_pid, signal), "send signal")?;

    if matches!(signal, 0 | libc::SIGSTOP | libc::SIGCONT) {
        return Ok(true);
    }

    let cached = registry()
        .lock()
        .get(&pid)
        .and_then(|managed| managed.exit_status);
    if cached.is_some() && !escalate {
        if signal == libc::SIGKILL {
            *shared_exit_status
                .lock()
                .expect("shared pipe exit status lock") = cached;
            retire_pipe_process(pid).await;
        }
        return Ok(true);
    }

    if !(escalate || signal == libc::SIGTERM || signal == libc::SIGKILL) {
        return Ok(true);
    }

    let mut reap = if cached.is_some() {
        cached
    } else {
        tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(pid))
            .await
            .ok()
            .and_then(Result::ok)
            .flatten()
    };
    if escalate
        && wait_for_process_group_exit(os_pid, tokio::time::Instant::now() + MANAGED_CHILD_WAIT)
            .await
    {
        require_process_group_signal(signal_process_group(os_pid, libc::SIGKILL), "send SIGKILL")?;
    }
    if reap.is_none() && escalate {
        reap = match tokio::time::timeout(MANAGED_CHILD_WAIT, wait_pipe_child(pid)).await {
            Ok(Ok(status)) => status,
            Ok(Err(error)) => {
                retire_pipe_process(pid).await;
                return Err(error);
            }
            Err(_) => {
                retire_pipe_process(pid).await;
                return Err(RpcError::process_error(format!(
                    "Timed out reaping process {pid} after SIGKILL"
                )));
            }
        };
    }
    if let Some(exit_status) = reap {
        *shared_exit_status
            .lock()
            .expect("shared pipe exit status lock") = Some(exit_status);
        if let Some(managed) = registry().lock().get_mut(&pid) {
            managed.exit_status = Some(exit_status);
        }
        if signal == libc::SIGKILL {
            retire_pipe_process(pid).await;
        }
        return Ok(true);
    }
    if signal == libc::SIGKILL {
        retire_pipe_process(pid).await;
    }

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
    let marked_terminating = signal == libc::SIGKILL;
    // The output push owns the read loop.  Stop it before destructive
    // SIGKILL so it cannot race registry removal or consume final output.
    let (push, shared_exit_status) = {
        let mut processes = registry().lock();
        let managed = processes
            .get_mut(&params.pid)
            .filter(|m| m.is_pipe())
            .ok_or_else(|| RpcError::process_error(format!("Process not found: {}", params.pid)))?;
        if managed.terminating {
            return Err(RpcError::process_error(format!(
                "Process is already terminating: {}",
                params.pid
            )));
        }
        if marked_terminating {
            managed.terminating = true;
            (
                managed.output_push.take(),
                Some(Arc::clone(&managed.shared_exit_status)),
            )
        } else {
            (None, None)
        }
    };
    let pushing = push.is_some();
    if let Some(push) = push {
        stop_output_push(push).await;
    }
    let handled = match terminate_pipe_process(params.pid, signal, false).await {
        Ok(handled) => handled,
        Err(error) => {
            // Only roll back `terminating` if THIS call set it; a failed
            // non-SIGKILL signal must never clobber a concurrent SIGKILL or
            // connection teardown.
            if marked_terminating && let Some(managed) = registry().lock().get_mut(&params.pid) {
                managed.terminating = false;
                if pushing && managed.output_push.is_none() {
                    managed.output_push = Some(new_pipe_push(params.pid));
                }
            }
            return Err(error);
        }
    };
    // SIGKILL removed the entry, so no push task can report this exit.  Only
    // the caller that still owned the registry entry reports SIGKILL.
    if marked_terminating && handled {
        let status = shared_exit_status
            .as_ref()
            .and_then(|status| *status.lock().expect("shared pipe exit status lock"))
            .unwrap_or_else(|| ExitStatus::from_raw(signal));
        send_exit_notification(params.pid, Some(status)).await;
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
