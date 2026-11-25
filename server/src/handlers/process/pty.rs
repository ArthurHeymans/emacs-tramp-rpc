// SPDX-License-Identifier: GPL-3.0-or-later

//! PTY (pseudo-terminal) process handlers.

use crate::msgpack_map;
use crate::protocol::{RpcError, RpcPath, from_value};
use nix::pty::{OpenptyResult, openpty};
use rmpv::Value;
use rustix::process::{
    Pid as RustixPid, Signal as RustixSignal, WaitOptions, WaitStatus, kill_process_group, waitpid,
};
use rustix::termios::{
    LocalModes, OptionalActions, OutputModes, Winsize, tcgetattr, tcgetpgrp, tcsetattr,
    tcsetwinsize,
};
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd};
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::process::{Command as StdCommand, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use tokio::io::Interest;
use tokio::io::unix::AsyncFd;
use tokio::sync::{Mutex, Notify};

use super::super::HandlerResult;
#[cfg(test)]
use super::MAX_PROCESS_READ_BYTES;
use super::push::{new_pty_push, send_exit_notification, stop_output_push};
#[cfg(target_os = "macos")]
use super::signal_process;
use super::{
    MANAGED_PTY_CHILD_WAIT, ManagedProcess, ProcessBackend, PtyBackend, SignalCode, dup_cloexec,
    registry, require_process_group_signal, set_fd_cloexec, set_fd_nonblocking,
    signal_process_group, wait_for_process_group_exit,
};

pub(super) struct PtyIoState {
    // The write lock covers the whole logical input stream, not just one
    // syscall, so concurrent requests cannot interleave their bytes.
    pub(super) write_lock: Mutex<()>,
    // Serialize the close transition with the final nonblocking write syscall.
    pub(super) syscall_lock: StdMutex<()>,
    // This is retained state: cancellation publishes `closed` before the
    // permit notification.  There is at most one waiter because writes are
    // serialized, so notify_one cannot lose a wakeup.
    pub(super) closed: AtomicBool,
    pub(super) cancelled: Notify,
}

impl PtyIoState {
    /// Publish cancellation after any in-flight write syscall finishes.
    pub(super) fn cancel(&self) {
        let _syscall_guard = self.syscall_lock.lock().expect("PTY syscall lock");
        if !self.closed.swap(true, Ordering::AcqRel) {
            self.cancelled.notify_one();
        }
    }

    pub(super) fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire)
    }

    /// Withdraw a cancellation whose motivating signal was not delivered.
    /// A writer already woken by `cancel` may still observe the error once;
    /// the `is_closed` re-check in `write_pty` lets later writes proceed.
    pub(super) fn reopen(&self) {
        let _syscall_guard = self.syscall_lock.lock().expect("PTY syscall lock");
        self.closed.store(false, Ordering::Release);
    }
}

pub(super) fn set_window_size<Fd: AsFd>(
    fd: Fd,
    rows: u16,
    cols: u16,
) -> Result<(), std::io::Error> {
    let ws = Winsize {
        ws_row: rows,
        ws_col: cols,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };
    Ok(tcsetwinsize(fd, ws)?)
}

/// Resolve the terminal's foreground group, falling back to its leader.
fn foreground_pty_group(fd: impl AsFd, child_pid: u32) -> u32 {
    tcgetpgrp(fd)
        .ok()
        .map(|group| group.as_raw_nonzero().get() as u32)
        .unwrap_or(child_pid)
}

pub(super) fn signal_pty_process_group(
    pid: u32,
    signal: i32,
    action: &str,
) -> Result<(), RpcError> {
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

#[derive(Clone)]
pub(super) struct PtyStartParams {
    pub(super) cmd: String,
    pub(super) args: Vec<String>,
    pub(super) cwd: Option<RpcPath>,
    pub(super) env: Option<HashMap<String, String>>,
    pub(super) clear_env: bool,
    pub(super) rows: u16,
    pub(super) cols: u16,
}

pub(super) struct PtyStartupGuard {
    pub(super) master_fd: Option<OwnedFd>,
    pub(super) child: Option<std::process::Child>,
    pub(super) tty_name: String,
}

impl PtyStartupGuard {
    fn master_fd(&self) -> BorrowedFd<'_> {
        self.master_fd.as_ref().expect("PTY master fd").as_fd()
    }

    fn take_master_fd(&mut self) -> OwnedFd {
        self.master_fd.take().expect("PTY master fd")
    }

    fn disarm(mut self) -> (u32, String) {
        let child = self.child.take().expect("PTY child");
        let child_pid = child.id();
        // Child does not kill or reap on Drop.  Once registered, lifecycle
        // handlers own reaping through waitpid.
        drop(child);
        (child_pid, std::mem::take(&mut self.tty_name))
    }
}

pub(super) fn spawn_async_pty_startup_reaper(mut child: std::process::Child) {
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

pub(super) fn reap_pty_startup_child_with<F>(child: std::process::Child, spawn_reaper: F)
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

pub(super) fn spawn_pty_startup_reaper(child: std::process::Child) {
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
        // Dropping an aborted spawn_blocking result must not orphan its child.
        // Reap on a detached thread so a pathological child cannot block a
        // Tokio worker while the master fd closes through OwnedFd.
        if let Some(pgid) = i32::try_from(child.id()).ok().and_then(RustixPid::from_raw) {
            let _ = kill_process_group(pgid, RustixSignal::KILL);
        }
        spawn_pty_startup_reaper(child);
    }
}

pub(super) fn do_fork_exec(params: PtyStartParams) -> Result<PtyStartupGuard, RpcError> {
    let OpenptyResult { master, slave } = openpty(None, None)
        .map_err(|e| RpcError::process_error(format!("Failed to open PTY: {e}")))?;

    // Emacs inserts interactive input into comint buffers itself.  Disable
    // kernel echo to avoid delivering every input line twice, and preserve LF
    // output instead of the terminal driver's CRLF conversion.
    let mut termios = tcgetattr(&slave)
        .map_err(|e| RpcError::process_error(format!("Failed to read PTY termios: {e}")))?;
    termios
        .local_modes
        .remove(LocalModes::ECHO | LocalModes::ECHONL);
    termios.output_modes.remove(OutputModes::ONLCR);
    tcsetattr(&slave, OptionalActions::Now, &termios)
        .map_err(|e| RpcError::process_error(format!("Failed to configure PTY termios: {e}")))?;

    set_fd_cloexec(&master)
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {e}")))?;
    set_fd_cloexec(&slave)
        .map_err(|e| RpcError::process_error(format!("Failed to mark PTY CLOEXEC: {e}")))?;

    let tty_name = rustix::termios::ttyname(&slave, Vec::new())
        .map(|name| String::from_utf8_lossy(name.as_bytes()).into_owned())
        .map_err(|e| RpcError::process_error(format!("Failed to get tty name: {e}")))?;

    set_window_size(&master, params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to set window size: {e}")))?;

    let mut cmd = StdCommand::new(&params.cmd);
    cmd.args(&params.args);

    if let Some(cwd) = &params.cwd {
        cmd.current_dir(cwd.resolve_sync());
    }

    if params.clear_env {
        cmd.env_clear();
    }

    if let Some(env) = &params.env {
        cmd.envs(env);
    }

    let slave_fd = slave.as_raw_fd();
    let master_fd = master.as_raw_fd();
    // Every duplicate is an `OwnedFd`, so a later duplication failure closes
    // the descriptors already acquired.
    let dup_slave = || {
        dup_cloexec(&slave)
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))
    };
    let stdin_fd = dup_slave()?;
    let stdout_fd = dup_slave()?;
    let stderr_fd = dup_slave()?;

    cmd.stdin(Stdio::from(stdin_fd));
    cmd.stdout(Stdio::from(stdout_fd));
    cmd.stderr(Stdio::from(stderr_fd));

    // SAFETY: the pre-exec hook runs in the forked child and only performs
    // async-signal-safe syscalls (close, setsid, ioctl).  Guard against
    // master_fd/slave_fd being in 0..=2 (e.g. if the server inherited closed
    // stdio slots), because Stdio::from already dup2'd stdio onto 0..=2.
    unsafe {
        cmd.pre_exec(move || {
            if master_fd > 2 {
                rustix::io::close(master_fd);
            }
            rustix::process::setsid()?;
            rustix::process::ioctl_tiocsctty(BorrowedFd::borrow_raw(slave_fd))?;
            if slave_fd > 2 {
                rustix::io::close(slave_fd);
            }
            Ok(())
        });
    }

    let child = cmd
        .spawn()
        .map_err(|e| RpcError::process_error(format!("Failed to spawn PTY process: {e}")))?;
    drop(slave);

    Ok(PtyStartupGuard {
        master_fd: Some(master),
        child: Some(child),
        tty_name,
    })
}

/// Start a process with a PTY (pseudo-terminal), returning both the RPC value
/// and the allocated PID so the dispatcher can arm output push without
/// re-parsing the response map.
pub(crate) async fn start_pty_outcome(params: Value) -> Result<(Value, u32), RpcError> {
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
        .map_err(|e| RpcError::process_error(format!("Task join error: {e}")))??;

    set_fd_nonblocking(startup.master_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to set non-blocking: {e}")))?;

    let async_fd = Arc::new(
        AsyncFd::new(startup.take_master_fd())
            .map_err(|e| RpcError::process_error(format!("Failed to create AsyncFd: {e}")))?,
    );

    let our_pid = registry().next_pid();
    let mut processes = registry().lock();
    // Disarm only after the final await.  Cancellation anywhere before this
    // point drops STARTUP, closes the PTY, and kills/reaps the unregistered child.
    let (child_pid, tty_name) = startup.disarm();

    let managed = ManagedProcess {
        child_pid,
        lifecycle: Arc::new(Mutex::new(())),
        exit_status: None,
        shared_exit_status: Arc::new(StdMutex::new(None)),
        output_push: None,
        terminating: false,
        backend: ProcessBackend::Pty(PtyBackend {
            async_fd,
            io: Arc::new(PtyIoState {
                write_lock: Mutex::new(()),
                syscall_lock: StdMutex::new(()),
                closed: AtomicBool::new(false),
                cancelled: Notify::new(),
            }),
        }),
    };

    processes.insert(our_pid, managed);
    drop(processes);

    let value = msgpack_map! {
        "pid" => our_pid,
        "os_pid" => child_pid,
        "tty_name" => tty_name
    };
    Ok((value, our_pid))
}

/// Start a process with a PTY (pseudo-terminal).
pub async fn start_pty(params: Value) -> HandlerResult {
    start_pty_outcome(params).await.map(|(value, _)| value)
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
        let processes = registry().lock();
        processes
            .get(&params.pid)
            .filter(|m| m.is_pty())
            .map(|managed| managed.lifecycle.clone())
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?
    };
    let _lifecycle_guard = lifecycle.lock().await;
    let (async_fd, child_pid, io) = {
        // Re-check the registry after acquiring lifecycle ownership: close or
        // terminal read may have removed the process while resize was waiting.
        let processes = registry().lock();
        let managed = processes
            .get(&params.pid)
            .filter(|m| m.is_pty())
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?;
        let pty = managed.as_pty().expect("verified PTY backend");
        (
            Arc::clone(&pty.async_fd),
            managed.child_pid,
            Arc::clone(&pty.io),
        )
    };
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY process is closed: {}",
            params.pid
        )));
    }

    set_window_size(async_fd.get_ref(), params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to resize PTY: {e}")))?;

    let group = foreground_pty_group(async_fd.get_ref(), child_pid);
    if let Some(pgid) = i32::try_from(group).ok().and_then(RustixPid::from_raw) {
        let _ = kill_process_group(pgid, RustixSignal::WINCH);
    }

    Ok(Value::Boolean(true))
}

/// Read from a PTY process with optional blocking
#[cfg(test)]
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
    if timeout > 0 {
        if result.pending {
            let _ = tokio::time::timeout(
                std::time::Duration::from_millis(timeout),
                wait_for_pty_readable(params.pid),
            )
            .await;
            result = read_pty_now(params.pid, params.max_bytes).await?;
        } else if result.output.is_empty() && !result.exited {
            // PTY master reached EOF before the child finished exiting; back
            // off briefly rather than spinning tight waitpid(WNOHANG) loops.
            tokio::time::sleep(std::time::Duration::from_millis(timeout.min(5))).await;
            result = read_pty_now(params.pid, params.max_bytes).await?;
        }
    }

    let output = if result.output.is_empty() {
        Value::Nil
    } else {
        Value::Binary(result.output)
    };
    let mut pairs = vec![
        (Value::String("output".into()), output),
        (
            Value::String("exited".into()),
            Value::Boolean(result.exited),
        ),
    ];
    pairs.extend(crate::protocol::exit_fields(result.exit));
    Ok(Value::Map(pairs))
}

#[cfg(test)]
pub(super) struct PtyReadResult {
    pub(super) output: Vec<u8>,
    pub(super) pending: bool,
    pub(super) exited: bool,
    pub(super) exit: Option<ExitStatus>,
}

pub(super) struct PtyReadIntoResult {
    pub(super) bytes_read: usize,
    pub(super) pending: bool,
    pub(super) exited: bool,
    pub(super) exit: Option<ExitStatus>,
}

/// Non-blocking PTY read into a caller-supplied buffer.
///
/// Uses the shared `AsyncFd::try_io` so Tokio clears stale edge-triggered
/// `READABLE` readiness atomically on `WouldBlock` without per-read
/// `dup`/`epoll_ctl`/`close` churn.
/// Per-PID count of `read_pty_into` calls, for tests of read pacing.
#[cfg(test)]
pub(super) static PTY_READ_CALLS: std::sync::LazyLock<StdMutex<HashMap<u32, usize>>> =
    std::sync::LazyLock::new(Default::default);

pub(super) async fn read_pty_into(pid: u32, buf: &mut [u8]) -> Result<PtyReadIntoResult, RpcError> {
    #[cfg(test)]
    {
        *PTY_READ_CALLS
            .lock()
            .expect("PTY read counter lock")
            .entry(pid)
            .or_default() += 1;
    }
    let (lifecycle, io, shared_exit_status, async_fd) = {
        let processes = registry().lock();
        let Some(managed) = processes.get(&pid) else {
            return Ok(PtyReadIntoResult {
                bytes_read: 0,
                pending: false,
                exited: true,
                exit: None,
            });
        };
        let Some(pty) = managed.as_pty() else {
            return Ok(PtyReadIntoResult {
                bytes_read: 0,
                pending: false,
                exited: true,
                exit: None,
            });
        };
        (
            Arc::clone(&managed.lifecycle),
            Arc::clone(&pty.io),
            Arc::clone(&managed.shared_exit_status),
            Arc::clone(&pty.async_fd),
        )
    };
    let _lifecycle_guard = lifecycle.lock().await;
    let mut processes = registry().lock();
    let Some(managed) = processes.get_mut(&pid) else {
        // Explicit SIGKILL intentionally discards output and removes registry
        // ownership.  A read already in flight still owns this status handle,
        // so report the terminal remote result rather than local success.
        let shared_status = *shared_exit_status
            .lock()
            .expect("shared PTY exit status lock");
        return Ok(PtyReadIntoResult {
            bytes_read: 0,
            pending: false,
            exited: true,
            exit: shared_status,
        });
    };

    // EOF is provisional while the child runs: it can close its last slave
    // descriptor, reopen the slave and write again, so every pass reads.
    let (bytes_read, pending, eof) = {
        let read_once = |fd: &OwnedFd, slice: &mut [u8]| -> std::io::Result<usize> {
            rustix::io::read(fd, slice).map_err(std::io::Error::from)
        };
        // Drive readiness state through `AsyncFd::try_io` so edge-triggered
        // readiness is cleared atomically on `WouldBlock`.  If readiness was
        // already cleared (e.g. direct non-blocking poll before the reactor
        // ticked), fall back to a single non-blocking syscall so fresh bytes
        // are still observed immediately without touching readiness bits.
        let mut attempted = false;
        let io_result = match async_fd.try_io(Interest::READABLE, |inner| {
            attempted = true;
            read_once(inner, buf)
        }) {
            Err(err) if !attempted && err.kind() == ErrorKind::WouldBlock => {
                read_once(async_fd.get_ref(), buf)
            }
            res => res,
        };
        match io_result {
            Ok(n) if n > 0 => (n, false, false),
            Ok(_) => (0, false, true),
            Err(err) if err.kind() == ErrorKind::WouldBlock => (0, true, false),
            // Linux reports PTY master EOF as EIO after the slave closes.
            Err(err) if err.raw_os_error() == Some(libc::EIO) => (0, false, true),
            Err(err) => {
                return Err(RpcError::process_error(format!(
                    "Failed to read PTY: {err}"
                )));
            }
        }
    };
    // Retire the entry only when this read found EOF after the child exited.
    let (child_exited, exit_status) = check_exit_status(managed);
    let exited = child_exited && eof;
    drop(processes);
    if exited {
        // Do not hold the registry lock while waiting for an in-flight write
        // syscall to finish.  The lifecycle guard keeps close/kill ordered
        // with this terminal read.
        io.cancel();
        registry().lock().remove(&pid);
    }
    Ok(PtyReadIntoResult {
        bytes_read,
        pending,
        exited,
        exit: exited.then_some(exit_status).flatten(),
    })
}

#[cfg(test)]
pub(super) async fn read_pty_now(pid: u32, max_bytes: usize) -> Result<PtyReadResult, RpcError> {
    let mut buf = vec![0u8; max_bytes];
    let result = read_pty_into(pid, &mut buf).await?;
    buf.truncate(result.bytes_read);
    Ok(PtyReadResult {
        output: buf,
        pending: result.pending,
        exited: result.exited,
        exit: result.exit,
    })
}

/// Convert a terminal rustix `WaitStatus` into a `std::process::ExitStatus`.
pub(super) fn exit_status_from_wait_status(status: WaitStatus) -> ExitStatus {
    ExitStatus::from_raw(status.as_raw())
}

pub(super) fn check_exit_status(managed: &mut ManagedProcess) -> (bool, Option<ExitStatus>) {
    if managed.exit_status.is_none() {
        let Some(rpid) = i32::try_from(managed.child_pid)
            .ok()
            .and_then(RustixPid::from_raw)
        else {
            return (false, None);
        };
        let status = match waitpid(Some(rpid), WaitOptions::NOHANG) {
            Ok(Some((_, status))) if status.exited() || status.signaled() => {
                exit_status_from_wait_status(status)
            }
            _ => return (false, None),
        };
        managed.exit_status = Some(status);
        *managed
            .shared_exit_status
            .lock()
            .expect("shared PTY exit status lock") = Some(status);
    }
    (true, managed.exit_status)
}

/// Wait until the PTY master fd becomes readable.
///
/// Returns `Ok(true)` when readable, `Ok(false)` when the process is no longer
/// registered (so the push loop should terminate), or `Err` on reactor failure.
pub(super) async fn wait_for_pty_readable(pid: u32) -> Result<bool, std::io::Error> {
    let async_fd = {
        let processes = registry().lock();
        let Some(pty) = processes.get(&pid).and_then(ManagedProcess::as_pty) else {
            return Ok(false);
        };
        Arc::clone(&pty.async_fd)
    };
    let _guard = async_fd.readable().await?;
    Ok(true)
}

pub(super) enum PtyWriteAction {
    Progress,
    Retry,
}

pub(super) fn apply_pty_write(
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

    // Capture the shared AsyncFd and cancellation state while briefly holding
    // the registry lock.  All waits and writes happen on owned Arcs afterwards.
    let (async_fd, io) = {
        let processes = registry().lock();
        let pty = processes
            .get(&params.pid)
            .and_then(ManagedProcess::as_pty)
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?;
        (Arc::clone(&pty.async_fd), Arc::clone(&pty.io))
    };
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
                if io.is_closed() {
                    return Err(RpcError::process_error(format!(
                        "PTY write cancelled: {}", params.pid
                    )));
                }
                // The cancellation was withdrawn (a signal that motivated it
                // failed); ignore the stale notification and retry.
                continue;
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
            rustix::io::write(inner.get_ref(), &data[offset..]).map_err(std::io::Error::from)
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

pub(super) async fn wait_pty_pid(
    child_pid: u32,
    discard_output: Option<&OwnedFd>,
) -> Result<Option<ExitStatus>, rustix::io::Errno> {
    let rpid = i32::try_from(child_pid)
        .ok()
        .and_then(RustixPid::from_raw)
        .ok_or(rustix::io::Errno::INVAL)?;
    loop {
        if let Some(fd) = discard_output {
            // Darwin can wait in ttywait during exit, even after SIGKILL, until
            // the master consumes pending output.  Explicit close/SIGKILL opts
            // out of delivery; ordinary signals must leave that output intact.
            // Bound each drain so an active descendant cannot starve the reap.
            let mut buffer = [0; 4096];
            for _ in 0..16 {
                match rustix::io::read(fd, &mut buffer) {
                    Ok(0) => break,
                    Ok(_) | Err(rustix::io::Errno::INTR) => {}
                    Err(_) => break,
                }
            }
        }
        match waitpid(Some(rpid), WaitOptions::NOHANG) {
            Ok(Some((_, status))) if status.exited() || status.signaled() => {
                return Ok(Some(exit_status_from_wait_status(status)));
            }
            Ok(None) => {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
            }
            Err(rustix::io::Errno::CHILD) => return Ok(None),
            Err(rustix::io::Errno::INTR) => continue,
            Err(error) => return Err(error),
            Ok(Some(_)) => return Err(rustix::io::Errno::INVAL),
        }
    }
}

/// Remove PID from the PTY registry, reaping the direct child in the
/// background when it was not confirmed dead.
///
/// Dropping the entry closes the PTY master (which sends SIGHUP), but an
/// unreaped child would still remain a zombie for the life of the server.
pub(super) async fn retire_pty_process(pid: u32, os_pid: u32) {
    let removed = registry().lock().remove(&pid);
    if let Some(managed) = removed
        && managed.exit_status.is_none()
    {
        tokio::spawn(async move {
            let _ = wait_pty_pid(os_pid, None).await;
        });
    }
}

/// Return whether this call found and handled the registry entry.
/// A missing entry is an idempotent removal, but does not own an exit notification.
pub(super) async fn terminate_pty_process(
    pid: u32,
    signal: i32,
    escalate: bool,
    remove: bool,
) -> Result<bool, RpcError> {
    let Some((os_pid, lifecycle, io, shared_exit_status, async_fd)) = ({
        let processes = registry().lock();
        processes.get(&pid).and_then(|managed| {
            let pty = managed.as_pty()?;
            Some((
                managed.child_pid,
                Arc::clone(&managed.lifecycle),
                Arc::clone(&pty.io),
                Arc::clone(&managed.shared_exit_status),
                Arc::clone(&pty.async_fd),
            ))
        })
    }) else {
        return if remove {
            Ok(false)
        } else {
            Err(RpcError::process_error(format!(
                "PTY process not found: {pid}"
            )))
        };
    };
    // Explicit teardown and SIGKILL must wake a writer blocked on readiness
    // immediately, before waiting for the lifecycle lock, so a reader or
    // writer can be released while the signal is still being delivered.
    // Other forwarded signals (notably SIGINT) are survivable for an
    // interactive shell, so leave the PTY I/O state usable until a terminal
    // exit has actually been confirmed.
    let cancelled_early = remove || signal == libc::SIGKILL;
    if cancelled_early {
        io.cancel();
    }
    let _lifecycle_guard = lifecycle.lock().await;
    if let Err(error) = signal_pty_process_group(os_pid, signal, "send signal") {
        // A failed signal (EPERM from a surviving credential-changing
        // descendant) must leave the PTY usable; otherwise the entry stays
        // registered with every later write rejected.
        if cancelled_early {
            io.reopen();
        }
        return Err(error);
    }

    if matches!(signal, 0 | libc::SIGSTOP | libc::SIGCONT) {
        return Ok(true);
    }

    let cached = registry()
        .lock()
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
            registry().lock().remove(&pid);
        }
        return Ok(true);
    }

    // As with pipe children: only wait for death on signals expected to
    // terminate the child; a surviving SIGINT/SIGUSR1 must not stall the
    // client for the wait budget.  Exits are reaped by later read polls.
    if !(escalate || signal == libc::SIGTERM || signal == libc::SIGKILL) {
        return Ok(true);
    }

    let discard_fd = remove.then(|| async_fd.get_ref());
    let mut reap = if cached.is_some() {
        cached
    } else {
        tokio::time::timeout(MANAGED_PTY_CHILD_WAIT, wait_pty_pid(os_pid, discard_fd))
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
        // The escalation reap is the last chance to observe the child.  Use the
        // PTY budget for it as well, and retire the entry on failure so a
        // single unreaped child cannot poison the shared registry for every
        // later test or connection.
        reap = match tokio::time::timeout(MANAGED_PTY_CHILD_WAIT, wait_pty_pid(os_pid, discard_fd))
            .await
        {
            Ok(Ok(status)) => status,
            Ok(Err(error)) => {
                retire_pty_process(pid, os_pid).await;
                return Err(RpcError::process_error(format!(
                    "Failed to reap PTY process {pid}: {error}"
                )));
            }
            Err(_) => {
                retire_pty_process(pid, os_pid).await;
                return Err(RpcError::process_error(format!(
                    "Timed out reaping PTY process {pid} after SIGKILL"
                )));
            }
        };
    }
    if let Some(exit_code) = reap {
        // Reaping confirms terminal death, so no further PTY input can be
        // admitted.  This also wakes a writer that was waiting for readiness.
        io.cancel();
        // Kill reaps the direct child but deliberately leaves the PTY master
        // registered until read_pty observes terminal EOF, unless this is an
        // explicit close.
        let mut processes = registry().lock();
        if remove {
            // Publish before discarding registry ownership.  An in-flight
            // read holds this Arc and must report the killed remote process,
            // not nil/zero local relay success.
            *shared_exit_status
                .lock()
                .expect("shared PTY exit status lock") = Some(exit_code);
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
        shared_exit_status
            .lock()
            .expect("shared PTY exit status lock")
            .get_or_insert(ExitStatus::from_raw(libc::SIGKILL));
        retire_pty_process(pid, os_pid).await;
        return Ok(true);
    }

    // As for pipe children: delivery is success; termination is not
    // guaranteed within the bounded wait and the entry stays for escalation
    // and draining.
    Ok(true)
}

/// Signal a PTY's leader group or, when requested, its foreground job.
/// Foreground delivery does not terminate or retire the managed shell.
pub async fn kill_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
        #[serde(default = "default_pty_signal")]
        signal: SignalCode,
        #[serde(default)]
        group: Option<String>,
    }

    fn default_pty_signal() -> SignalCode {
        SignalCode::Number(libc::SIGTERM)
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let signal = params.signal.resolve()?;
    if let Some(group) = params.group {
        let skip_leader = match group.as_str() {
            "foreground" => false,
            "foreground_unless_leader" => true,
            _ => {
                return Err(RpcError::invalid_params(format!(
                    "Unknown PTY signal group: {group}"
                )));
            }
        };
        let (async_fd, child_pid) = {
            let processes = registry().lock();
            let managed = processes
                .get(&params.pid)
                .filter(|m| m.is_pty())
                .ok_or_else(|| {
                    RpcError::process_error(format!("PTY process not found: {}", params.pid))
                })?;
            let pty = managed.as_pty().expect("verified PTY backend");
            if pty.io.is_closed() {
                return Err(RpcError::process_error(format!(
                    "PTY process is closed: {}",
                    params.pid
                )));
            }
            (Arc::clone(&pty.async_fd), managed.child_pid)
        };
        let foreground = foreground_pty_group(async_fd.get_ref(), child_pid);
        if !skip_leader || foreground != child_pid {
            signal_pty_process_group(foreground, signal, "send foreground signal")?;
        }
        // Even SIGKILL here targets a job, not the lifetime of the relay.
        // Normal output/exit delivery retains ownership and drains its bytes.
        return Ok(Value::Boolean(true));
    }
    let marked_terminating = signal == libc::SIGKILL;
    let (push, shared_exit_status) = {
        let mut processes = registry().lock();
        let managed = processes
            .get_mut(&params.pid)
            .filter(|m| m.is_pty())
            .ok_or_else(|| {
                RpcError::process_error(format!("PTY process not found: {}", params.pid))
            })?;
        if managed.terminating {
            return Err(RpcError::process_error(format!(
                "PTY process is already terminating: {}",
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
    // Match local signal-process semantics: forward the requested signal
    // without turning a survivable signal such as SIGINT into SIGKILL.
    // Explicit close and connection cleanup retain escalation authority.
    // Explicit SIGKILL also opts out of output draining.
    let handled = match terminate_pty_process(params.pid, signal, false, marked_terminating).await {
        Ok(handled) => handled,
        Err(error) => {
            // Only roll back `terminating` if THIS call set it; a failed
            // non-SIGKILL signal must never clobber a concurrent SIGKILL,
            // close, or connection teardown.
            if marked_terminating {
                allow_pty_termination_retry(params.pid, pushing);
            }
            return Err(error);
        }
    };
    // A push iteration can finish and report natural exit while being stopped.
    // Only the caller that still owned the registry entry reports SIGKILL.
    if marked_terminating && handled {
        let status = shared_exit_status
            .as_ref()
            .and_then(|status| *status.lock().expect("shared PTY exit status lock"))
            .unwrap_or_else(|| ExitStatus::from_raw(signal));
        send_exit_notification(params.pid, Some(status)).await;
    }
    Ok(Value::Boolean(true))
}

/// Let a later kill or close retry after a failed termination, and resume
/// pushing output when PUSHING was stopped for it: the process may live on.
fn allow_pty_termination_retry(pid: u32, pushing: bool) {
    if let Some(managed) = registry().lock().get_mut(&pid) {
        managed.terminating = false;
        if pushing && managed.output_push.is_none() {
            managed.output_push = Some(new_pty_push(pid));
        }
    }
}

/// Close a PTY process and discard buffered output.  Repeating close is harmless.
pub async fn close_pty(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        pid: u32,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let push = {
        let mut processes = registry().lock();
        match processes.get_mut(&params.pid) {
            Some(managed) if !managed.is_pty() => None,
            Some(managed) if managed.terminating => {
                return Err(RpcError::process_error(format!(
                    "PTY process is already terminating: {}",
                    params.pid
                )));
            }
            Some(managed) => {
                managed.terminating = true;
                managed.output_push.take()
            }
            None => None,
        }
    };
    let pushing = push.is_some();
    if let Some(push) = push {
        stop_output_push(push).await;
    }
    // Explicit close is the opt-out from kill's drain-preserving ownership.
    if let Err(error) = terminate_pty_process(params.pid, libc::SIGKILL, true, true).await {
        allow_pty_termination_retry(params.pid, pushing);
        return Err(error);
    }
    Ok(Value::Boolean(true))
}
