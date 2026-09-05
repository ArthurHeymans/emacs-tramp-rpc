// SPDX-License-Identifier: GPL-3.0-or-later

//! PTY (pseudo-terminal) process handlers.

use crate::msgpack_map;
use crate::protocol::{RpcError, from_value};
use nix::pty::{OpenptyResult, openpty};
use nix::sys::signal::Signal;
use nix::sys::termios::{LocalFlags, OutputFlags, SetArg, tcgetattr, tcsetattr};
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::{Pid, tcgetpgrp};
use rmpv::Value;
use rustix::process::{Pid as RustixPid, Signal as RustixSignal};
use rustix::termios::{Winsize, tcsetwinsize};
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd};
use std::os::unix::process::CommandExt;
use std::process::{Command as StdCommand, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex as StdMutex, OnceLock};
use tokio::io::Interest;
use tokio::io::unix::AsyncFd;
use tokio::sync::{Mutex, Notify};

use super::super::HandlerResult;
use super::super::system::expand_tilde;
#[cfg(target_os = "macos")]
use super::signal_process;
use super::subscription::{PushSubscription, send_process_notification, stop_push_subscription};
use super::{
    MANAGED_CHILD_WAIT, MANAGED_PTY_CHILD_WAIT, MAX_PROCESS_READ_BYTES, SignalCode, dup_cloexec,
    require_process_group_signal, set_fd_cloexec, set_fd_nonblocking, signal_process_group,
    wait_for_process_group_exit,
};

pub(super) static PTY_PROCESS_MAP: OnceLock<Mutex<HashMap<u32, ManagedPtyProcess>>> =
    OnceLock::new();
pub(super) static TERMINATED_PTY_STATUSES: OnceLock<StdMutex<HashMap<u32, i32>>> = OnceLock::new();
pub(super) static PTY_PID_COUNTER: OnceLock<Mutex<u32>> = OnceLock::new();

pub(super) fn get_pty_process_map() -> &'static Mutex<HashMap<u32, ManagedPtyProcess>> {
    PTY_PROCESS_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

pub(super) fn record_terminated_pty_status(pid: u32, exit_code: i32) {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .insert(pid, exit_code);
}

pub(super) fn take_terminated_pty_status(pid: u32) -> Option<i32> {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .remove(&pid)
}

pub(super) fn discard_terminated_pty_status(pid: u32) {
    let _ = take_terminated_pty_status(pid);
}

pub(super) fn clear_terminated_pty_statuses() {
    TERMINATED_PTY_STATUSES
        .get_or_init(|| StdMutex::new(HashMap::new()))
        .lock()
        .expect("terminated PTY status lock")
        .clear();
}

pub(super) async fn get_next_pty_pid() -> u32 {
    let counter = PTY_PID_COUNTER.get_or_init(|| Mutex::new(10000));
    let mut pid = counter.lock().await;
    let current = *pid;
    *pid += 1;
    current
}

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
}

pub(super) struct ManagedPtyProcess {
    pub(super) async_fd: AsyncFd<OwnedFd>,
    pub(super) lifecycle: Arc<Mutex<()>>,
    pub(super) io: Arc<PtyIoState>,
    pub(super) child_pid: Pid,
    pub(super) cmd: String,
    pub(super) exit_status: Option<i32>,
    // Retain an observed terminal status for a read that captured the PTY
    // before explicit SIGKILL removes its registry entry.
    pub(super) shared_exit_status: Arc<StdMutex<Option<i32>>>,
    pub(super) output_eof: bool,
    pub(super) push_subscription: Option<PushSubscription>,
    pub(super) subscription_requested: bool,
    pub(super) terminating: bool,
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
    pub(super) cwd: Option<String>,
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

    fn disarm(mut self) -> (Pid, String) {
        let child = self.child.take().expect("PTY child");
        let child_pid = Pid::from_raw(child.id() as i32);
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
            let _ = rustix::process::kill_process_group(pgid, RustixSignal::KILL);
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
        .local_flags
        .remove(LocalFlags::ECHO | LocalFlags::ECHONL);
    termios.output_flags.remove(OutputFlags::ONLCR);
    tcsetattr(&slave, SetArg::TCSANOW, &termios)
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
        cmd.current_dir(expand_tilde(cwd));
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
    // async-signal-safe syscalls (close, setsid, ioctl).  The raw descriptors
    // are still open in the child because CLOEXEC only applies at exec, and
    // the child owns them exclusively, so closing them here is sound.
    unsafe {
        cmd.pre_exec(move || {
            rustix::io::close(master_fd);
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
        .map_err(|e| RpcError::process_error(format!("Task join error: {e}")))??;

    set_fd_nonblocking(startup.master_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to set non-blocking: {e}")))?;

    let async_fd = AsyncFd::new(startup.take_master_fd())
        .map_err(|e| RpcError::process_error(format!("Failed to create AsyncFd: {e}")))?;

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
        let fd = dup_cloexec(managed.async_fd.get_ref())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (fd, managed.child_pid, managed.io.clone())
    };
    let owned_fd = fd;
    if io.is_closed() {
        return Err(RpcError::process_error(format!(
            "PTY process is closed: {}",
            params.pid
        )));
    }

    set_window_size(&owned_fd, params.rows, params.cols)
        .map_err(|e| RpcError::process_error(format!("Failed to resize PTY: {e}")))?;

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

pub(super) struct PtyReadResult {
    pub(super) output: Vec<u8>,
    pub(super) pending: bool,
    pub(super) exited: bool,
    pub(super) exit_code: Option<i32>,
}

pub(super) async fn read_pty_now(pid: u32, max_bytes: usize) -> Result<PtyReadResult, RpcError> {
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
        let fd = dup_cloexec(managed.async_fd.get_ref())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (
            managed.lifecycle.clone(),
            managed.io.clone(),
            managed.shared_exit_status.clone(),
            fd,
        )
    };
    let owned_fd = fd;
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
    let (pending, eof) = match rustix::io::read(&owned_fd, output.as_mut_slice()) {
        Ok(n) if n > 0 => {
            output.truncate(n);
            (false, false)
        }
        Ok(_) => {
            output.clear();
            (false, true)
        }
        Err(rustix::io::Errno::AGAIN) => {
            output.clear();
            (true, false)
        }
        // Linux reports PTY master EOF as EIO after the slave closes.
        Err(rustix::io::Errno::IO) => {
            output.clear();
            (false, true)
        }
        Err(errno) => {
            return Err(RpcError::process_error(format!(
                "Failed to read PTY: {}",
                std::io::Error::from(errno)
            )));
        }
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

pub(super) fn check_exit_status(managed: &mut ManagedPtyProcess) -> (bool, Option<i32>) {
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

pub(super) async fn wait_for_pty_readable(pid: u32) -> bool {
    // Wait on a duplicate so close/kill can remove the registry entry and
    // close the real master without racing an in-flight readiness wait.
    let fd = {
        let processes = get_pty_process_map().lock().await;
        let Some(managed) = processes.get(&pid) else {
            return false;
        };
        match dup_cloexec(managed.async_fd.get_ref()) {
            Ok(fd) => fd,
            Err(_) => return false,
        }
    };
    let async_fd = match AsyncFd::new(fd) {
        Ok(fd) => fd,
        Err(_) => return false,
    };
    async_fd.readable().await.is_ok()
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

    // Capture the descriptor and cancellation state while briefly holding the
    // registry lock.  All waits and writes happen on owned state afterwards.
    let (fd, io) = {
        let processes = get_pty_process_map().lock().await;
        let managed = processes.get(&params.pid).ok_or_else(|| {
            RpcError::process_error(format!("PTY process not found: {}", params.pid))
        })?;
        let fd = dup_cloexec(managed.async_fd.get_ref())
            .map_err(|e| RpcError::process_error(format!("Failed to duplicate PTY: {e}")))?;
        (fd, managed.io.clone())
    };
    let async_fd = AsyncFd::new(fd)
        .map_err(|e| RpcError::process_error(format!("Failed to monitor PTY: {e}")))?;
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

pub(super) async fn wait_pty_pid(child_pid: Pid) -> Result<Option<i32>, nix::errno::Errno> {
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

pub(super) async fn terminate_pty_process(
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
        signal: SignalCode,
    }

    fn default_pty_signal() -> SignalCode {
        SignalCode::Number(libc::SIGTERM)
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;
    let signal = params.signal.resolve()?;
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
    // Match local signal-process semantics: forward the requested signal
    // without turning a survivable signal such as SIGINT into SIGKILL.
    // Explicit close and connection cleanup retain escalation authority.
    // Explicit SIGKILL also opts out of output draining.
    // If SIGKILL fails after cancellation, the terminal entry deliberately
    // remains marked as terminating; PtyIoState cancellation is irreversible.
    terminate_pty_process(
        params.pid,
        signal,
        false,
        signal == libc::SIGKILL,
        signal == libc::SIGKILL,
    )
    .await?;
    if signal == libc::SIGKILL && subscribed {
        let exit_code = shared_exit_status
            .as_ref()
            .and_then(|status| *status.lock().expect("shared PTY exit status lock"))
            .map(i64::from)
            .unwrap_or_else(|| i64::from(128 + signal));
        let _ = send_process_notification(
            "process.pty_exit",
            msgpack_map! { "pid" => params.pid, "exit_code" => exit_code },
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
    // A failure leaves the entry terminal because cancellation is irreversible.
    terminate_pty_process(params.pid, libc::SIGKILL, true, true, false).await?;
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
