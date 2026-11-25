// SPDX-License-Identifier: GPL-3.0-or-later

//! Process execution operations
//!
//! `pipe` holds the pipe-backed handlers (`process.run`, `process.start`, ...),
//! `pty` the pseudo-terminal handlers.  This module owns the unified process
//! registry and helpers shared by both: process-group signalling, signal name
//! parsing, and the connection-teardown cleanup pass.

mod pipe;
mod pty;
mod push;
#[cfg(test)]
mod tests;

pub(crate) use pipe::{ChildError, ChildSpec, run_child, start_outcome};
pub use pipe::{close_stdin, kill, run, signal_pid, start, write};
pub(crate) use pty::start_pty_outcome;
pub use pty::{close_pty, kill_pty, resize_pty, start_pty, write_pty};
pub use push::{init_notification_writer, start_output_push};

use crate::protocol::RpcError;
use nix::sys::signal::Signal;
use rustix::fs::{OFlags, fcntl_getfl, fcntl_setfl};
use rustix::io::{FdFlags, fcntl_dupfd_cloexec, fcntl_getfd, fcntl_setfd};
use rustix::process::{Pid as RustixPid, Signal as RustixSignal};
#[cfg(any(target_os = "linux", target_os = "android"))]
use rustix_libc_wrappers::process::SignalExt;
use serde::Deserialize;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsFd, OwnedFd};
use std::process::ExitStatus;
#[cfg(test)]
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, LazyLock, Mutex as StdMutex};
use tokio::io::unix::AsyncFd;
use tokio::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command};
use tokio::sync::Mutex;

use pipe::terminate_pipe_process;
use pty::{PtyIoState, terminate_pty_process};
use push::{OutputPush, stop_output_push};

/// Upper bound for one test-only `read`/`read_pty` request.
#[cfg(test)]
const MAX_PROCESS_READ_BYTES: usize = 1024 * 1024;

// ============================================================================
// Unified Process Registry
// ============================================================================

struct PipeBackend {
    child: Child,
    stdin: Arc<Mutex<Option<ChildStdin>>>,
    stdout: Arc<Mutex<Option<ChildStdout>>>,
    stderr: Arc<Mutex<Option<ChildStderr>>>,
}

struct PtyBackend {
    async_fd: Arc<AsyncFd<OwnedFd>>,
    io: Arc<PtyIoState>,
}

enum ProcessBackend {
    Pipe(PipeBackend),
    Pty(PtyBackend),
}

struct ManagedProcess {
    child_pid: u32,
    lifecycle: Arc<Mutex<()>>,
    exit_status: Option<ExitStatus>,
    // Retain an observed terminal status for a read that captured streams/fd
    // before explicit SIGKILL removed its registry entry.
    shared_exit_status: Arc<StdMutex<Option<ExitStatus>>>,
    output_push: Option<OutputPush>,
    terminating: bool,
    backend: ProcessBackend,
}

impl ManagedProcess {
    pub(super) fn as_pipe(&self) -> Option<&PipeBackend> {
        match &self.backend {
            ProcessBackend::Pipe(pipe) => Some(pipe),
            ProcessBackend::Pty(_) => None,
        }
    }

    pub(super) fn as_pipe_mut(&mut self) -> Option<&mut PipeBackend> {
        match &mut self.backend {
            ProcessBackend::Pipe(pipe) => Some(pipe),
            ProcessBackend::Pty(_) => None,
        }
    }

    pub(super) fn as_pty(&self) -> Option<&PtyBackend> {
        match &self.backend {
            ProcessBackend::Pty(pty) => Some(pty),
            ProcessBackend::Pipe(_) => None,
        }
    }

    #[cfg(test)]
    pub(super) fn as_pty_mut(&mut self) -> Option<&mut PtyBackend> {
        match &mut self.backend {
            ProcessBackend::Pty(pty) => Some(pty),
            ProcessBackend::Pipe(_) => None,
        }
    }

    pub(super) fn is_pipe(&self) -> bool {
        matches!(self.backend, ProcessBackend::Pipe(_))
    }

    pub(super) fn is_pty(&self) -> bool {
        matches!(self.backend, ProcessBackend::Pty(_))
    }
}

/// Process-wide registry of managed async pipe and PTY processes.
///
/// The map lock is a `std::sync::Mutex` because map operations are strictly
/// non-blocking O(1) lookups/insertions/removals and are never held across
/// `.await` points.
struct ProcessRegistry {
    next_pid: AtomicU32,
    processes: StdMutex<HashMap<u32, ManagedProcess>>,
}

impl ProcessRegistry {
    fn new() -> Self {
        Self {
            next_pid: AtomicU32::new(1),
            processes: StdMutex::new(HashMap::new()),
        }
    }

    fn next_pid(&self) -> u32 {
        self.next_pid.fetch_add(1, Ordering::Relaxed)
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<u32, ManagedProcess>> {
        self.processes
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

static PROCESS_REGISTRY: LazyLock<ProcessRegistry> = LazyLock::new(ProcessRegistry::new);

fn registry() -> &'static ProcessRegistry {
    &PROCESS_REGISTRY
}

/// A child that exits or closes stdin before consuming all input is normal
/// shell behavior (`head`, `grep -q', a failing filter).  `tramp-sh' runs
/// `command <infile', where the resulting SIGPIPE/EPIPE is invisible to
/// Emacs, so these must not turn into RPC errors.
fn is_benign_stdin_error(error: &std::io::Error) -> bool {
    matches!(
        error.kind(),
        ErrorKind::BrokenPipe | ErrorKind::WriteZero | ErrorKind::ConnectionReset
    )
}

/// Own a newly spawned child process group until its request finishes.
///
/// Request tasks are aborted when their RPC transport disappears.  Tokio can
/// kill the direct child on drop, but descendants would otherwise survive, so
/// this guard synchronously kills the whole group when the request future is
/// cancelled.  It is disarmed immediately after the direct child is reaped.
struct ProcessGroupGuard {
    pgid: Option<u32>,
}

impl ProcessGroupGuard {
    fn new(pgid: u32) -> Self {
        Self { pgid: Some(pgid) }
    }

    fn disarm(&mut self) {
        self.pgid = None;
    }
}

impl Drop for ProcessGroupGuard {
    fn drop(&mut self) {
        if let Some(pgid) = self.pgid {
            // Best effort only: Drop cannot report an error and ESRCH means the
            // group already exited.
            if let Some(pgid) = i32::try_from(pgid).ok().and_then(RustixPid::from_raw) {
                let _ = rustix::process::kill_process_group(pgid, RustixSignal::KILL);
            }
        }
    }
}

fn configure_process_group(cmd: &mut Command) {
    // Keep descendants in a group owned by this request, separate from the
    // server and unrelated processes.
    // SAFETY: the pre-exec hook only performs the async-signal-safe setpgid
    // syscall.
    unsafe {
        cmd.pre_exec(|| rustix::process::setpgid(None, None).map_err(std::io::Error::from));
    }
}

#[cfg(test)]
pub(crate) async fn wait_for_process_exit(pid: i32) {
    for _ in 0..100 {
        // Reap direct children ourselves when Tokio's best-effort drop reaper
        // has not done so yet.
        if let Some(rpid) = RustixPid::from_raw(pid) {
            let _ = rustix::process::waitpid(Some(rpid), rustix::process::WaitOptions::NOHANG);
        }
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

    match RustixPid::from_raw(pid).map(rustix::process::test_kill_process) {
        Some(Ok(())) => true,
        Some(Err(errno)) => errno == rustix::io::Errno::PERM,
        None => false,
    }
}

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
    registry().lock().is_empty()
}

#[cfg(test)]
pub(crate) async fn test_managed_os_pids() -> Vec<i32> {
    registry()
        .lock()
        .values()
        .map(|managed| managed.child_pid as i32)
        .collect()
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

    let pgid = i32::try_from(pid)
        .ok()
        .and_then(RustixPid::from_raw)
        .ok_or_else(|| std::io::Error::new(ErrorKind::InvalidInput, "PID is out of range"))?;
    if signal == 0 {
        rustix::process::test_kill_process_group(pgid).map_err(std::io::Error::from)
    } else {
        let signal = rustix_signal(signal).ok_or_else(|| {
            std::io::Error::new(ErrorKind::InvalidInput, "Signal is out of range")
        })?;
        rustix::process::kill_process_group(pgid, signal).map_err(std::io::Error::from)
    }
}

fn rustix_signal(signal: i32) -> Option<RustixSignal> {
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        RustixSignal::from_raw(signal)
    }
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    {
        RustixSignal::from_named_raw(signal)
    }
}

fn signal_process(pid: u32, signal: i32) -> std::io::Result<()> {
    let pid = i32::try_from(pid)
        .ok()
        .and_then(RustixPid::from_raw)
        .ok_or_else(|| std::io::Error::new(ErrorKind::InvalidInput, "PID is out of range"))?;
    if signal == 0 {
        rustix::process::test_kill_process(pid).map_err(std::io::Error::from)
    } else {
        let signal = rustix_signal(signal).ok_or_else(|| {
            std::io::Error::new(ErrorKind::InvalidInput, "Signal is out of range")
        })?;
        rustix::process::kill_process(pid, signal).map_err(std::io::Error::from)
    }
}

#[derive(Deserialize)]
#[serde(untagged)]
enum SignalCode {
    Number(i32),
    Name(String),
}

impl SignalCode {
    fn resolve(self) -> Result<i32, RpcError> {
        match self {
            Self::Number(signal) => {
                validate_signal(signal)?;
                Ok(signal)
            }
            Self::Name(name) => {
                let name = name.to_ascii_uppercase();
                let name = if name.starts_with("SIG") {
                    name
                } else {
                    format!("SIG{name}")
                };
                let canonical_name = match name.as_str() {
                    "SIGCLD" => "SIGCHLD",
                    "SIGIOT" => "SIGABRT",
                    "SIGPOLL" => "SIGIO",
                    "SIGUNUSED" => "SIGSYS",
                    _ => name.as_str(),
                };
                if canonical_name.starts_with("SIGRTMIN") || canonical_name.starts_with("SIGRTMAX")
                {
                    parse_realtime_signal(canonical_name)
                        .ok_or_else(|| RpcError::invalid_params(format!("Invalid signal: {name}")))
                } else {
                    canonical_name
                        .parse::<Signal>()
                        .map(|signal| signal as i32)
                        .map_err(|_| RpcError::invalid_params(format!("Invalid signal: {name}")))
                }
            }
        }
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
fn realtime_signal_bounds() -> Option<(i32, i32)> {
    Some((libc::SIGRTMIN(), libc::SIGRTMAX()))
}

#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn realtime_signal_bounds() -> Option<(i32, i32)> {
    None
}

fn parse_realtime_signal(name: &str) -> Option<i32> {
    let (minimum, maximum) = realtime_signal_bounds()?;
    let (base, suffix, minimum_based) = if let Some(suffix) = name.strip_prefix("SIGRTMIN") {
        (minimum, suffix, true)
    } else {
        (maximum, name.strip_prefix("SIGRTMAX")?, false)
    };
    let offset = if suffix.is_empty() {
        0
    } else {
        let expected_operator = if minimum_based { '+' } else { '-' };
        suffix
            .starts_with(expected_operator)
            .then(|| suffix.parse::<i32>().ok())??
    };
    let signal = base.checked_add(offset)?;
    (minimum..=maximum).contains(&signal).then_some(signal)
}

fn validate_signal(signal: i32) -> Result<(), RpcError> {
    let realtime = realtime_signal_bounds()
        .is_some_and(|(minimum, maximum)| (minimum..=maximum).contains(&signal));
    if signal == 0 || Signal::try_from(signal).is_ok() || realtime {
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

const MANAGED_CHILD_WAIT: std::time::Duration = std::time::Duration::from_millis(500);
// PTY signal handlers may need a scheduler turn to flush their final output
// before exit; retain that output before escalating to SIGKILL.
const MANAGED_PTY_CHILD_WAIT: std::time::Duration = std::time::Duration::from_secs(2);

fn process_group_exists(pgid: u32) -> bool {
    match i32::try_from(pgid)
        .ok()
        .and_then(RustixPid::from_raw)
        .map(rustix::process::test_kill_process_group)
    {
        Some(Ok(())) => true,
        Some(Err(errno)) => errno == rustix::io::Errno::PERM,
        None => false,
    }
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

fn set_fd_nonblocking<Fd: AsFd>(fd: Fd) -> Result<(), std::io::Error> {
    let flags = fcntl_getfl(&fd)?;
    fcntl_setfl(&fd, flags | OFlags::NONBLOCK)?;
    Ok(())
}

fn set_fd_cloexec<Fd: AsFd>(fd: Fd) -> Result<(), std::io::Error> {
    let flags = fcntl_getfd(&fd)?;
    fcntl_setfd(&fd, flags | FdFlags::CLOEXEC)?;
    Ok(())
}

fn dup_cloexec<Fd: AsFd>(fd: Fd) -> Result<OwnedFd, std::io::Error> {
    Ok(fcntl_dupfd_cloexec(fd, 0)?)
}

/// Stop and reap managed children after the transport is gone.
pub async fn cleanup_managed_processes() -> Result<(), RpcError> {
    // The notification transport is already gone.  Stop output consumers before
    // terminating children so no detached task retains stream or descriptor state.
    let pushes: Vec<OutputPush> = {
        let mut processes = registry().lock();
        processes
            .values_mut()
            .filter_map(|managed| {
                managed.terminating = true;
                managed.output_push.take()
            })
            .collect()
    };
    futures::future::join_all(pushes.into_iter().map(stop_output_push)).await;

    let (pipe_pids, pty_pids): (Vec<u32>, Vec<u32>) = {
        let processes = registry().lock();
        let mut pipes = Vec::new();
        let mut ptys = Vec::new();
        for (&pid, managed) in processes.iter() {
            if managed.is_pipe() {
                pipes.push(pid);
            } else {
                ptys.push(pid);
            }
        }
        (pipes, ptys)
    };

    let pipe_reaps = pipe_pids
        .into_iter()
        .map(|pid| async move { (pid, terminate_pipe_process(pid, libc::SIGTERM, true).await) });
    let pty_reaps = pty_pids.into_iter().map(|pid| async move {
        (
            pid,
            terminate_pty_process(pid, libc::SIGTERM, true, true).await,
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
        let mut processes = registry().lock();
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
    if failures.is_empty() {
        Ok(())
    } else {
        Err(RpcError::process_error(format!(
            "Managed process cleanup failed: {}",
            failures.join("; ")
        )))
    }
}
