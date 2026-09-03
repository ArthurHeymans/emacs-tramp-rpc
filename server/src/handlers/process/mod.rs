// SPDX-License-Identifier: GPL-3.0-or-later

//! Process execution operations
//!
//! `pipe` holds the pipe-backed handlers (`process.run`, `process.start`, ...),
//! `pty` the pseudo-terminal handlers.  This module owns the state and helpers
//! shared by both: process-group signalling, signal name parsing, and the
//! connection-teardown cleanup pass.

mod pipe;
mod pty;
#[cfg(test)]
mod tests;

pub(crate) use pipe::{ChildError, ChildSpec, run_child};
pub use pipe::{close_stdin, kill, list, read, run, signal_pid, start, status, write};
pub use pty::{close_pty, kill_pty, list_pty, read_pty, resize_pty, start_pty, write_pty};

use crate::protocol::RpcError;
use nix::sys::signal::Signal;
#[cfg(test)]
use nix::sys::wait::{WaitPidFlag, waitpid};
#[cfg(test)]
use nix::unistd::Pid;
use rustix::fs::{OFlags, fcntl_getfl, fcntl_setfl};
use rustix::io::{FdFlags, fcntl_dupfd_cloexec, fcntl_getfd, fcntl_setfd};
use rustix::process::{Pid as RustixPid, Signal as RustixSignal};
#[cfg(any(target_os = "linux", target_os = "android"))]
use rustix_libc_wrappers::process::SignalExt;
use serde::Deserialize;
#[cfg(test)]
use std::collections::HashMap;
use std::io::ErrorKind;
use std::os::fd::{AsFd, OwnedFd};
#[cfg(test)]
use std::sync::{Mutex as StdMutex, OnceLock};
use tokio::process::Command;
#[cfg(test)]
use tokio::sync::Mutex;

use pipe::{get_process_map, terminate_pipe_process};
#[cfg(test)]
use pty::TERMINATED_PTY_STATUSES;
use pty::{clear_terminated_pty_statuses, get_pty_process_map, terminate_pty_process};

const MAX_PROCESS_READ_BYTES: usize = 1024 * 1024;

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
