// SPDX-License-Identifier: GPL-3.0-or-later

//! Command execution and ancestor scanning for TRAMP-RPC
//!
//! This module provides:
//! - `commands.run_parallel`: Run multiple Tokio-managed child processes
//! - `ancestors.scan`: Scan ancestor directories for marker files

use crate::msgpack_map;
use crate::protocol::{IntoValue, RpcError, exit_code_from_status, from_value};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::{Arc, LazyLock};
use std::time::UNIX_EPOCH;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
use tokio::sync::Semaphore;

use super::HandlerResult;

/// Maximum number of commands that can be run in a single request.
/// Prevents resource exhaustion from excessively large batches.
const MAX_PARALLEL_COMMANDS: usize = 256;

/// Default per-command timeout for `commands.run_parallel`.  Without it one
/// hung `git` stalls the whole batch until the Lisp 30s call timeout fires,
/// discarding completed results and tearing down the connection.
const DEFAULT_PARALLEL_COMMAND_TIMEOUT_MS: u64 = 25_000;
/// Hard cap so a client cannot request an unbounded hang.
const MAX_PARALLEL_COMMAND_TIMEOUT_MS: u64 = 120_000;

/// Global child budget for `commands.run_parallel` requests.  Request-level
/// admission alone is insufficient because each admitted request can fan out
/// into hundreds of operating-system processes.
const MAX_CONCURRENT_PARALLEL_CHILDREN: usize = 64;
static PARALLEL_CHILD_ADMISSIONS: LazyLock<Semaphore> =
    LazyLock::new(|| Semaphore::new(MAX_CONCURRENT_PARALLEL_CHILDREN));

/// Run multiple commands concurrently.
///
/// Each command is driven by Tokio, allowing transport cancellation to stop
/// its whole process group.  Returns a map
/// of key -> {exit_code, stdout, stderr} for each command.
///
/// This replaces the old `magit.status` handler: instead of hardcoding
/// ~30 git commands on the server, the client sends exactly the commands
/// it needs and gets raw results back.
///
/// # Security
///
/// This handler executes arbitrary commands as requested by the client.
/// This is acceptable because the server is only reachable via SSH stdin/stdout,
/// so the caller already has full shell access to the remote host.  The RPC
/// channel does not grant any capabilities beyond what SSH already provides.
/// If the transport model ever changes (e.g., TCP socket), this handler
/// would need a command whitelist.
pub async fn run_parallel(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct CommandEntry {
        /// Lookup key (client-defined, returned as-is in results)
        key: String,
        /// Command to run
        cmd: String,
        /// Arguments (default: empty)
        #[serde(default)]
        args: Vec<String>,
        /// Working directory (optional)
        cwd: Option<String>,
        /// Stdin input as binary
        #[serde(default, with = "serde_bytes")]
        stdin: Option<Vec<u8>>,
    }

    #[derive(Deserialize)]
    struct Params {
        commands: Vec<CommandEntry>,
        /// Environment variables applied to every command in the batch.
        #[serde(default)]
        env: Option<HashMap<String, String>>,
        /// Clear the inherited server environment before applying `env`.
        #[serde(default)]
        clear_env: bool,
        /// Per-command timeout in milliseconds.  Each hung command fails
        /// individually instead of stalling the whole batch until the Lisp
        /// call timeout tears down the connection.  Defaults to
        /// `DEFAULT_PARALLEL_COMMAND_TIMEOUT_MS`, capped at
        /// `MAX_PARALLEL_COMMAND_TIMEOUT_MS`.
        #[serde(default = "default_parallel_timeout_ms")]
        timeout_ms: u64,
    }

    fn default_parallel_timeout_ms() -> u64 {
        DEFAULT_PARALLEL_COMMAND_TIMEOUT_MS
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    if params.commands.is_empty() {
        return Ok(Value::Map(vec![]));
    }

    // Enforce command count limit to prevent resource exhaustion
    if params.commands.len() > MAX_PARALLEL_COMMANDS {
        return Err(RpcError::invalid_params(format!(
            "Too many commands: {} (max {})",
            params.commands.len(),
            MAX_PARALLEL_COMMANDS
        )));
    }

    // This budget is shared by every command in the batch, so one chatty
    // command cannot push the combined response over the frame limit.
    let remaining = Arc::new(Semaphore::new(crate::MAX_RESPONSE_OUTPUT_BYTES));
    let env = params.env.map(Arc::new);
    let clear_env = params.clear_env;
    let timeout_ms = params.timeout_ms.clamp(1, MAX_PARALLEL_COMMAND_TIMEOUT_MS);
    let timeout = std::time::Duration::from_millis(timeout_ms);
    let results = futures::future::join_all(params.commands.into_iter().map(|entry| {
        let remaining = Arc::clone(&remaining);
        let env = env.clone();
        async move {
            // The per-command deadline covers queueing for the global child
            // permit as well as execution.  All batch entries start their
            // clocks together, so N hung waves cannot stack into N x timeout
            // past the Lisp call timeout; the whole batch fails fast.
            let deadline = tokio::time::Instant::now() + timeout;
            let timeout_error = |key: String| {
                (
                    key,
                    msgpack_map! {
                        "exit_code" => -1i32,
                        "stdout" => Value::Binary(vec![]),
                        "stderr" => Value::Binary(
                            format!("Command timed out after {timeout_ms}ms").into_bytes()
                        ),
                        "timed_out" => Value::Boolean(true)
                    },
                )
            };
            // Valid batch entries queue behind earlier entries.  Dropping the
            // request future on transport cancellation cancels this wait.
            let _child_permit = match tokio::time::timeout_at(
                deadline,
                PARALLEL_CHILD_ADMISSIONS.acquire(),
            )
            .await
            {
                Ok(permit) => permit.expect("parallel child admission semaphore is never closed"),
                Err(_) => return timeout_error(entry.key),
            };
            let mut cmd = Command::new(&entry.cmd);
            cmd.args(&entry.args);
            if let Some(ref cwd) = entry.cwd {
                cmd.current_dir(super::expand_tilde(cwd));
            }
            if clear_env {
                cmd.env_clear();
            }
            if let Some(env) = env {
                cmd.envs(env.iter());
            }
            cmd.stdin(if entry.stdin.is_some() {
                Stdio::piped()
            } else {
                Stdio::null()
            });
            cmd.stdout(Stdio::piped());
            cmd.stderr(Stdio::piped());
            cmd.kill_on_drop(true);
            super::process::configure_process_group(&mut cmd);

            let value = match cmd.spawn() {
                Ok(mut child) => {
                    let Some(child_pid) = child.id() else {
                        return (
                            entry.key,
                            msgpack_map! {
                                "exit_code" => -1i32,
                                "stdout" => Value::Binary(vec![]),
                                "stderr" => Value::Binary(b"Spawned process has no PID".to_vec())
                            },
                        );
                    };
                    let mut process_group = super::process::ProcessGroupGuard::new(child_pid);
                    let stdin_data = entry.stdin;
                    let mut stdin = child.stdin.take();
                    let stdout = child.stdout.take().expect("piped stdout");
                    let stderr = child.stderr.take().expect("piped stderr");
                    let write_stdin = async move {
                        if let Some(data) = stdin_data
                            && let Some(mut stdin) = stdin.take()
                            && let Err(error) = stdin.write_all(&data).await
                            // A child that stops reading early is not an error.
                            && !super::process::is_benign_stdin_error(&error)
                        {
                            return Err(std::io::Error::other(format!(
                                "Failed to write stdin: {error}"
                            )));
                        }
                        Ok(())
                    };
                    // Isolate hung commands: each command gets its own deadline
                    // so one stuck `git` fails individually while completed
                    // results are preserved, instead of stalling `join_all`
                    // until the Lisp call timeout tears down the connection.
                    // The deadline was set before queueing, so queue wait
                    // consumes the same budget.
                    let run = async {
                        tokio::try_join!(
                            write_stdin,
                            super::process::read_sync_output(
                                stdout,
                                Arc::clone(&remaining),
                                crate::MAX_RESPONSE_OUTPUT_BYTES,
                            ),
                            super::process::read_sync_output(
                                stderr,
                                remaining,
                                crate::MAX_RESPONSE_OUTPUT_BYTES,
                            ),
                            child.wait()
                        )
                    };
                    let result = match tokio::time::timeout_at(deadline, run).await {
                        Ok(result) => result,
                        Err(_) => {
                            let _ = child.kill().await;
                            let _ = child.wait().await;
                            Err(std::io::Error::other(format!(
                                "Command timed out after {timeout_ms}ms"
                            )))
                        }
                    };
                    match result {
                        Ok(((), stdout, stderr, status)) => {
                            process_group.disarm();
                            msgpack_map! {
                                "exit_code" => exit_code_from_status(status),
                                "stdout" => Value::Binary(stdout),
                                "stderr" => Value::Binary(stderr)
                            }
                        }
                        Err(error) => {
                            let _ = child.kill().await;
                            let _ = child.wait().await;
                            let message = error.to_string();
                            let timed_out = message.starts_with("Command timed out");
                            msgpack_map! {
                                "exit_code" => -1i32,
                                "stdout" => Value::Binary(vec![]),
                                "stderr" => Value::Binary(message.into_bytes()),
                                "timed_out" => Value::Boolean(timed_out)
                            }
                        }
                    }
                }
                Err(error) => msgpack_map! {
                    "exit_code" => -1i32,
                    "stdout" => Value::Binary(vec![]),
                    "stderr" => Value::Binary(error.to_string().into_bytes())
                },
            };
            (entry.key, value)
        }
    }))
    .await;

    Ok(Value::Map(
        results
            .into_iter()
            .map(|(key, value)| (Value::String(key.into()), value))
            .collect(),
    ))
}

fn ancestor_path_value(bytes: Vec<u8>) -> Value {
    match String::from_utf8(bytes) {
        Ok(path) => Value::String(path.into()),
        Err(error) => Value::Binary(error.into_bytes()),
    }
}

/// Scan ancestor directories for marker files
///
/// This is useful for project detection, VCS detection, etc.
/// Returns a map of marker -> directory where it was found (or null if not found)
pub async fn ancestors_scan(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Starting directory
        #[serde(with = "crate::protocol::path_or_bytes")]
        directory: Vec<u8>,
        /// Marker files/directories to look for
        markers: Vec<String>,
        /// Maximum depth to search (default: 10)
        #[serde(default = "default_max_depth")]
        max_depth: usize,
    }

    fn default_max_depth() -> usize {
        10
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Wrap in spawn_blocking since this does blocking filesystem I/O
    let expanded_directory = super::file::bytes_to_path(&params.directory).await?;
    tokio::task::spawn_blocking(move || {
        let dir = expanded_directory.as_path();
        if !dir.exists() {
            return Err(RpcError::file_not_found(
                &expanded_directory.to_string_lossy(),
            ));
        }

        // Initialize results with None for each marker
        let mut results: HashMap<String, Option<Vec<u8>>> =
            params.markers.iter().map(|m| (m.clone(), None)).collect();

        // Walk up the directory tree
        let mut current = dir.to_path_buf();
        let mut depth = 0;

        while depth < params.max_depth {
            // Check each marker that hasn't been found yet
            for marker in &params.markers {
                if results.get(marker).is_some_and(Option::is_none) {
                    let marker_path = current.join(marker);
                    if marker_path.exists() {
                        results.insert(
                            marker.clone(),
                            Some(current.as_os_str().as_bytes().to_vec()),
                        );
                    }
                }
            }

            // Check if all markers found
            if results.values().all(|v| v.is_some()) {
                break;
            }

            // Move to parent
            match current.parent() {
                Some(parent) if parent != current => {
                    current = parent.to_path_buf();
                    depth += 1;
                }
                _ => break, // Reached root
            }
        }

        // Convert to Value
        let pairs: Vec<(Value, Value)> = results
            .into_iter()
            .map(|(key, path)| {
                let path = path.map_or(Value::Nil, ancestor_path_value);
                (key.into_value(), path)
            })
            .collect();

        Ok(Value::Map(pairs))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

fn canonical_or_original(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

fn find_existing_start(path: &Path) -> Option<&Path> {
    if path.exists() {
        return Some(path);
    }

    let mut current = path;
    while !current.exists() {
        current = current.parent()?;
    }
    Some(current)
}

fn as_search_dir(path: &Path) -> Option<PathBuf> {
    if path.is_dir() {
        Some(path.to_path_buf())
    } else {
        path.parent().map(|p| p.to_path_buf())
    }
}

const MAX_DOMINATING_DEPTH: usize = 100;

fn find_dominating_dir(
    start_dir: &Path,
    names: &[String],
) -> Result<Option<(PathBuf, Vec<String>)>, RpcError> {
    let mut current = start_dir.to_path_buf();
    let mut depth = 0usize;
    loop {
        let found: Vec<String> = names
            .iter()
            .filter(|name| current.join(name.as_str()).exists())
            .cloned()
            .collect();

        if !found.is_empty() {
            return Ok(Some((current, found)));
        }

        match current.parent() {
            Some(parent) if parent != current => {
                if depth >= MAX_DOMINATING_DEPTH {
                    return Err(RpcError::invalid_params(format!(
                        "Maximum ancestor traversal depth ({}) exceeded",
                        MAX_DOMINATING_DEPTH
                    )));
                }
                current = parent.to_path_buf();
                depth += 1;
            }
            _ => return Ok(None),
        }
    }
}

fn remap_to_lexical_ancestor(start_dir: &Path, found_dir: &Path) -> PathBuf {
    let canonical_found = canonical_or_original(found_dir);
    let mut current = start_dir.to_path_buf();
    loop {
        if canonical_or_original(&current) == canonical_found {
            return current;
        }
        match current.parent() {
            Some(parent) if parent != current => current = parent.to_path_buf(),
            _ => return found_dir.to_path_buf(),
        }
    }
}

fn mtime_seconds(path: &Path) -> Option<i64> {
    let modified = std::fs::metadata(path).ok()?.modified().ok()?;
    let duration = modified.duration_since(UNIX_EPOCH).ok()?;
    Some(duration.as_secs() as i64)
}

/// Return readable regular files from a directory for a list of names.
pub async fn highlevel_test_files_in_dir(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        directory: String,
        names: Vec<String>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    tokio::task::spawn_blocking(move || {
        let dir = canonical_or_original(Path::new(&super::expand_tilde(&params.directory)));
        if !dir.is_dir() {
            return Ok(Value::Array(vec![]));
        }

        let mut found = Vec::new();
        for name in &params.names {
            let candidate = dir.join(name);
            if candidate.is_file() && File::open(&candidate).is_ok() {
                found.push(candidate.to_string_lossy().to_string().into_value());
            }
        }
        Ok(Value::Array(found))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

/// Locate marker files in ancestor directories.
///
/// Returns marker paths from the first ancestor that contains any markers.
pub async fn highlevel_locate_dominating_file_multi(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        file: String,
        names: Vec<String>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    tokio::task::spawn_blocking(move || {
        let path = PathBuf::from(super::expand_tilde(&params.file));
        // Preserve lexical path shape instead of canonicalizing symlinks.
        // TRAMP clients rely on this to compute repo-relative paths correctly.
        let Some(existing_start) = find_existing_start(&path) else {
            return Ok(Value::Array(vec![]));
        };
        let Some(start_dir) = as_search_dir(existing_start) else {
            return Ok(Value::Array(vec![]));
        };

        let Some((dir, found_names)) = find_dominating_dir(&start_dir, &params.names)? else {
            return Ok(Value::Array(vec![]));
        };

        let marker_paths: Vec<Value> = found_names
            .into_iter()
            .map(|name| dir.join(name).to_string_lossy().to_string().into_value())
            .collect();
        Ok(Value::Array(marker_paths))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

/// Prepare dir-locals data in one RPC call.
pub async fn highlevel_dir_locals_find_file_cache_update(params: Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        file: String,
        names: Vec<String>,
        #[serde(default)]
        cache_dirs: Vec<String>,
    }

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    tokio::task::spawn_blocking(move || {
        let file_path = PathBuf::from(super::expand_tilde(&params.file));
        // Keep lexical (non-canonical) path shape to match locate-dominating behavior.
        let lexical_file = file_path.clone();
        let file_value = lexical_file.to_string_lossy().to_string().into_value();

        let Some(existing_start) = find_existing_start(&file_path) else {
            return Ok(msgpack_map! {
                "file" => file_value,
                "locals" => Value::Nil,
                "cache" => Value::Nil
            });
        };
        let Some(start_dir) = as_search_dir(existing_start) else {
            return Ok(msgpack_map! {
                "file" => file_value,
                "locals" => Value::Nil,
                "cache" => Value::Nil
            });
        };

        let locals_value =
            if let Some((locals_dir, _)) = find_dominating_dir(&start_dir, &params.names)? {
                let locals_dir = remap_to_lexical_ancestor(&start_dir, &locals_dir);
                let local_files: Vec<Value> = params
                    .names
                    .iter()
                    .filter_map(|name| {
                        let p = locals_dir.join(name);
                        if p.is_file() {
                            mtime_seconds(&p).map(|mtime| {
                                msgpack_map! {
                                    "name" => name.clone(),
                                    "mtime" => mtime
                                }
                            })
                        } else {
                            None
                        }
                    })
                    .collect();
                msgpack_map! {
                    "dir" => locals_dir.to_string_lossy().to_string(),
                    "files" => Value::Array(local_files)
                }
            } else {
                Value::Nil
            };

        let mut best_cache: Option<PathBuf> = None;
        for cache_dir in &params.cache_dirs {
            let p = PathBuf::from(cache_dir);
            if p.is_dir()
                && lexical_file.starts_with(&p)
                && best_cache
                    .as_ref()
                    .map(|best| p.components().count() > best.components().count())
                    .unwrap_or(true)
            {
                best_cache = Some(p);
            }
        }

        let cache_value = if let Some(cache_dir) = best_cache {
            let cache_files: Vec<Value> = params
                .names
                .iter()
                .filter_map(|name| {
                    let p = cache_dir.join(name);
                    if p.is_file() {
                        mtime_seconds(&p).map(|mtime| {
                            msgpack_map! {
                                "name" => name.clone(),
                                "mtime" => mtime
                            }
                        })
                    } else {
                        None
                    }
                })
                .collect();
            msgpack_map! {
                "dir" => cache_dir.to_string_lossy().to_string(),
                "files" => Value::Array(cache_files)
            }
        } else {
            Value::Nil
        };

        Ok(msgpack_map! {
            "file" => file_value,
            "locals" => locals_value,
            "cache" => cache_value
        })
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ancestor_paths_use_text_when_compatible_and_binary_when_required() {
        assert_eq!(
            ancestor_path_value(b"/tmp/project".to_vec()),
            Value::String("/tmp/project".into())
        );
        assert_eq!(
            ancestor_path_value(b"/tmp/\xff".to_vec()),
            Value::Binary(b"/tmp/\xff".to_vec())
        );
    }

    #[tokio::test]
    async fn parallel_child_admission_queues_until_release() {
        let semaphore = Semaphore::new(1);
        let first = semaphore.acquire().await.unwrap();
        let queued = semaphore.acquire();
        tokio::pin!(queued);
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(10), queued.as_mut())
                .await
                .is_err()
        );
        drop(first);
        let _permit = tokio::time::timeout(std::time::Duration::from_secs(1), queued)
            .await
            .expect("queued entry should proceed after release")
            .expect("test semaphore remains open");
    }
}
