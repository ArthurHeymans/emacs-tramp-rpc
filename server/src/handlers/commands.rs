//! Command execution and ancestor scanning for TRAMP-RPC
//!
//! This module provides:
//! - `commands.run_parallel`: Run multiple commands in parallel using OS threads
//! - `ancestors.scan`: Scan ancestor directories for marker files

use crate::msgpack_map;
use crate::protocol::{from_value, IntoValue, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use std::thread;

use super::HandlerResult;

/// Maximum number of commands that can be run in a single request.
/// Prevents resource exhaustion from excessively large batches.
const MAX_PARALLEL_COMMANDS: usize = 256;

/// Run multiple commands in parallel using OS threads.
///
/// Each command is spawned as an OS thread via `thread::scope`, giving true
/// parallelism for I/O-bound operations like git commands.  Returns a map
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
pub async fn run_parallel(params: &Value) -> HandlerResult {
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
    }

    #[derive(Deserialize)]
    struct Params {
        commands: Vec<CommandEntry>,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

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

    // Run all commands in parallel using OS threads (not async tasks)
    // to get true parallelism for blocking process spawning.
    tokio::task::spawn_blocking(move || {
        let results: Vec<(String, Value)> = thread::scope(|s| {
            let handles: Vec<_> = params
                .commands
                .into_iter()
                .map(|entry| {
                    s.spawn(move || {
                        let mut cmd = Command::new(&entry.cmd);
                        cmd.args(&entry.args);
                        if let Some(ref cwd) = entry.cwd {
                            cmd.current_dir(cwd);
                        }
                        let value = match cmd.output() {
                            Ok(output) => {
                                msgpack_map! {
                                    "exit_code" => output.status.code().unwrap_or(-1),
                                    "stdout" => Value::Binary(output.stdout),
                                    "stderr" => Value::Binary(output.stderr)
                                }
                            }
                            Err(e) => {
                                msgpack_map! {
                                    "exit_code" => -1i32,
                                    "stdout" => Value::Binary(vec![]),
                                    "stderr" => Value::Binary(e.to_string().into_bytes())
                                }
                            }
                        };
                        (entry.key, value)
                    })
                })
                .collect();

            // Recover from thread panics instead of unwinding
            handles
                .into_iter()
                .filter_map(|h| match h.join() {
                    Ok(result) => Some(result),
                    Err(_) => None, // thread panicked; skip this result
                })
                .collect()
        });

        let pairs: Vec<(Value, Value)> = results
            .into_iter()
            .map(|(k, v)| (Value::String(k.into()), v))
            .collect();

        Ok(Value::Map(pairs))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

/// Scan ancestor directories for marker files
///
/// This is useful for project detection, VCS detection, etc.
/// Returns a map of marker -> directory where it was found (or null if not found)
pub async fn ancestors_scan(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Starting directory
        directory: String,
        /// Marker files/directories to look for
        markers: Vec<String>,
        /// Maximum depth to search (default: 10)
        #[serde(default = "default_max_depth")]
        max_depth: usize,
    }

    fn default_max_depth() -> usize {
        10
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Wrap in spawn_blocking since this does blocking filesystem I/O
    tokio::task::spawn_blocking(move || {
        let dir = Path::new(&params.directory);
        if !dir.exists() {
            return Err(RpcError::file_not_found(&params.directory));
        }

        // Initialize results with None for each marker
        let mut results: HashMap<String, Option<String>> =
            params.markers.iter().map(|m| (m.clone(), None)).collect();

        // Walk up the directory tree
        let mut current = dir.to_path_buf();
        let mut depth = 0;

        while depth < params.max_depth {
            // Check each marker that hasn't been found yet
            for marker in &params.markers {
                if results.get(marker).unwrap().is_none() {
                    let marker_path = current.join(marker);
                    if marker_path.exists() {
                        results.insert(marker.clone(), Some(current.to_string_lossy().to_string()));
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
            .map(|(k, v)| (k.into_value(), v.into_value()))
            .collect();

        Ok(Value::Map(pairs))
    })
    .await
    .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}
