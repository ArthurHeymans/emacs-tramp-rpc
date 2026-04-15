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
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread;
use std::time::UNIX_EPOCH;

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
    }

    #[derive(Deserialize)]
    struct Params {
        commands: Vec<CommandEntry>,
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
                            cmd.current_dir(super::expand_tilde(cwd));
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
                .filter_map(|h| h.join().ok()) // thread panicked; skip this result
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
pub async fn ancestors_scan(params: Value) -> HandlerResult {
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

    let params: Params = from_value(params).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    // Wrap in spawn_blocking since this does blocking filesystem I/O
    let expanded_directory = super::expand_tilde(&params.directory);
    tokio::task::spawn_blocking(move || {
        let dir = Path::new(&expanded_directory);
        if !dir.exists() {
            return Err(RpcError::file_not_found(&expanded_directory));
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
                        results
                            .insert(marker.clone(), Some(current.to_string_lossy().into_owned()));
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
        let dir = canonical_or_original(Path::new(&params.directory));
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
        let path = PathBuf::from(&params.file);
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
        let file_path = PathBuf::from(&params.file);
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
