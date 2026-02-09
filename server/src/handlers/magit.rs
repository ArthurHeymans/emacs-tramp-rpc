//! Magit-specific operations for efficient remote git status
//!
//! This module provides a single RPC call that returns all data needed
//! for magit-status, eliminating the need for dozens of individual git calls.
//!
//! Optimized to run independent git commands in parallel using thread::scope.

use crate::msgpack_map;
use crate::protocol::{from_value, IntoValue, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use std::thread;

use super::HandlerResult;

/// Get complete magit status data in a single call
///
/// This replaces ~60 individual git commands with one RPC call.
/// Returns all data needed to render a magit-status buffer.
/// Independent git commands are run in parallel for lower latency.
pub async fn status(params: &Value) -> HandlerResult {
    #[derive(Deserialize)]
    struct Params {
        /// Repository directory
        directory: String,
    }

    let params: Params =
        from_value(params.clone()).map_err(|e| RpcError::invalid_params(e.to_string()))?;

    let dir = Path::new(&params.directory);
    if !dir.exists() {
        return Err(RpcError::file_not_found(&params.directory));
    }

    // Run git commands in a blocking task to avoid blocking the async runtime
    let directory = params.directory.clone();

    tokio::task::spawn_blocking(move || collect_magit_status(&directory))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

/// Collect all magit status data synchronously (runs in blocking task)
/// Uses thread::scope to run independent git commands in parallel.
fn collect_magit_status(directory: &str) -> HandlerResult {
    // Phase 1: Get basic repo info first (needed by later phases)
    let toplevel = git_string(directory, &["rev-parse", "--show-toplevel"]);
    let gitdir = git_string(directory, &["rev-parse", "--git-dir"]);

    // Phase 2: Run all independent git commands in parallel
    thread::scope(|s| {
        // HEAD info - 4 independent commands
        let head_hash_h = s.spawn(|| git_string(directory, &["rev-parse", "HEAD"]));
        let head_short_h = s.spawn(|| git_string(directory, &["rev-parse", "--short", "HEAD"]));
        let head_branch_h = s.spawn(|| git_string(directory, &["symbolic-ref", "--short", "HEAD"]));
        let head_message_h =
            s.spawn(|| git_string(directory, &["log", "-1", "--format=%s", "HEAD"]));

        // Upstream info
        let upstream_branch_h =
            s.spawn(|| git_string(directory, &["rev-parse", "--abbrev-ref", "@{upstream}"]));

        // Push remote info
        let push_branch_h =
            s.spawn(|| git_string(directory, &["rev-parse", "--abbrev-ref", "@{push}"]));

        // Staged changes (2 commands)
        // Use magit's exact flags: --ita-visible-in-index --no-ext-diff --no-prefix
        let staged_diff_h = s.spawn(|| {
            git_output(
                directory,
                &[
                    "diff",
                    "--ita-visible-in-index",
                    "--cached",
                    "--no-ext-diff",
                    "--no-prefix",
                    "--",
                ],
            )
        });
        let staged_stat_h =
            s.spawn(|| git_string(directory, &["diff", "--cached", "--stat", "--no-color"]));

        // Unstaged changes (2 commands)
        let unstaged_diff_h = s.spawn(|| {
            git_output(
                directory,
                &[
                    "diff",
                    "--ita-visible-in-index",
                    "--no-ext-diff",
                    "--no-prefix",
                    "--",
                ],
            )
        });
        let unstaged_stat_h = s.spawn(|| git_string(directory, &["diff", "--stat", "--no-color"]));

        // Untracked files
        let untracked_h = s.spawn(|| {
            git_lines(
                directory,
                &[
                    "ls-files",
                    "--others",
                    "--exclude-standard",
                    "--directory",
                    "--no-empty-directory",
                ],
            )
        });

        // (Stash list and recent commits are now served via raw-format
        // equivalents: stash_reflog and recent_decorated below.)

        // Tags (2 commands)
        let tag_at_head_h =
            s.spawn(|| git_string(directory, &["describe", "--tags", "--exact-match", "HEAD"]));
        let tag_contains_h =
            s.spawn(|| git_string(directory, &["describe", "--tags", "--abbrev=0"]));

        // Remotes
        let remotes_h = s.spawn(|| git_lines(directory, &["remote"]));

        // Config
        let config_h = s.spawn(|| collect_git_config(directory));

        // State files (filesystem only, no git commands)
        let gitdir_clone = gitdir.clone();
        let dir_clone = directory.to_string();
        let state_files_h =
            s.spawn(move || collect_state_files(&dir_clone, gitdir_clone.as_deref()));

        // Repo state detection
        let gitdir_clone2 = gitdir.clone();
        let dir_clone2 = directory.to_string();
        let state_h = s.spawn(move || detect_repo_state(&dir_clone2, gitdir_clone2.as_deref()));

        // === Additional commands matching magit's exact invocations ===

        // Full config list (magit calls: config --list -z)
        let config_list_h = s.spawn(|| git_output(directory, &["config", "--list", "-z"]));

        // Tag descriptions in magit's format
        let describe_long_h =
            s.spawn(|| git_string(directory, &["describe", "--long", "--tags"]));
        let describe_contains_h =
            s.spawn(|| git_string(directory, &["describe", "--contains", "HEAD"]));

        // Porcelain status (magit calls: status -z --porcelain --untracked-files=normal --)
        let status_porcelain_h = s.spawn(|| {
            git_output(
                directory,
                &[
                    "status",
                    "-z",
                    "--porcelain",
                    "--untracked-files=normal",
                    "--",
                ],
            )
        });

        // showUntrackedFiles config
        let config_untracked_h = s.spawn(|| {
            git_string(
                directory,
                &[
                    "config",
                    "--local",
                    "-z",
                    "--get-all",
                    "--include",
                    "status.showUntrackedFiles",
                ],
            )
        });

        // Stash reflog (magit's format: %gd%x00%aN%x00%at%x00%gs)
        let stash_reflog_h = s.spawn(|| {
            git_output(
                directory,
                &[
                    "reflog",
                    "--format=%gd%x00%aN%x00%at%x00%gs",
                    "refs/stash",
                ],
            )
        });

        // Parent commit info
        let head_parent_short_h =
            s.spawn(|| git_string(directory, &["rev-parse", "--short", "HEAD~"]));
        let head_parent_10_h =
            s.spawn(|| git_string(directory, &["rev-parse", "--verify", "HEAD~10"]));

        // Recent log with decorations (magit's format)
        let recent_decorated_h = s.spawn(|| {
            git_output(
                directory,
                &[
                    "log",
                    "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s",
                    "--decorate=full",
                    "-n10",
                    "--use-mailmap",
                    "--no-prefix",
                    "--",
                ],
            )
        });

        // ---- Collect all results ----
        let head_hash = head_hash_h.join().unwrap();
        let head_short = head_short_h.join().unwrap();
        let head_branch = head_branch_h.join().unwrap();
        let head_message = head_message_h.join().unwrap();
        let upstream_branch = upstream_branch_h.join().unwrap();
        let push_branch = push_branch_h.join().unwrap();
        let staged_diff = staged_diff_h.join().unwrap();
        let staged_stat = staged_stat_h.join().unwrap();
        let unstaged_diff = unstaged_diff_h.join().unwrap();
        let unstaged_stat = unstaged_stat_h.join().unwrap();
        let untracked = untracked_h.join().unwrap();
        let tag_at_head = tag_at_head_h.join().unwrap();
        let tag_contains = tag_contains_h.join().unwrap();
        let remotes = remotes_h.join().unwrap();
        let config = config_h.join().unwrap();
        let state_files = state_files_h.join().unwrap();
        let state = state_h.join().unwrap();
        let config_list = config_list_h.join().unwrap();
        let describe_long = describe_long_h.join().unwrap();
        let describe_contains = describe_contains_h.join().unwrap();
        let status_porcelain = status_porcelain_h.join().unwrap();
        let config_untracked = config_untracked_h.join().unwrap();
        let stash_reflog = stash_reflog_h.join().unwrap();
        let head_parent_short = head_parent_short_h.join().unwrap();
        let head_parent_10 = head_parent_10_h.join().unwrap();
        let recent_decorated = recent_decorated_h.join().unwrap();

        // Phase 3: Dependent operations (need upstream/push branch results)
        // These are quick since they're just rev-list counts
        let (upstream_ahead, upstream_behind) = if upstream_branch.is_some() {
            parse_ahead_behind(git_string(
                directory,
                &["rev-list", "--count", "--left-right", "@{upstream}...HEAD"],
            ))
        } else {
            (None, None)
        };

        let (push_ahead, push_behind) = if push_branch.is_some() {
            parse_ahead_behind(git_string(
                directory,
                &["rev-list", "--count", "--left-right", "@{push}...HEAD"],
            ))
        } else {
            (None, None)
        };

        // Build result
        let mut result: Vec<(Value, Value)> = Vec::with_capacity(25);

        result.push(("toplevel".into_value(), toplevel.into_value()));
        result.push(("gitdir".into_value(), gitdir.into_value()));

        result.push((
            "head".into_value(),
            msgpack_map! {
                "hash" => head_hash.into_value(),
                "short" => head_short.into_value(),
                "branch" => head_branch.into_value(),
                "message" => head_message.into_value()
            },
        ));

        result.push((
            "upstream".into_value(),
            msgpack_map! {
                "branch" => upstream_branch.into_value(),
                "ahead" => upstream_ahead.into_value(),
                "behind" => upstream_behind.into_value()
            },
        ));

        result.push((
            "push".into_value(),
            msgpack_map! {
                "branch" => push_branch.into_value(),
                "ahead" => push_ahead.into_value(),
                "behind" => push_behind.into_value()
            },
        ));

        result.push(("state".into_value(), state));

        result.push((
            "staged".into_value(),
            msgpack_map! {
                "diff" => staged_diff.into_value(),
                "stat" => staged_stat.into_value()
            },
        ));

        result.push((
            "unstaged".into_value(),
            msgpack_map! {
                "diff" => unstaged_diff.into_value(),
                "stat" => unstaged_stat.into_value()
            },
        ));

        result.push(("untracked".into_value(), untracked.into_value()));

        result.push((
            "tags".into_value(),
            msgpack_map! {
                "at_head" => tag_at_head.into_value(),
                "latest" => tag_contains.into_value()
            },
        ));

        result.push(("remotes".into_value(), remotes.into_value()));
        result.push(("config".into_value(), config));
        result.push(("state_files".into_value(), state_files));

        // Additional fields for magit's exact command patterns
        result.push(("config_list".into_value(), config_list.into_value()));
        result.push(("describe_long".into_value(), describe_long.into_value()));
        result.push(("describe_contains".into_value(), describe_contains.into_value()));
        result.push(("status_porcelain".into_value(), status_porcelain.into_value()));
        result.push(("config_untracked".into_value(), config_untracked.into_value()));
        result.push(("stash_reflog".into_value(), stash_reflog.into_value()));
        result.push(("head_parent_short".into_value(), head_parent_short.into_value()));
        result.push(("head_parent_10".into_value(), head_parent_10.into_value()));
        result.push(("recent_decorated".into_value(), recent_decorated.into_value()));

        Ok(Value::Map(result))
    })
}

/// Detect repository state (merge, rebase, cherry-pick, etc.)
fn detect_repo_state(directory: &str, gitdir: Option<&str>) -> Value {
    let gitdir = match gitdir {
        Some(d) => {
            if Path::new(d).is_absolute() {
                d.to_string()
            } else {
                format!("{}/{}", directory, d)
            }
        }
        None => format!("{}/.git", directory),
    };

    let state = if Path::new(&format!("{}/rebase-merge", gitdir)).exists() {
        if Path::new(&format!("{}/rebase-merge/interactive", gitdir)).exists() {
            "rebase-interactive"
        } else {
            "rebase-merge"
        }
    } else if Path::new(&format!("{}/rebase-apply", gitdir)).exists() {
        if Path::new(&format!("{}/rebase-apply/applying", gitdir)).exists() {
            "am"
        } else {
            "rebase-apply"
        }
    } else if Path::new(&format!("{}/MERGE_HEAD", gitdir)).exists() {
        "merge"
    } else if Path::new(&format!("{}/CHERRY_PICK_HEAD", gitdir)).exists() {
        "cherry-pick"
    } else if Path::new(&format!("{}/REVERT_HEAD", gitdir)).exists() {
        "revert"
    } else if Path::new(&format!("{}/BISECT_LOG", gitdir)).exists() {
        "bisect"
    } else {
        return Value::Nil;
    };

    state.into_value()
}

/// Collect commonly-needed git config values
fn collect_git_config(directory: &str) -> Value {
    let configs = [
        ("user.name", git_string(directory, &["config", "user.name"])),
        (
            "user.email",
            git_string(directory, &["config", "user.email"]),
        ),
        (
            "remote.origin.url",
            git_string(directory, &["config", "remote.origin.url"]),
        ),
        (
            "core.bare",
            git_string(
                directory,
                &["config", "--bool", "--default", "false", "core.bare"],
            ),
        ),
    ];

    let pairs: Vec<(Value, Value)> = configs
        .iter()
        .map(|(k, v)| ((*k).into_value(), v.clone().into_value()))
        .collect();

    Value::Map(pairs)
}

/// Check existence of git state files
fn collect_state_files(directory: &str, gitdir: Option<&str>) -> Value {
    let gitdir = match gitdir {
        Some(d) => {
            if Path::new(d).is_absolute() {
                d.to_string()
            } else {
                format!("{}/{}", directory, d)
            }
        }
        None => format!("{}/.git", directory),
    };

    // All state files that magit might check for
    let files = [
        // Merge/revert/cherry-pick state
        "MERGE_HEAD",
        "REVERT_HEAD",
        "CHERRY_PICK_HEAD",
        "ORIG_HEAD",
        "FETCH_HEAD",
        "AUTO_MERGE",
        "SQUASH_MSG",
        // Bisect state
        "BISECT_LOG",
        "BISECT_CMD_OUTPUT",
        "BISECT_TERMS",
        // Interactive rebase (rebase-merge)
        "rebase-merge",
        "rebase-merge/git-rebase-todo",
        "rebase-merge/done",
        "rebase-merge/onto",
        "rebase-merge/orig-head",
        "rebase-merge/head-name",
        "rebase-merge/amend",
        "rebase-merge/stopped-sha",
        "rebase-merge/rewritten-pending",
        // Non-interactive rebase / am (rebase-apply)
        "rebase-apply",
        "rebase-apply/onto",
        "rebase-apply/head-name",
        "rebase-apply/applying",
        "rebase-apply/original-commit",
        "rebase-apply/rewritten",
        // Sequencer (cherry-pick/revert --continue)
        "sequencer",
        "sequencer/todo",
        "sequencer/head",
        // Basic git state
        "HEAD",
        "config",
        "index",
        "refs/stash",
        "info/exclude",
        "NOTES_MERGE_WORKTREE",
    ];

    let pairs: Vec<(Value, Value)> = files
        .iter()
        .map(|f| {
            let path = format!("{}/{}", gitdir, f);
            let exists = Path::new(&path).exists();
            ((*f).into_value(), exists.into_value())
        })
        .collect();

    Value::Map(pairs)
}

/// Run a git command and return stdout as a single string (trimmed)
fn git_string(directory: &str, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(directory)
        .output()
        .ok()?;

    if output.status.success() {
        let s = String::from_utf8_lossy(&output.stdout).trim().to_string();
        if s.is_empty() {
            None
        } else {
            Some(s)
        }
    } else {
        None
    }
}

/// Run a git command and return stdout as raw bytes
fn git_output(directory: &str, args: &[&str]) -> Option<Vec<u8>> {
    let output = Command::new("git")
        .args(args)
        .current_dir(directory)
        .output()
        .ok()?;

    if output.status.success() && !output.stdout.is_empty() {
        Some(output.stdout)
    } else {
        None
    }
}

/// Run a git command and return stdout as lines
fn git_lines(directory: &str, args: &[&str]) -> Vec<String> {
    git_string(directory, args)
        .map(|s| s.lines().map(|l| l.to_string()).collect())
        .unwrap_or_default()
}

/// Parse "behind\tahead" format from rev-list --left-right --count
fn parse_ahead_behind(output: Option<String>) -> (Option<u32>, Option<u32>) {
    match output {
        Some(s) => {
            let parts: Vec<&str> = s.split_whitespace().collect();
            if parts.len() == 2 {
                let behind = parts[0].parse().ok();
                let ahead = parts[1].parse().ok();
                (ahead, behind)
            } else {
                (None, None)
            }
        }
        None => (None, None),
    }
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
}
