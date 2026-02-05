//! Magit-specific operations for efficient remote git status
//!
//! This module provides a single RPC call that returns all data needed
//! for magit-status, eliminating the need for dozens of individual git calls.

use crate::msgpack_map;
use crate::protocol::{from_value, IntoValue, RpcError};
use rmpv::Value;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use super::HandlerResult;

/// Get complete magit status data in a single call
///
/// This replaces ~60 individual git commands with one RPC call.
/// Returns all data needed to render a magit-status buffer.
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

    // Run git commands in parallel using tokio
    let directory = params.directory.clone();

    tokio::task::spawn_blocking(move || collect_magit_status(&directory))
        .await
        .map_err(|e| RpcError::internal_error(format!("Task join error: {}", e)))?
}

/// Collect all magit status data synchronously (runs in blocking task)
fn collect_magit_status(directory: &str) -> HandlerResult {
    let mut result: Vec<(Value, Value)> = Vec::new();

    // Basic repo info
    let toplevel = git_string(directory, &["rev-parse", "--show-toplevel"]);
    let gitdir = git_string(directory, &["rev-parse", "--git-dir"]);

    result.push(("toplevel".into_value(), toplevel.into_value()));
    result.push(("gitdir".into_value(), gitdir.clone().into_value()));

    // HEAD info
    let head_hash = git_string(directory, &["rev-parse", "HEAD"]);
    let head_short = git_string(directory, &["rev-parse", "--short", "HEAD"]);
    let head_branch = git_string(directory, &["symbolic-ref", "--short", "HEAD"]);
    let head_message = git_string(directory, &["log", "-1", "--format=%s", "HEAD"]);

    result.push((
        "head".into_value(),
        msgpack_map! {
            "hash" => head_hash.into_value(),
            "short" => head_short.into_value(),
            "branch" => head_branch.into_value(),
            "message" => head_message.into_value()
        },
    ));

    // Upstream info
    let upstream_branch = git_string(directory, &["rev-parse", "--abbrev-ref", "@{upstream}"]);
    let (upstream_ahead, upstream_behind) = if upstream_branch.is_some() {
        parse_ahead_behind(git_string(
            directory,
            &["rev-list", "--count", "--left-right", "@{upstream}...HEAD"],
        ))
    } else {
        (None, None)
    };

    result.push((
        "upstream".into_value(),
        msgpack_map! {
            "branch" => upstream_branch.into_value(),
            "ahead" => upstream_ahead.into_value(),
            "behind" => upstream_behind.into_value()
        },
    ));

    // Push remote info
    let push_branch = git_string(directory, &["rev-parse", "--abbrev-ref", "@{push}"]);
    let (push_ahead, push_behind) = if push_branch.is_some() {
        parse_ahead_behind(git_string(
            directory,
            &["rev-list", "--count", "--left-right", "@{push}...HEAD"],
        ))
    } else {
        (None, None)
    };

    result.push((
        "push".into_value(),
        msgpack_map! {
            "branch" => push_branch.into_value(),
            "ahead" => push_ahead.into_value(),
            "behind" => push_behind.into_value()
        },
    ));

    // Repository state (merge, rebase, cherry-pick, etc.)
    let state = detect_repo_state(directory, gitdir.as_deref());
    result.push(("state".into_value(), state));

    // Staged changes (git diff --cached)
    let staged_diff = git_output(directory, &["diff", "--cached", "--no-color"]);
    let staged_stat = git_string(directory, &["diff", "--cached", "--stat", "--no-color"]);
    result.push((
        "staged".into_value(),
        msgpack_map! {
            "diff" => staged_diff.into_value(),
            "stat" => staged_stat.into_value()
        },
    ));

    // Unstaged changes (git diff)
    let unstaged_diff = git_output(directory, &["diff", "--no-color"]);
    let unstaged_stat = git_string(directory, &["diff", "--stat", "--no-color"]);
    result.push((
        "unstaged".into_value(),
        msgpack_map! {
            "diff" => unstaged_diff.into_value(),
            "stat" => unstaged_stat.into_value()
        },
    ));

    // Untracked files
    let untracked = git_lines(
        directory,
        &[
            "ls-files",
            "--others",
            "--exclude-standard",
            "--directory",
            "--no-empty-directory",
        ],
    );
    result.push(("untracked".into_value(), untracked.into_value()));

    // Stashes
    let stashes = git_lines(directory, &["stash", "list", "--format=%gd\t%gs"]);
    let stash_list: Vec<Value> = stashes
        .iter()
        .filter_map(|line| {
            let parts: Vec<&str> = line.splitn(2, '\t').collect();
            if parts.len() == 2 {
                Some(msgpack_map! {
                    "ref" => parts[0].to_string().into_value(),
                    "message" => parts[1].to_string().into_value()
                })
            } else {
                None
            }
        })
        .collect();
    result.push(("stashes".into_value(), Value::Array(stash_list)));

    // Recent commits (for unpushed section)
    let recent = git_lines(directory, &["log", "-20", "--format=%H\t%s", "HEAD"]);
    let recent_commits: Vec<Value> = recent
        .iter()
        .filter_map(|line| {
            let parts: Vec<&str> = line.splitn(2, '\t').collect();
            if parts.len() == 2 {
                Some(msgpack_map! {
                    "hash" => parts[0].to_string().into_value(),
                    "message" => parts[1].to_string().into_value()
                })
            } else {
                None
            }
        })
        .collect();
    result.push(("recent_commits".into_value(), Value::Array(recent_commits)));

    // Tags
    let tag_at_head = git_string(directory, &["describe", "--tags", "--exact-match", "HEAD"]);
    let tag_contains = git_string(directory, &["describe", "--tags", "--abbrev=0"]);
    result.push((
        "tags".into_value(),
        msgpack_map! {
            "at_head" => tag_at_head.into_value(),
            "latest" => tag_contains.into_value()
        },
    ));

    // Remotes
    let remotes = git_lines(directory, &["remote"]);
    result.push(("remotes".into_value(), remotes.into_value()));

    // Config values magit commonly needs
    let config = collect_git_config(directory);
    result.push(("config".into_value(), config));

    // Git state files existence (for detecting merge/rebase/etc state)
    let state_files = collect_state_files(directory, gitdir.as_deref());
    result.push(("state_files".into_value(), state_files));

    Ok(Value::Map(result))
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
    let mut results: HashMap<String, Option<String>> = params
        .markers
        .iter()
        .map(|m| (m.clone(), None))
        .collect();

    // Walk up the directory tree
    let mut current = dir.to_path_buf();
    let mut depth = 0;

    while depth < params.max_depth {
        // Check each marker that hasn't been found yet
        for marker in &params.markers {
            if results.get(marker).unwrap().is_none() {
                let marker_path = current.join(marker);
                if marker_path.exists() {
                    results.insert(
                        marker.clone(),
                        Some(current.to_string_lossy().to_string()),
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
        .map(|(k, v)| (k.into_value(), v.into_value()))
        .collect();

    Ok(Value::Map(pairs))
}
