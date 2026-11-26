// SPDX-License-Identifier: GPL-3.0-or-later

//! Compile-time-checked generator and verifier for `doc/RPC_METHODS.org`.
//!
//! Only compiled under `#[cfg(test)]`, so it adds zero bytes to the release
//! server binary.  Adding or removing a `Method` variant in `protocol.rs` is a
//! compile error here until its documentation entry is updated, and
//! `cargo test` fails if `doc/RPC_METHODS.org` drifts out of sync.
//!
//! Regenerate with:
//!   UPDATE_RPC_DOCS=1 cargo test rpc_methods_doc_is_up_to_date

use crate::protocol::{Method, PROTOCOL_REVISION};
use std::collections::HashSet;
use std::fmt::Write as _;
use std::path::Path;

struct MethodDoc {
    wire_name: &'static str,
    category: &'static str,
    params: &'static [&'static str],
    description: &'static str,
}

const ALL_METHODS: &[Method] = &[
    Method::Batch,
    Method::FileStat,
    Method::FileAccess,
    Method::FileTruename,
    Method::DirList,
    Method::DirCreate,
    Method::DirRemove,
    Method::FileRead,
    Method::FileWrite,
    Method::FileCopy,
    Method::FileRename,
    Method::FileDelete,
    Method::FileSetModes,
    Method::FileSetTimes,
    Method::FileMakeSymlink,
    Method::FileMakeHardlink,
    Method::FileChown,
    Method::ProcessRun,
    Method::ProcessStart,
    Method::ProcessWrite,
    Method::ProcessCloseStdin,
    Method::ProcessKill,
    Method::ProcessSignal,
    Method::ProcessStartPty,
    Method::ProcessWritePty,
    Method::ProcessResizePty,
    Method::ProcessKillPty,
    Method::ProcessClosePty,
    Method::SystemPing,
    Method::SystemInfo,
    Method::SystemStatvfs,
    Method::SystemUserHome,
    Method::CommandsRunParallel,
    Method::AncestorsScan,
    Method::HighlevelTestFilesInDir,
    Method::HighlevelLocateDominatingFileMulti,
    Method::HighlevelDirLocalsFindFileCacheUpdate,
    Method::WatchAdd,
    Method::WatchRemove,
];

const NOTIFICATIONS: &[(&str, &str)] = &[
    (
        "fs.events",
        "Batched filesystem change events from watched directories (or ~rescan~).",
    ),
    (
        "process.output",
        "Streamed ~stdout~ / ~stderr~ bytes for a managed pipe or PTY process.",
    ),
    (
        "process.exit",
        "Final ~exit_code~ and terminating ~signal~ when a managed process exits.",
    ),
];

/// Exhaustive documentation mapping for every `Method` variant.
/// Adding a new variant to `Method` fails to compile here until documented.
fn method_doc(method: &Method) -> Option<MethodDoc> {
    let doc = match method {
        Method::Batch => MethodDoc {
            wire_name: "batch",
            category: "Batch",
            params: &["requests"],
            description: "Execute up to 64 RPC requests in a single round-trip with bounded concurrency.",
        },
        Method::FileStat => MethodDoc {
            wire_name: "file.stat",
            category: "File Metadata",
            params: &["path", "lstat?"],
            description: "Return file attributes (or ~nil~ if absent).",
        },
        Method::FileAccess => MethodDoc {
            wire_name: "file.access",
            category: "File Metadata",
            params: &["path", "mode", "check_parent_if_missing?"],
            description: "Check effective-credential access via the kernel; filesystem denials return ~errno~ + ~message~.",
        },
        Method::FileTruename => MethodDoc {
            wire_name: "file.truename",
            category: "File Metadata",
            params: &["path"],
            description: "Resolve a file's canonical path (including symlinked ancestors of missing paths).",
        },
        Method::DirList => MethodDoc {
            wire_name: "dir.list",
            category: "Directory",
            params: &["path", "include_attrs?", "include_hidden?"],
            description: "List directory entries, optionally with full attributes.",
        },
        Method::DirCreate => MethodDoc {
            wire_name: "dir.create",
            category: "Directory",
            params: &["path", "parents?", "mode?"],
            description: "Create a directory.",
        },
        Method::DirRemove => MethodDoc {
            wire_name: "dir.remove",
            category: "Directory",
            params: &["path", "recursive?"],
            description: "Remove a directory.",
        },
        Method::FileRead => MethodDoc {
            wire_name: "file.read",
            category: "File I/O",
            params: &["path", "offset?", "length?", "compress?", "include_meta?"],
            description: "Read raw file bytes (optionally zlib-compressed and/or with metadata snapshot).",
        },
        Method::FileWrite => MethodDoc {
            wire_name: "file.write",
            category: "File I/O",
            params: &[
                "path",
                "content",
                "mode?",
                "append?",
                "offset?",
                "mustbenew?",
                "compress?",
                "return_stat?",
            ],
            description: "Write raw file bytes.",
        },
        Method::FileCopy => MethodDoc {
            wire_name: "file.copy",
            category: "File I/O",
            params: &[
                "src",
                "dest",
                "preserve?",
                "preserve_permissions?",
                "preserve_times?",
                "overwrite?",
                "exact_dest?",
                "merge_existing_directories?",
            ],
            description: "Copy a file or directory.",
        },
        Method::FileRename => MethodDoc {
            wire_name: "file.rename",
            category: "File I/O",
            params: &["src", "dest", "overwrite?"],
            description: "Rename or move a file.",
        },
        Method::FileDelete => MethodDoc {
            wire_name: "file.delete",
            category: "File I/O",
            params: &["path", "force?"],
            description: "Delete a file.",
        },
        Method::FileSetModes => MethodDoc {
            wire_name: "file.set_modes",
            category: "File I/O",
            params: &["path", "mode", "nofollow"],
            description: "Set file permission bits.",
        },
        Method::FileSetTimes => MethodDoc {
            wire_name: "file.set_times",
            category: "File I/O",
            params: &[
                "path",
                "mtime",
                "mtime_nsec?",
                "atime?",
                "atime_nsec?",
                "nofollow?",
            ],
            description: "Set file access and modification timestamps.",
        },
        Method::FileMakeSymlink => MethodDoc {
            wire_name: "file.make_symlink",
            category: "File I/O",
            params: &["target", "link_path"],
            description: "Create a symbolic link.",
        },
        Method::FileMakeHardlink => MethodDoc {
            wire_name: "file.make_hardlink",
            category: "File I/O",
            params: &["src", "dest"],
            description: "Create a hard link.",
        },
        Method::FileChown => MethodDoc {
            wire_name: "file.chown",
            category: "File I/O",
            params: &["path", "uid", "gid"],
            description: "Change file ownership.",
        },
        Method::ProcessRun => MethodDoc {
            wire_name: "process.run",
            category: "Process",
            params: &[
                "cmd",
                "args?",
                "cwd?",
                "env?",
                "stdin?",
                "clear_env?",
                "merge_stderr?",
            ],
            description: "Run a command synchronously and return exit status, stdout, and stderr.",
        },
        Method::ProcessStart => MethodDoc {
            wire_name: "process.start",
            category: "Process",
            params: &["cmd", "args?", "cwd?", "env?", "clear_env?"],
            description: "Start a managed async pipe process; output is pushed via ~process.output~.",
        },
        Method::ProcessWrite => MethodDoc {
            wire_name: "process.write",
            category: "Process",
            params: &["pid", "data"],
            description: "Write bytes to a managed process's stdin.",
        },
        Method::ProcessCloseStdin => MethodDoc {
            wire_name: "process.close_stdin",
            category: "Process",
            params: &["pid"],
            description: "Close a managed process's stdin (send EOF).",
        },
        Method::ProcessKill => MethodDoc {
            wire_name: "process.kill",
            category: "Process",
            params: &["pid", "signal?"],
            description: "Signal a managed process and reap its direct child.",
        },
        Method::ProcessSignal => MethodDoc {
            wire_name: "process.signal",
            category: "Process",
            params: &["pid", "signal"],
            description: "Signal a positive OS PID permitted by the server's credentials.",
        },
        Method::ProcessStartPty => MethodDoc {
            wire_name: "process.start_pty",
            category: "PTY",
            params: &[
                "cmd",
                "args?",
                "cwd?",
                "env?",
                "clear_env?",
                "rows?",
                "cols?",
            ],
            description: "Start a pseudo-terminal process; output is pushed via ~process.output~.",
        },
        Method::ProcessWritePty => MethodDoc {
            wire_name: "process.write_pty",
            category: "PTY",
            params: &["pid", "data"],
            description: "Write input bytes to a pseudo-terminal process.",
        },
        Method::ProcessResizePty => MethodDoc {
            wire_name: "process.resize_pty",
            category: "PTY",
            params: &["pid", "rows", "cols"],
            description: "Resize a pseudo-terminal.",
        },
        Method::ProcessKillPty => MethodDoc {
            wire_name: "process.kill_pty",
            category: "PTY",
            params: &["pid", "signal?", "group?"],
            description: "Signal a PTY's leader group or foreground job.",
        },
        Method::ProcessClosePty => MethodDoc {
            wire_name: "process.close_pty",
            category: "PTY",
            params: &["pid"],
            description: "Close a pseudo-terminal process and discard buffered output.",
        },
        Method::SystemPing => MethodDoc {
            wire_name: "system.ping",
            category: "System",
            params: &[],
            description: "Lightweight liveness probe using reserved control capacity.",
        },
        Method::SystemInfo => MethodDoc {
            wire_name: "system.info",
            category: "System",
            params: &[],
            description: "Return server protocol revision, capabilities, and remote host info.",
        },
        Method::SystemStatvfs => MethodDoc {
            wire_name: "system.statvfs",
            category: "System",
            params: &["path"],
            description: "Return filesystem capacity information (~statvfs~).",
        },
        Method::SystemUserHome => MethodDoc {
            wire_name: "system.user_home",
            category: "System",
            params: &["user"],
            description: "Resolve a user's home directory natively via ~getpwnam_r~.",
        },
        Method::CommandsRunParallel => MethodDoc {
            wire_name: "commands.run_parallel",
            category: "Batch & High-Level",
            params: &["commands", "env?", "clear_env?", "timeout_ms?"],
            description: "Run multiple commands concurrently and return their results in order.",
        },
        Method::AncestorsScan => MethodDoc {
            wire_name: "ancestors.scan",
            category: "Batch & High-Level",
            params: &["directory", "markers", "max_depth?"],
            description: "Scan ancestor directories for marker files in a single RPC.",
        },
        Method::HighlevelTestFilesInDir => MethodDoc {
            wire_name: "highlevel.test_files_in_dir",
            category: "Batch & High-Level",
            params: &["directory", "names"],
            description: "Return readable regular files from a directory matching a list of names.",
        },
        Method::HighlevelLocateDominatingFileMulti => MethodDoc {
            wire_name: "highlevel.locate_dominating_file_multi",
            category: "Batch & High-Level",
            params: &["file", "names"],
            description: "Locate marker files in the nearest matching ancestor directory.",
        },
        Method::HighlevelDirLocalsFindFileCacheUpdate => MethodDoc {
            wire_name: "highlevel.dir_locals_find_file_cache_update",
            category: "Batch & High-Level",
            params: &["file", "names", "cache_dirs?"],
            description: "Resolve directory-local file cache data in a single RPC.",
        },
        Method::WatchAdd => MethodDoc {
            wire_name: "watch.add",
            category: "Watch",
            params: &["path", "recursive?", "nofollow?", "nofollow_if_symlink?"],
            description: "Start watching a directory or symlink for changes (~inotify~ / ~kqueue~).",
        },
        Method::WatchRemove => MethodDoc {
            wire_name: "watch.remove",
            category: "Watch",
            params: &["path", "nofollow?"],
            description: "Stop watching a directory or symlink.",
        },
        Method::Unknown(_) => return None,
    };
    Some(doc)
}

fn task_class_label(method: &Method) -> &'static str {
    if method.is_control() {
        "Control"
    } else if method.is_process_write() {
        "ProcessWrite"
    } else {
        "General"
    }
}

fn render_rpc_methods_org() -> String {
    let mut out = String::new();
    writeln!(out, "#+TITLE: TRAMP-RPC Server Methods").unwrap();
    writeln!(out, "#+OPTIONS: toc:2").unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "# Auto-generated by `UPDATE_RPC_DOCS=1 cargo test rpc_methods_doc_is_up_to_date` — do not edit by hand."
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "Current protocol revision: ={PROTOCOL_REVISION}= (=PROTOCOL_REVISION= in =server/src/protocol.rs=)."
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "All requests use length-prefixed MessagePack frames (=<4-byte big-endian length><payload>=)"
    )
    .unwrap();
    writeln!(
        out,
        "over SSH stdin/stdout.  Optional parameters are marked with =?=.  Task classes:"
    )
    .unwrap();
    writeln!(
        out,
        "- *General*: regular request pool (bounded concurrency + memory budget)."
    )
    .unwrap();
    writeln!(
        out,
        "- *Control*: reserved control slots so probes and process signals cannot starve behind heavy I/O."
    )
    .unwrap();
    writeln!(
        out,
        "- *ProcessWrite*: dedicated pool for pipe/PTY stdin writes that may block on slow consumers."
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(out, "* RPC Methods").unwrap();
    writeln!(out).unwrap();

    let mut seen_wires = HashSet::new();
    let mut categories: Vec<&'static str> = Vec::new();
    let mut entries: Vec<(MethodDoc, &'static str)> = Vec::new();

    for method in ALL_METHODS {
        let doc = method_doc(method).expect("ALL_METHODS must not contain Unknown");
        assert!(
            seen_wires.insert(doc.wire_name),
            "duplicate method in ALL_METHODS: {}",
            doc.wire_name
        );
        assert_eq!(
            Method::parse(doc.wire_name),
            *method,
            "wire name {:?} does not round-trip via Method::parse",
            doc.wire_name
        );
        if !categories.contains(&doc.category) {
            categories.push(doc.category);
        }
        entries.push((doc, task_class_label(method)));
    }

    for category in categories {
        writeln!(out, "** {category}").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "| Method | Class | Parameters | Description |").unwrap();
        writeln!(out, "|--------+-------+------------+-------------|").unwrap();
        for (doc, class) in entries.iter().filter(|(d, _)| d.category == category) {
            let params = if doc.params.is_empty() {
                "(none)".to_string()
            } else {
                doc.params
                    .iter()
                    .map(|p| format!("~{p}~"))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            writeln!(
                out,
                "| ~{}~ | {} | {} | {} |",
                doc.wire_name, class, params, doc.description
            )
            .unwrap();
        }
        writeln!(out).unwrap();
    }

    writeln!(out, "* Server-Pushed Notifications").unwrap();
    writeln!(out).unwrap();
    writeln!(
        out,
        "Notifications carry no request =id= and are pushed asynchronously by the server:"
    )
    .unwrap();
    writeln!(out).unwrap();
    writeln!(out, "| Notification | Description |").unwrap();
    writeln!(out, "|--------------+-------------|").unwrap();
    for (name, desc) in NOTIFICATIONS {
        writeln!(out, "| ~{name}~ | {desc} |").unwrap();
    }

    out
}

#[test]
fn rpc_methods_doc_is_up_to_date() {
    let doc_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../doc/RPC_METHODS.org");
    let generated = render_rpc_methods_org();

    if std::env::var_os("UPDATE_RPC_DOCS").is_some() {
        std::fs::write(&doc_path, &generated)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", doc_path.display()));
        return;
    }

    // Skip filesystem comparison only if built from a filtered source tree without `doc/`.
    if !doc_path.parent().is_some_and(Path::exists) {
        return;
    }

    let existing = std::fs::read_to_string(&doc_path).unwrap_or_default();
    assert_eq!(
        existing, generated,
        "doc/RPC_METHODS.org is out of date. Run:\n  UPDATE_RPC_DOCS=1 cargo test rpc_methods_doc_is_up_to_date"
    );
}
