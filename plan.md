# Implementation Plan

## Goal

Execute the approved remediation workflow in six serial milestones, making the test gates trustworthy first, then fixing Lisp/Rust semantics, process lifecycle, transport/PTY behavior, and deployment/documentation without changing the MessagePack-RPC contract.

## Current-code validation

- `test/run-tests.sh --protocol` currently selects `(tag tramp-rpc-mock-test)`, but the protocol ERTs are named `tramp-rpc-mock-test-protocol-*` and do not carry that tag. This can produce a zero-test false success; Milestone 0 is a hard prerequisite.
- The Rust server already has `file.write` offset support (`server/src/handlers/io.rs`), but `tramp-rpc-handle-write-region` (`lisp/tramp-rpc.el`) implements integer `APPEND` by reading and overwriting only the prefix, dropping the suffix.
- Directory handlers accept no count parameter; both Lisp handlers apply incomplete `COUNT` logic (`count > 0` only for `directory-files`, unconditional `seq-take` for attributes). Negative/zero/count validation must be specified by Emacs behavior and tested.
- `file-attributes` exposes full `st_mode`; `tramp-rpc-handle-file-executable-p` handles owner/group/world bits but does not make the root-any-execute rule explicit.
- Pipelined response collection returns `nil` for missing IDs after timeout, and synchronous wait loops collapse process death into timeout. Async write callbacks can recreate a removed queue.
- `process.run` and `commands.run_parallel` use piped streams in paths that can deadlock or inherit RPC stdin; `main.rs` accumulates `JoinSet` tasks until connection EOF. Managed process and PTY maps retain normally exited entries, while SIGKILL/PTY close paths do not explicitly reap.
- `process_request` maps both malformed MessagePack and a structurally invalid request through the same deserialization failure path. The dispatcher itself is the source of truth for the README method table; README currently advertises methods such as `file.stat_batch` and `file.executable` that are not dispatcher entries.
- `tramp-rpc--process-write-queues` is keyed only by remote PID; PTY writes are one-shot `libc::write` calls under the global PTY map lock across `await`; PTY reads treat errors as retryable/no-data; there is no connection-process sentinel that purges all async state on transport death.
- Download checksum verification is optional when the checksum download fails, and deployment issues `chmod && mv` without checking the command result.

## Test command contract

Every command below must report a non-zero executed test count. For ERT, record the `Ran N tests` line and the skipped count; `0 tests`, all-skipped, or a skip caused by loading an obsolete/unsupported Tramp is a failure/blocker, never success. For Cargo, record the test count and exit status. Remote commands must use a fresh temporary `tramp-rpc-deploy-remote-directory`; do not use `emacsclient`.

`BROAD` (run after each milestone, in this order):

```sh
cargo fmt --all -- --check
cargo test --workspace --all-targets
cargo clippy --workspace --all-targets --all-features -- -D warnings
rm -f lisp/*.elc && emacs -Q --batch --eval "(require 'package)" --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" --eval "(package-initialize)" --eval "(setq byte-compile-error-on-warn t)" --eval "(push \"$(pwd)/lisp\" load-path)" -f batch-byte-compile lisp/*.el
./test/run-autoload-tests.sh
./test/run-tests.sh --protocol
./test/run-tests.sh --mock
```

For milestones marked remote, add the focused remote ERT command and `./test/run-tests.sh --remote`; run upstream `./test/run-tests.sh --upstream` when the milestone changes TRAMP process/PTY/deployment behavior. If the configured host is unavailable, stop and record an environment blocker rather than converting the run to a skip.

## Tasks

1. **Milestone 0 — Make validation trustworthy**
   - Files: `test/run-tests.sh`, `test/tramp-rpc-mock-tests.el`, and the cache/setup paths in `lisp/tramp-rpc.el`/`lisp/tramp-rpc-process.el`.
   - Changes: select the eight protocol tests by the exact name prefix `^tramp-rpc-mock-test-protocol`; remove/update tests calling deleted private helpers; reset or isolate global connection, callback, queue, and cache state in test setup/teardown; make unsupported Tramp detection fail clearly and assert that selected ERTs actually execute.
   - Focused validation: `emacs -Q --batch -l test/tramp-rpc-mock-tests.el --eval '(ert-run-tests-batch-and-exit "^tramp-rpc-mock-test-protocol")'` and `./test/run-tests.sh --mock`; verify the protocol command reports eight executed tests and no obsolete-helper failures.
   - Broad validation: `BROAD`.
   - Acceptance: protocol selection cannot return success with zero tests; a fresh Emacs runs supported Tramp or exits non-zero; mock tests pass independently of test order.

2. **Milestone 1 — File and RPC semantics**
   - Files: `lisp/tramp-rpc.el`, `server/src/handlers/io.rs`, `server/src/handlers/mod.rs`, `server/src/handlers/dir.rs`, `test/tramp-rpc-tests.el`, and Rust unit tests in the touched modules.
   - Changes: send integer `write-region` offsets directly to existing `file.write` so suffix bytes survive; implement Emacs-compatible directory `COUNT` semantics and reject invalid values consistently for both listing handlers; fix root executable-bit evaluation; make pipelined missing responses and dead RPC processes signal errors; map only confirmed spawn/not-found failures to exit 127; use no-follow destination existence checks for dangling symlinks and preserve structured `EEXIST`; replace fixed 64-group allocation with `getgroups` sizing/retry; cap client-controlled file/process read sizes before allocation.
   - Focused tests to add/run: `tramp-rpc-test05-write-region-offset-preserves-suffix`, `tramp-rpc-test07-directory-files-count`, `tramp-rpc-test07-directory-files-and-attributes-count`, `tramp-rpc-test04-file-executable-root`, `tramp-rpc-test06-rename-dangling-symlink-no-overwrite`, `tramp-rpc-test00-remote-groups-dynamic-sizing`, `tramp-rpc-test-pipelined-timeout-signals-error`, plus Rust tests named `write_offset_preserves_suffix`, `rename_no_overwrite_rejects_dangling_symlink`, `groups_retry_after_erange`, and `read_size_limit_rejects_oversized_request`.
   - Focused command: `emacs -Q --batch -l test/tramp-rpc-tests.el --eval '(ert-run-tests-batch-and-exit "\(tramp-rpc-test05-write-region-offset-preserves-suffix\|tramp-rpc-test07-directory-files-count\|tramp-rpc-test07-directory-files-and-attributes-count\|tramp-rpc-test04-file-executable-root\|tramp-rpc-test06-rename-dangling-symlink-no-overwrite\|tramp-rpc-test00-remote-groups-dynamic-sizing\|tramp-rpc-test-pipelined-timeout-signals-error\)")'`; run `cargo test -p tramp-rpc-server write_offset_preserves_suffix rename_no_overwrite_rejects_dangling_symlink groups_retry_after_erange read_size_limit_rejects_oversized_request` as separate filters if Cargo accepts only one filter. Configure the remote host and temporary deployment directory first.
   - Broad validation: `BROAD` plus `./test/run-tests.sh --remote` (and upstream tests if process-file exit handling changed).
   - Acceptance: all existing file/directory/rename/process-file tests remain passing; no protocol field or method name changes; errors retain existing codes and add structured errno rather than requiring message parsing.

3. **Milestone 2 — Synchronous Rust process and server lifecycle**
   - Files: `server/src/handlers/process.rs`, `server/src/handlers/commands.rs`, `server/src/main.rs`, `server/src/protocol.rs`, and corresponding mock/Rust tests.
   - Changes: make `process.run` and `commands.run_parallel` inherit null stdin unless input is explicitly provided; concurrently write synchronous stdin while collecting stdout/stderr; reap completed `JoinSet` entries during connection lifetime and impose a bounded in-flight policy; distinguish MessagePack parse failures from structurally invalid RPC requests while preserving JSON-RPC-style codes; remove managed processes after final output/EOF; kill and `waitpid` pipe/PTY children so all exit paths reap.
   - Dependencies: Milestone 0 test gating and Milestone 1 exit/error semantics must be stable first. The stdin and response/error changes must be implemented before lifecycle cleanup tests so failures are attributable.
   - Focused tests: existing `tramp-rpc-mock-test-server-process-run`, `tramp-rpc-mock-test-server-process-signal-exit`, Rust `process_run_large_stdin_does_not_deadlock`, `process_run_without_stdin_does_not_consume_rpc_input`, `commands_run_parallel_without_stdin`, `malformed_msgpack_is_parse_error`, `structurally_invalid_request_is_invalid_request`, `managed_process_removed_after_eof`, and `killed_pipe_child_is_reaped`.
   - Focused commands: `emacs -Q --batch -l test/tramp-rpc-mock-tests.el --eval '(ert-run-tests-batch-and-exit "^tramp-rpc-mock-test-server-process")'`; `cargo test -p tramp-rpc-server process_run_large_stdin_does_not_deadlock` (repeat for each named Rust regression); then the configured remote process selector `tramp-rpc-test10-.*`.
   - Broad validation: `BROAD` plus `./test/run-tests.sh --remote` and `./test/run-tests.sh --upstream`; the remote process ladder must use temporary deployment.
   - Acceptance: large stdin/output completes without a hang, malformed and invalid-request codes are stable, process maps do not retain completed children, and no zombie is left after kill/close.

4. **Milestone 3 — Async pipe transport, cleanup, and coding**
   - Files: `lisp/tramp-rpc.el`, `lisp/tramp-rpc-process.el`, `lisp/tramp-rpc-advice.el`, `server/src/handlers/process.rs` where closed-stdin behavior is fixed, and mock/remote tests.
   - Changes: key queues by connection identity plus PID; make callbacks verify queue/process liveness before mutating state; make queue-drain timeout explicit and fail rather than closing stdin with pending data; install a connection sentinel that removes callbacks, pending responses, queues, pipe relays, and PTYs on transport death; make disconnect kill/reap remote async processes; keep RPC output/input as raw bytes and apply the requested Emacs coding systems exactly once; return an error when writing closed/missing stdin instead of reporting success.
   - Dependencies: Milestone 2 must establish reliable process exit/reap behavior; queue cleanup must be in place before transport-death and disconnect tests; coding changes must not be mixed with MessagePack framing changes.
   - Focused tests: `tramp-rpc-test14-make-process`, `tramp-rpc-test14-make-process-drains-output-after-exit`, `tramp-rpc-test13d-async-read-rpc-error-exits-process`, new `tramp-rpc-test-process-write-queues-isolate-connections`, `tramp-rpc-test-connection-death-cleans-async-state`, `tramp-rpc-test-coding-system-roundtrip-exactly-once`, `tramp-rpc-test-write-closed-stdin-signals-error`, plus Rust `write_closed_stdin_is_process_error`.
   - Focused command: `emacs -Q --batch -l test/tramp-rpc-tests.el --eval '(ert-run-tests-batch-and-exit "\(tramp-rpc-test14-\|tramp-rpc-test13d-async-read-rpc-error-exits-process\|tramp-rpc-test-process-write-queues-isolate-connections\|tramp-rpc-test-connection-death-cleans-async-state\|tramp-rpc-test-coding-system-roundtrip-exactly-once\|tramp-rpc-test-write-closed-stdin-signals-error\)")'`; run the mock unit selector for queue/sentinel tests and the configured remote process selector.
   - Broad validation: `BROAD` plus `./test/run-tests.sh --remote` and `./test/run-tests.sh --upstream`.
   - Acceptance: no callback can resurrect state after cleanup, queued writes are ordered and either fully acknowledged or explicitly failed, raw bytes are not UTF-8-decoded twice, and a dead RPC transport leaves no tracked client or remote process.

5. **Milestone 4 — PTY correctness and concurrency**
   - Files: `server/src/handlers/process.rs`, `lisp/tramp-rpc-process.el`, `lisp/tramp-rpc-advice.el`, and PTY Rust/remote tests.
   - Changes: serialize PTY writes per process and loop until all bytes are written in order; never hold the global PTY registry mutex over an await; read until PTY EOF after child exit so buffered bytes are delivered; classify PTY read errors as terminal; remove raw-fd polling/close lifetime races; kill and reap PTY children on kill/close and make client cleanup idempotent.
   - Dependencies: Milestone 3 connection sentinel and async cleanup must land first because PTY callbacks/queues share that lifecycle. Do not optimize or consolidate PTY code until stress tests are green.
   - Focused tests: Rust `pty_write_is_complete_and_ordered`, `pty_write_does_not_hold_registry_lock_across_await`, `pty_read_drains_buffered_output_after_exit`, `pty_read_error_is_terminal`, `pty_fd_close_race_is_safe`, `pty_kill_reaps_child`; remote ERT regressions `tramp-rpc-test-pty-write-order`, `tramp-rpc-test-pty-output-after-exit`, and `tramp-rpc-test-pty-kill-reaps`.
   - Focused commands: `cargo test -p tramp-rpc-server pty_` (must report non-zero executed tests and no ignored-only result); configured remote ERT selector `tramp-rpc-test-pty-` and existing `tramp-rpc-test20-` tests.
   - Broad validation: `BROAD` plus `./test/run-tests.sh --remote` and `./test/run-tests.sh --upstream`; run repeated PTY stress tests when the host supports PTYs.
   - Acceptance: complete ordered input and all buffered output survive concurrent exit/close; no lock spans await; PTY errors terminate rather than spin; child PIDs are reaped.

6. **Milestone 5 — Deployment, documentation, and low-risk consolidation**
   - Files: `lisp/tramp-rpc-deploy.el`, `README.org`, duplicated wait/setup/routing/conversion/NSS helpers identified by review, and deployment/mock/remote tests.
   - Changes: require a verified release checksum and fall back to source build when verification material is unavailable or invalid; check the result of remote `chmod && mv` and report failure; update the README method table from `server/src/handlers/mod.rs`; consolidate only duplicate logic covered by passing behavior tests; remove only demonstrably unused private helpers.
   - Dependencies: all functional milestones and the final byte-compile/test ladder must be green before consolidation. Checksum verification precedes transfer promotion; documentation must be updated after dispatcher review.
   - Focused tests: new `tramp-rpc-mock-test-deploy-checksum-required`, `tramp-rpc-mock-test-deploy-chmod-mv-failure`, `tramp-rpc-mock-test-deploy-method-table`, plus remote deployment using a temporary `tramp-rpc-deploy-remote-directory`; run a static method-table comparison against dispatcher entries.
   - Focused commands: `emacs -Q --batch -l test/tramp-rpc-mock-tests.el --eval '(ert-run-tests-batch-and-exit "^tramp-rpc-mock-test-deploy-")'`; configured remote deployment ERT selector; `grep`/review comparison of README RPC methods against `dispatch_inner`.
   - Broad validation: `BROAD` plus remote deployment/process/PTY tests and `./test/run-tests.sh --upstream` when available.
   - Acceptance: corrupted or unverifiable release artifacts never execute, failed promotion is reported, README lists only actual dispatcher methods, and consolidation produces no behavior or byte-compile regressions.

## Ordering dependencies

- M0 is mandatory: every later gate depends on non-zero, correctly selected tests and order-independent state.
- M1 precedes M2 because it establishes stable error/exit semantics and bounded read behavior used by process tests; both are otherwise separate implementation areas.
- M2 precedes M3: async transport cleanup relies on reliable server-side process exit/reap and request classification.
- M3 precedes M4: PTY cleanup and callback/queue behavior depend on connection-death cleanup and exact byte/coding rules.
- M5 is last: deployment and README verification depend on the final dispatcher and lifecycle API; consolidation is permitted only after behavior tests.
- Within every milestone: implementation and regression test are one writer step, then independent review/tests, then stabilization and the full `BROAD` ladder. No parallel writers.

## Protocol-compatibility guardrails

- Preserve four-byte big-endian framing, MessagePack binary values, request `version = "2.0"`, method names, parameter names (`offset`, `append`, `max_bytes`, etc.), and existing success fields.
- Preserve existing RPC error codes. Add structured `data.os_errno` where already supported; do not make clients parse changed prose. `EEXIST` must remain an IO error with a structured errno, not a new unapproved protocol version.
- Keep raw process/file bytes as MessagePack bin values. Emacs coding systems belong at the client process boundary and must be applied once, not by changing server payload encoding.
- Keep PTY process IDs and response fields compatible; lifecycle cleanup must not turn a previously valid response into a new method or version.
- Any need to rename a method/field, change framing, add a protocol version, or alter public error semantics is outside this approved workflow and must stop for an explicit architecture/API decision.

## Files to Modify

- `test/run-tests.sh` - reliable protocol selection and runner failure behavior.
- `test/tramp-rpc-mock-tests.el` - mock regressions, state isolation, and deployment/protocol checks.
- `test/tramp-rpc-tests.el` - remote file/process/PTY regressions.
- `lisp/tramp-rpc.el` - file semantics, RPC wait/error handling, connection sentinel/state cleanup.
- `lisp/tramp-rpc-process.el` - async queues, coding, pipe/PTY cleanup and transport lifecycle.
- `lisp/tramp-rpc-advice.el` - process routing and accurate closed/write/PTY behavior.
- `lisp/tramp-rpc-deploy.el` - mandatory checksum and checked promotion.
- `server/src/handlers/io.rs` - bounded file IO, offset writes, rename errors.
- `server/src/handlers/dir.rs` - directory listing contract if server-side count support is required.
- `server/src/handlers/file.rs` - metadata/executable-related helpers only if the review confirms server changes are needed.
- `server/src/handlers/mod.rs` - dynamic group lookup and dispatcher validation.
- `server/src/handlers/process.rs` - synchronous/async/PTY process lifecycle and IO.
- `server/src/handlers/commands.rs` - synchronous parallel command stdin/deadlock handling.
- `server/src/main.rs` and `server/src/protocol.rs` - task reaping and parse-vs-invalid-request classification.
- `README.org` - actual RPC dispatcher method table.

## New Files

- None expected. Add regressions to the existing test files; do not create a framework or a new abstraction for one implementation.

## Risks

- `context.md` and `.config` are absent in this worktree, although the request and `AGENTS.md` reference them. The review therefore cannot name the approved remote host, Tramp source checkout, or msgpack path; remote execution must resolve these before proceeding.
- Emacs/Tramp version differences can make tests skip or change `COUNT`, process, and PTY semantics. A skip or zero-test run is an environment failure, not evidence of correctness.
- `getgroups` sizing and NSS behavior are platform-specific (Linux/macOS); validate both where configured and avoid assuming a fixed supplementary-group limit.
- Concurrent stdin/output, `JoinSet` reaping, PTY fd ownership, and callback cleanup are race-sensitive; use deterministic Rust tests plus repeated remote PTY/process tests before consolidation.
- Release checksum file formats and availability vary by release; verify the exact artifact name and source-build fallback without weakening the mandatory verification requirement.
- Do not expand into unrelated cleanup: consolidation/removal in M5 is allowed only for duplicates or private helpers named by the approved findings and protected by existing behavior tests.

## Dependencies

- M0 -> M1 -> M2 -> M3 -> M4 -> M5.
- Every implementation change depends on its focused regression test; every milestone completion depends on the full `BROAD` ladder and an independent review/stabilization pass.
- Remote process/PTY/deployment checks depend on a configured reachable host and temporary remote deployment directory; Cargo checks depend on a working Rust linker/toolchain; Lisp checks depend on supported Tramp and msgpack.

## Required evidence

- **manual-notes:** Record exact commands, exit codes, ERT `Ran N tests` and skipped counts, Cargo test counts, toolchain/Tramp versions, remote host availability, and temporary deployment path for each milestone. Record any intentional test skip with its reason and never call it a pass.
- **residual-risks:** Carry forward only verified environment blockers, platform-specific untested paths, or review findings requiring an API/architecture decision. A skipped/zero-test command must remain a blocker until rerun successfully.

## Acceptance report

```acceptance-report
{
  "criteriaSatisfied": [
    {
      "id": "criterion-1",
      "status": "satisfied",
      "evidence": "All six workflow milestones are mapped in order, with file-level tasks, intra-milestone dependencies, and explicit M0->M1->M2->M3->M4->M5 gating."
    },
    {
      "id": "criterion-2",
      "status": "satisfied",
      "evidence": "Each milestone has focused selectors/commands and the shared exact Rust/Elisp/autoload/protocol/mock broad ladder; the plan explicitly rejects zero-test and skipped-test false positives."
    },
    {
      "id": "criterion-3",
      "status": "satisfied",
      "evidence": "Manual-notes and residual-risks evidence requirements are explicit, and protocol compatibility risks plus stop conditions are recorded."
    }
  ],
  "changedFiles": [
    "plan.md"
  ],
  "testsAddedOrUpdated": [],
  "commandsRun": [],
  "validationOutput": [
    "Static review completed against doc/REMEDIATION_WORKFLOW.md, AGENTS.md, README.org, current Lisp/Rust handlers, and existing test runners/tests.",
    "No project tests were executed in this planning-only pass; execution commands and non-zero-count checks are specified above."
  ],
  "residualRisks": [
    "context.md and .config are absent, so remote target and Tramp/msgpack paths require environment resolution before remote validation.",
    "No remote, Cargo, or Emacs command output is available from this planning-only pass."
  ],
  "noStagedFiles": true,
  "diffSummary": "Added only the requested execution map and acceptance report in plan.md; no project/source files were edited.",
  "reviewFindings": [
    "blocker: test/run-tests.sh protocol selector can execute zero tests while succeeding.",
    "finding: lisp/tramp-rpc.el integer write-region offset path drops the existing suffix.",
    "finding: server process/PTY cleanup and transport queue paths have lifecycle, deadlock, and race risks listed by milestones.",
    "finding: README.org RPC method table is not aligned with server/src/handlers/mod.rs dispatcher.",
    "no unapproved API or architecture decision identified; stop only if implementation requires one."
  ],
  "manualNotes": "The workflow is approved and should be executed serially. Use a temporary remote deployment directory, record exact counts/exit codes, and treat all zero-test or environment-induced skips as failures/blockers rather than successes."
}
```
