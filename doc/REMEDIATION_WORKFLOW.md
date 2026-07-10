<!-- markdownlint-disable MD013 -->

# TRAMP-RPC review remediation workflow

This workflow addresses the Lisp and Rust review findings in small, serial milestones. Only one worker edits the active worktree at a time. Every milestone has an implementation pass, independent review/test passes, and a stabilization pass before the next milestone begins.

Run it with:

```text
/run-chain tramp-rpc-remediation -- Address all approved review findings. Stop for unapproved API or architecture decisions.
```

## Operating rules

- Use Jujutsu (`jj`) for status and diffs; do not mutate the repository with Git.
- Do not mix unrelated cleanup into correctness milestones.
- Add a regression test before or with every logic fix.
- Preserve protocol compatibility unless a versioned protocol change is explicitly approved.
- Keep one writer at a time. Parallel steps are review/test-only.
- A milestone is not complete merely because focused tests pass: run the milestone test ladder below.
- If the Rust toolchain/linker or Tramp test environment is broken, stop and repair or clearly escalate the environment problem. Do not report skipped or zero-test runs as success.
- Remote server tests must deploy to a temporary remote directory, as required by `AGENTS.md`.

## Test ladder for every milestone

Workers and validators should run the applicable focused tests first, then the broad checks:

1. New regression tests for the milestone.
2. `cargo fmt --all -- --check`.
3. `cargo test --workspace --all-targets`.
4. `cargo clippy --workspace --all-targets --all-features -- -D warnings`.
5. Byte-compile every Lisp file with `byte-compile-error-on-warn` using the command in `AGENTS.md`.
6. `./test/run-autoload-tests.sh`.
7. `./test/run-tests.sh --protocol`; confirm the output reports a non-zero test count.
8. `./test/run-tests.sh --mock`; confirm required tests execute rather than skip because an obsolete Tramp was loaded.
9. For process, PTY, deployment, or cross-language lifecycle milestones: run the relevant remote ERT tests when the configured host is available, using a temporary `tramp-rpc-deploy-remote-directory`.

A validator must record exact commands, exit codes, executed/skipped test counts, and environmental blockers.

## Milestone 0 — Make validation trustworthy

- Fix `test/run-tests.sh --protocol` so it selects the eight protocol tests by name instead of a nonexistent tag.
- Remove or update mock tests that call deleted implementation helpers.
- Isolate global caches/state so the full mock suite is order-independent.
- Make the test runner load a supported Tramp or fail clearly instead of succeeding with most tests skipped.
- Establish a clean baseline before functional changes.

Preferred model: Terra for implementation and test plumbing; Luna for review.

## Milestone 1 — File and RPC semantics

- Preserve suffix data for integer-offset `write-region` by using the existing Rust `file.write` offset support.
- Correct `directory-files`/`directory-files-and-attributes` `COUNT` semantics and validation.
- Correct root executable-bit handling.
- Make pipelined RPC timeouts/process death signal errors rather than returning nil results.
- Convert only confirmed process spawn/not-found failures to exit code 127.
- Fix no-overwrite rename behavior for dangling symlinks and return structured `EEXIST`.
- Replace the fixed 64-group limit with dynamic `getgroups` sizing.
- Bound client-controlled read sizes.

Preferred model: Terra for the bounded implementation; Luna for semantic review.

## Milestone 2 — Synchronous Rust process and server lifecycle

- Give `process.run` and `commands.run_parallel` a null stdin unless input is explicitly supplied.
- Write synchronous process stdin concurrently with stdout/stderr collection to prevent pipe deadlocks.
- Reap completed `JoinSet` tasks during the connection lifetime and bound in-flight work if necessary.
- Distinguish malformed MessagePack from structurally invalid RPC requests.
- Remove normally exited managed processes after final output/EOF.
- Reap killed pipe and PTY children instead of leaving zombies.

Preferred model: Luna for implementation and correctness review; Terra for test execution.

## Milestone 3 — Async pipe transport, cleanup, and coding

- Key write queues by connection identity plus remote PID.
- Prevent callbacks from recreating queues after process cleanup.
- Make write-queue drain timeout explicit rather than silently closing stdin with pending data.
- Install an RPC connection sentinel that cleans callbacks, pending responses, queues, pipe relays, and PTYs after transport death.
- Ensure explicit disconnect kills/reaps remote async processes.
- Preserve raw process bytes across RPC and honor the configured Emacs process coding systems exactly once.
- Make writes to an already-closed remote stdin fail accurately.

Preferred model: Luna for both implementation and primary review; Terra for regression execution.

## Milestone 4 — PTY correctness and concurrency

- Serialize PTY writes per process and guarantee complete writes in order.
- Never hold the global PTY registry lock across an await.
- Continue reading after child exit until PTY EOF so buffered output is not truncated.
- Treat PTY read RPC errors as terminal errors rather than retrying forever.
- Remove raw-fd lifetime races between polling and concurrent close.
- Reap PTY children on kill/close.

Preferred model: Luna for implementation and concurrency review; Terra for stress/regression tests.

## Milestone 5 — Deployment, documentation, and low-risk consolidation

- Require checksum verification for downloaded release binaries or fall back to a source build.
- Check the result of remote `chmod && mv` before reporting deployment success.
- Update the README RPC method table to match the actual dispatcher.
- Consolidate duplicated RPC wait loops, command setup, process send routing, file-type conversion, and NSS lookup only where tests demonstrate behavior is preserved.
- Remove genuinely unused private helpers.

Preferred model: Terra for mechanical implementation/docs; Luna for final API and regression review.

## Final acceptance

- All focused and broad local tests pass with non-zero executed counts.
- Rust format, test, and Clippy checks pass.
- All Lisp files byte-compile with warnings as errors.
- Relevant remote process/PTY/deployment tests pass when the configured remote target is available.
- Fresh Luna reviewers report no correctness blockers.
- Terra independently reruns the final test matrix and reports exact evidence.
- `jj diff` contains only remediation changes, tests, and necessary documentation.
