# TRAMP RPC

for details about the project see @README.org

# Building the Rust server
The script to build the rust server is located at: scripts/build-all.sh

If building for a target other than linux, refer to that script for options.

# Config settings

The settings for this setup are located in `.config`

@.config

This can include:

- remote target
- remote OS
- default tramp method
- tramp source code directory
- msgpack package path
- any other relevant instructions or information

# Testing updates

NEVER use emacsclient. That will interfer with the users configuration

Always use `emacs -Q --batch --eval <lisp commands>`

When testing updates to the rust server, always copy it to a temporary location and set `tramp-rpc-deploy-remote-directory` to the temporary path so that testing does not interfere with existing sessions.

## Test Example

running all tests that start with `tramp-rpc-test` in tramp-rpc-tests.el
```sh
  emacs -Q --batch -l test/tramp-rpc-tests.el \
    --eval '(add-to-list (quote load-path) "/path/to/msgpack")' \
    --eval '(setq tramp-rpc-test-source "/path/to/tramp/source")' \
    --eval '(setq tramp-rpc-test-host "remote-host")' \
    --eval '(ert-run-tests-batch-and-exit "^tramp-rpc-test")'
```

## Benchmark Example

running "file-exists" and "file-read" benchmarks with rpc
```sh
  emacs -Q --batch -l benchmark/benchmark.el \
    --eval '(add-to-list (quote load-path) "/path/to/msgpack")' \
    --eval '(add-to-list (quote load-path) (expand-file-name "lisp" default-directory))' \
    --eval '(require (quote tramp-rpc))' \
    --eval '(setq tramp-rpc-benchmark-host "remote-host")' \
    --eval '(tramp-rpc-benchmark-run-subset (quote ("file-exists" "file-read")) (quote ("rpc")))'
```

