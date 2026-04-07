# TRAMP RPC

Always read README.org for details about the project

# Building the Rust server
The script to build the rust server is located at: scripts/build-all.sh

If building for a target other than linux, refer to that script for options.

# Config settings

The settings for this setup are located in .config

always read that file. It will contain:

- remote target
- remote OS
- default tramp method
- tramp source code directory
- any other relevant instructions or information

# Testing updates

NEVER use emacsclient. That will interfer with the users configuration

Always use `emacs -Q --batch --eval <lisp commands>`

When testing updates to the rust server, always copy it to a temporary location and set `tramp-rpc-deploy-remote-directory` to the temporary path so that testing does not interfere with existing sessions.
