#!/usr/bin/env python3
"""
TRAMP-RPC Server - Async JSON-RPC 2.0 server for remote file operations.

This server provides a high-performance alternative to shell-based TRAMP
operations by exposing file system and process operations via JSON-RPC 2.0
over stdin/stdout.

Requires Python 3.7+
"""

import asyncio
import base64
import errno
import fcntl
import grp
import json
import os
import pty
import pwd
import select
import shutil
import signal
import socket
import stat
import struct
import sys
import termios
import tty
from typing import Any, Dict, List, Optional, Tuple, Union

VERSION = "0.1.0"

# ============================================================================
# JSON-RPC 2.0 Error Codes
# ============================================================================

PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603

# Custom error codes
FILE_NOT_FOUND = -32001
PERMISSION_DENIED = -32002
IO_ERROR = -32003
PROCESS_ERROR = -32004

# ============================================================================
# Global State
# ============================================================================

# Regular subprocess management
processes: Dict[int, dict] = {}  # our_pid -> {process, cmd}
next_process_pid = 1

# PTY process management
pty_processes: Dict[
    int, dict
] = {}  # our_pid -> {master_fd, child_pid, cmd, exit_status}
next_pty_pid = 10000

# Lock for state modifications (for concurrent access)
state_lock = asyncio.Lock()


# ============================================================================
# Protocol Helpers
# ============================================================================


def success_response(request_id: Any, result: Any) -> dict:
    """Create a successful JSON-RPC response."""
    return {"jsonrpc": "2.0", "id": request_id, "result": result}


def error_response(request_id: Any, code: int, message: str, data: Any = None) -> dict:
    """Create an error JSON-RPC response."""
    error = {"code": code, "message": message}
    if data is not None:
        error["data"] = data
    return {"jsonrpc": "2.0", "id": request_id, "error": error}


def map_os_error(e: OSError, path: str) -> Tuple[int, str]:
    """Map an OSError to appropriate JSON-RPC error code and message."""
    if e.errno == errno.ENOENT:
        return FILE_NOT_FOUND, f"File not found: {path}"
    elif e.errno == errno.EACCES or e.errno == errno.EPERM:
        return PERMISSION_DENIED, f"Permission denied: {path}"
    else:
        return IO_ERROR, str(e)


def smart_encode(data: bytes) -> Tuple[str, str]:
    """
    Encode bytes smartly: use raw text if valid UTF-8, otherwise base64.
    Returns (encoded_string, encoding_type).
    """
    try:
        text = data.decode("utf-8")
        return text, "text"
    except UnicodeDecodeError:
        return base64.b64encode(data).decode("ascii"), "base64"


# ============================================================================
# File Type Helpers
# ============================================================================


def get_file_type(mode: int) -> str:
    """Convert stat mode to file type string."""
    if stat.S_ISREG(mode):
        return "file"
    elif stat.S_ISDIR(mode):
        return "directory"
    elif stat.S_ISLNK(mode):
        return "symlink"
    elif stat.S_ISCHR(mode):
        return "chardevice"
    elif stat.S_ISBLK(mode):
        return "blockdevice"
    elif stat.S_ISFIFO(mode):
        return "fifo"
    elif stat.S_ISSOCK(mode):
        return "socket"
    else:
        return "unknown"


def get_user_name(uid: int) -> Optional[str]:
    """Get username from UID."""
    try:
        return pwd.getpwuid(uid).pw_name
    except (KeyError, OSError):
        return None


def get_group_name(gid: int) -> Optional[str]:
    """Get group name from GID."""
    try:
        return grp.getgrgid(gid).gr_name
    except (KeyError, OSError):
        return None


def format_file_attributes(
    st: os.stat_result, path: str, is_lstat: bool = False
) -> dict:
    """Format stat result as file attributes dict matching Rust implementation."""
    file_type = get_file_type(st.st_mode)

    attrs = {
        "type": file_type,
        "nlinks": st.st_nlink,
        "uid": st.st_uid,
        "gid": st.st_gid,
        "atime": int(st.st_atime),
        "mtime": int(st.st_mtime),
        "ctime": int(st.st_ctime),
        "size": st.st_size,
        "mode": st.st_mode,
        "inode": st.st_ino,
        "dev": st.st_dev,
    }

    # Add optional fields
    uname = get_user_name(st.st_uid)
    if uname:
        attrs["uname"] = uname

    gname = get_group_name(st.st_gid)
    if gname:
        attrs["gname"] = gname

    # Add symlink target if it's a symlink
    if file_type == "symlink":
        try:
            attrs["link_target"] = os.readlink(path)
        except OSError:
            pass

    return attrs


# ============================================================================
# Async Helper for Blocking Operations
# ============================================================================


async def run_in_thread(func, *args, **kwargs):
    """Run a blocking function in a thread pool."""
    loop = asyncio.get_event_loop()
    return await loop.run_in_executor(None, lambda: func(*args, **kwargs))


# ============================================================================
# File Metadata Handlers
# ============================================================================


async def file_stat(params: dict) -> dict:
    """Get file attributes."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    lstat = params.get("lstat", False)

    def do_stat():
        st = os.lstat(path) if lstat else os.stat(path)
        return format_file_attributes(st, path, lstat)

    return await run_in_thread(do_stat)


async def file_stat_batch(params: dict) -> List[dict]:
    """Batch stat operation - returns results for multiple paths."""
    paths = params.get("paths", [])
    lstat = params.get("lstat", False)

    async def stat_one(p: str):
        try:
            st = os.lstat(p) if lstat else os.stat(p)
            return format_file_attributes(st, p, lstat)
        except OSError as e:
            return {"error": str(e)}

    # Run all stat operations concurrently
    tasks = [stat_one(p) for p in paths]
    return await asyncio.gather(*tasks)


async def file_exists(params: dict) -> bool:
    """Check if file exists."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return await run_in_thread(os.path.exists, path)


async def file_readable(params: dict) -> bool:
    """Check if file is readable."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return await run_in_thread(os.access, path, os.R_OK)


async def file_writable(params: dict) -> bool:
    """Check if file is writable."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return await run_in_thread(os.access, path, os.W_OK)


async def file_executable(params: dict) -> bool:
    """Check if file is executable."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return await run_in_thread(os.access, path, os.X_OK)


async def file_truename(params: dict) -> str:
    """Get the true name of a file (resolve symlinks)."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return await run_in_thread(os.path.realpath, path)


async def file_newer_than(params: dict) -> bool:
    """Check if file1 is newer than file2."""
    file1 = params.get("file1")
    file2 = params.get("file2")
    if not file1 or not file2:
        raise ValueError("file1 and file2 are required")

    def compare():
        try:
            mtime1 = os.stat(file1).st_mtime
        except OSError:
            return False  # file1 doesn't exist
        try:
            mtime2 = os.stat(file2).st_mtime
        except OSError:
            return True  # file1 exists, file2 doesn't
        return mtime1 > mtime2

    return await run_in_thread(compare)


# ============================================================================
# File I/O Handlers
# ============================================================================


async def file_read(params: dict) -> dict:
    """Read file contents."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    offset = params.get("offset")
    length = params.get("length")

    def do_read():
        with open(path, "rb") as f:
            if offset is not None:
                f.seek(offset)
            if length is not None:
                content = f.read(length)
            else:
                content = f.read()
        return content

    content = await run_in_thread(do_read)
    encoded = base64.b64encode(content).decode("ascii")

    return {"content": encoded, "size": len(content)}


async def file_write(params: dict) -> dict:
    """Write file contents."""
    path = params.get("path")
    content_b64 = params.get("content")
    if not path or content_b64 is None:
        raise ValueError("path and content are required")

    content = base64.b64decode(content_b64)
    mode = params.get("mode")
    append = params.get("append", False)
    offset = params.get("offset")

    def do_write():
        if append:
            open_mode = "ab"
        elif offset is not None:
            open_mode = "r+b"
        else:
            open_mode = "wb"

        # Create file if it doesn't exist for r+b mode
        if open_mode == "r+b" and not os.path.exists(path):
            open(path, "wb").close()

        with open(path, open_mode) as f:
            if offset is not None:
                f.seek(offset)
            f.write(content)

        if mode is not None:
            os.chmod(path, mode)

        return len(content)

    written = await run_in_thread(do_write)
    return {"written": written}


async def file_copy(params: dict) -> dict:
    """Copy a file."""
    src = params.get("src")
    dest = params.get("dest")
    if not src or not dest:
        raise ValueError("src and dest are required")

    preserve = params.get("preserve", False)

    def do_copy():
        if preserve:
            shutil.copy2(src, dest)  # Preserves metadata
        else:
            shutil.copy(src, dest)
        return os.path.getsize(dest)

    copied = await run_in_thread(do_copy)
    return {"copied": copied}


async def file_rename(params: dict) -> bool:
    """Rename/move a file."""
    src = params.get("src")
    dest = params.get("dest")
    if not src or not dest:
        raise ValueError("src and dest are required")

    overwrite = params.get("overwrite", False)

    def do_rename():
        if not overwrite and os.path.exists(dest):
            raise OSError(errno.EEXIST, f"Destination already exists: {dest}")
        os.rename(src, dest)
        return True

    return await run_in_thread(do_rename)


async def file_delete(params: dict) -> bool:
    """Delete a file."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    force = params.get("force", False)

    def do_delete():
        try:
            os.remove(path)
            return True
        except FileNotFoundError:
            if force:
                return False
            raise

    return await run_in_thread(do_delete)


async def file_set_modes(params: dict) -> bool:
    """Set file permissions."""
    path = params.get("path")
    mode = params.get("mode")
    if not path or mode is None:
        raise ValueError("path and mode are required")

    await run_in_thread(os.chmod, path, mode)
    return True


async def file_set_times(params: dict) -> bool:
    """Set file timestamps."""
    path = params.get("path")
    mtime = params.get("mtime")
    if not path or mtime is None:
        raise ValueError("path and mtime are required")

    atime = params.get("atime", mtime)

    await run_in_thread(os.utime, path, (atime, mtime))
    return True


async def file_make_symlink(params: dict) -> bool:
    """Create a symbolic link."""
    target = params.get("target")
    link_path = params.get("link_path")
    if not target or not link_path:
        raise ValueError("target and link_path are required")

    await run_in_thread(os.symlink, target, link_path)
    return True


async def file_make_hardlink(params: dict) -> bool:
    """Create a hard link."""
    src = params.get("src")
    dest = params.get("dest")
    if not src or not dest:
        raise ValueError("src and dest are required")

    await run_in_thread(os.link, src, dest)
    return True


async def file_chown(params: dict) -> bool:
    """Change file ownership."""
    path = params.get("path")
    uid = params.get("uid", -1)
    gid = params.get("gid", -1)
    if not path:
        raise ValueError("path is required")

    await run_in_thread(os.chown, path, uid, gid)
    return True


# ============================================================================
# Directory Handlers
# ============================================================================


async def dir_list(params: dict) -> List[dict]:
    """List directory contents."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    include_attrs = params.get("include_attrs", False)
    include_hidden = params.get("include_hidden", True)

    def do_list():
        entries = []

        # Add . and .. if including hidden
        if include_hidden:
            # Current directory
            try:
                st = os.stat(path)
                entry = {
                    "name": ".",
                    "type": "directory",
                }
                if include_attrs:
                    entry["attrs"] = format_file_attributes(st, path)
                entries.append(entry)
            except OSError:
                pass

            # Parent directory
            parent = os.path.dirname(path) or path
            try:
                st = os.stat(parent)
                entry = {
                    "name": "..",
                    "type": "directory",
                }
                if include_attrs:
                    entry["attrs"] = format_file_attributes(st, parent)
                entries.append(entry)
            except OSError:
                pass

        # List directory contents
        with os.scandir(path) as it:
            for dir_entry in it:
                name = dir_entry.name

                # Skip hidden files if not requested
                if not include_hidden and name.startswith("."):
                    continue

                # Get file type
                try:
                    if dir_entry.is_symlink():
                        file_type = "symlink"
                    elif dir_entry.is_dir():
                        file_type = "directory"
                    elif dir_entry.is_file():
                        file_type = "file"
                    else:
                        # Need to stat for special files
                        st = dir_entry.stat(follow_symlinks=False)
                        file_type = get_file_type(st.st_mode)
                except OSError:
                    file_type = "unknown"

                entry = {
                    "name": name,
                    "type": file_type,
                }

                if include_attrs:
                    try:
                        st = dir_entry.stat(follow_symlinks=False)
                        entry["attrs"] = format_file_attributes(
                            st, dir_entry.path, is_lstat=True
                        )
                    except OSError:
                        pass

                entries.append(entry)

        # Sort by name
        entries.sort(key=lambda e: e["name"])
        return entries

    return await run_in_thread(do_list)


async def dir_create(params: dict) -> bool:
    """Create a directory."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    parents = params.get("parents", False)
    mode = params.get("mode", 0o755)

    def do_create():
        if parents:
            os.makedirs(path, mode=mode, exist_ok=True)
        else:
            os.mkdir(path, mode=mode)
        return True

    return await run_in_thread(do_create)


async def dir_remove(params: dict) -> bool:
    """Remove a directory."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    recursive = params.get("recursive", False)

    def do_remove():
        if recursive:
            shutil.rmtree(path)
        else:
            os.rmdir(path)
        return True

    return await run_in_thread(do_remove)


async def dir_completions(params: dict) -> List[str]:
    """Get completions for a path prefix."""
    directory = params.get("directory")
    prefix = params.get("prefix", "")
    if not directory:
        raise ValueError("directory is required")

    def do_completions():
        completions = []
        try:
            with os.scandir(directory) as it:
                for entry in it:
                    name = entry.name
                    if name.startswith(prefix):
                        suffix = "/" if entry.is_dir() else ""
                        completions.append(name + suffix)
        except OSError:
            pass
        completions.sort()
        return completions

    return await run_in_thread(do_completions)


# ============================================================================
# Process Handlers
# ============================================================================


async def process_run(params: dict) -> dict:
    """Run a command and wait for it to complete."""
    cmd = params.get("cmd")
    if not cmd:
        raise ValueError("cmd is required")

    args = params.get("args", [])
    cwd = params.get("cwd")
    env = params.get("env")
    stdin_b64 = params.get("stdin")
    clear_env = params.get("clear_env", False)

    # Prepare environment
    if clear_env:
        proc_env = dict(env) if env else {}
    elif env:
        proc_env = os.environ.copy()
        proc_env.update(env)
    else:
        proc_env = None

    # Prepare stdin
    stdin_data = base64.b64decode(stdin_b64) if stdin_b64 else None

    # Create subprocess
    proc = await asyncio.create_subprocess_exec(
        cmd,
        *args,
        stdin=asyncio.subprocess.PIPE if stdin_data else asyncio.subprocess.DEVNULL,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=cwd,
        env=proc_env,
    )

    # Communicate
    stdout, stderr = await proc.communicate(stdin_data)

    # Smart encode output
    stdout_str, stdout_enc = smart_encode(stdout)
    stderr_str, stderr_enc = smart_encode(stderr)

    return {
        "exit_code": proc.returncode,
        "stdout": stdout_str,
        "stderr": stderr_str,
        "stdout_encoding": stdout_enc,
        "stderr_encoding": stderr_enc,
    }


async def process_start(params: dict) -> dict:
    """Start an async process."""
    global next_process_pid

    cmd = params.get("cmd")
    if not cmd:
        raise ValueError("cmd is required")

    args = params.get("args", [])
    cwd = params.get("cwd")
    env = params.get("env")
    clear_env = params.get("clear_env", False)

    # Prepare environment
    if clear_env:
        proc_env = dict(env) if env else {}
    elif env:
        proc_env = os.environ.copy()
        proc_env.update(env)
    else:
        proc_env = None

    # Create subprocess
    proc = await asyncio.create_subprocess_exec(
        cmd,
        *args,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=cwd,
        env=proc_env,
    )

    async with state_lock:
        pid = next_process_pid
        next_process_pid += 1
        processes[pid] = {"process": proc, "cmd": cmd}

    return {"pid": pid}


async def process_write(params: dict) -> dict:
    """Write to an async process's stdin."""
    pid = params.get("pid")
    data_b64 = params.get("data")
    if pid is None or data_b64 is None:
        raise ValueError("pid and data are required")

    data = base64.b64decode(data_b64)

    async with state_lock:
        if pid not in processes:
            raise ValueError(f"Process not found: {pid}")
        proc = processes[pid]["process"]

    proc.stdin.write(data)
    await proc.stdin.drain()

    return {"written": len(data)}


async def process_read(params: dict) -> dict:
    """Read from an async process's stdout/stderr."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    max_bytes = params.get("max_bytes", 65536)
    timeout_ms = params.get("timeout_ms", 0)

    async with state_lock:
        if pid not in processes:
            raise ValueError(f"Process not found: {pid}")
        proc = processes[pid]["process"]

    timeout = timeout_ms / 1000.0 if timeout_ms > 0 else 0.001

    # Try to read stdout
    stdout_data = b""
    try:
        stdout_data = await asyncio.wait_for(
            proc.stdout.read(max_bytes), timeout=timeout
        )
    except asyncio.TimeoutError:
        pass

    # Try to read stderr
    stderr_data = b""
    try:
        stderr_data = await asyncio.wait_for(
            proc.stderr.read(max_bytes), timeout=timeout
        )
    except asyncio.TimeoutError:
        pass

    # Check if process has exited
    exited = proc.returncode is not None

    # Build response
    result = {"exited": exited, "exit_code": proc.returncode}

    if stdout_data:
        stdout_str, stdout_enc = smart_encode(stdout_data)
        result["stdout"] = stdout_str
        result["stdout_encoding"] = stdout_enc
    else:
        result["stdout"] = None
        result["stdout_encoding"] = None

    if stderr_data:
        stderr_str, stderr_enc = smart_encode(stderr_data)
        result["stderr"] = stderr_str
        result["stderr_encoding"] = stderr_enc
    else:
        result["stderr"] = None
        result["stderr_encoding"] = None

    return result


async def process_close_stdin(params: dict) -> bool:
    """Close the stdin of an async process."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    async with state_lock:
        if pid not in processes:
            raise ValueError(f"Process not found: {pid}")
        proc = processes[pid]["process"]

    proc.stdin.close()
    await proc.stdin.wait_closed()
    return True


async def process_kill(params: dict) -> bool:
    """Kill an async process."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    sig = params.get("signal", signal.SIGTERM)

    async with state_lock:
        if pid not in processes:
            raise ValueError(f"Process not found: {pid}")
        proc = processes[pid]["process"]

    os.kill(proc.pid, sig)

    # Remove from map if SIGKILL
    if sig == signal.SIGKILL:
        async with state_lock:
            processes.pop(pid, None)

    return True


async def process_list(params: dict) -> List[dict]:
    """List all managed async processes."""
    result = []

    async with state_lock:
        for pid, info in processes.items():
            proc = info["process"]
            exited = proc.returncode is not None
            result.append(
                {
                    "pid": pid,
                    "os_pid": proc.pid,
                    "cmd": info["cmd"],
                    "exited": exited,
                    "exit_code": proc.returncode,
                }
            )

    return result


# ============================================================================
# PTY Process Handlers
# ============================================================================


def set_window_size(fd: int, rows: int, cols: int):
    """Set terminal window size."""
    winsize = struct.pack("HHHH", rows, cols, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)


def set_nonblocking(fd: int):
    """Set file descriptor to non-blocking mode."""
    flags = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)


async def pty_start(params: dict) -> dict:
    """Start a process with a PTY."""
    global next_pty_pid

    cmd = params.get("cmd")
    if not cmd:
        raise ValueError("cmd is required")

    args = params.get("args", [])
    cwd = params.get("cwd")
    env = params.get("env")
    clear_env = params.get("clear_env", False)
    rows = params.get("rows", 24)
    cols = params.get("cols", 80)

    def do_fork():
        # Open PTY
        master_fd, slave_fd = pty.openpty()

        # Set window size
        set_window_size(master_fd, rows, cols)

        # Get tty name
        tty_name = os.ttyname(slave_fd)

        # Fork
        child_pid = os.fork()

        if child_pid == 0:
            # Child process
            os.close(master_fd)

            # Create new session
            os.setsid()

            # Set controlling terminal
            fcntl.ioctl(slave_fd, termios.TIOCSCTTY, 0)

            # Duplicate slave to stdin/stdout/stderr
            os.dup2(slave_fd, 0)
            os.dup2(slave_fd, 1)
            os.dup2(slave_fd, 2)

            if slave_fd > 2:
                os.close(slave_fd)

            # Change directory
            if cwd:
                os.chdir(cwd)

            # Set environment
            if clear_env:
                os.environ.clear()
            if env:
                os.environ.update(env)

            # Execute
            os.execvp(cmd, [cmd] + args)
            # If execvp returns, it failed
            os._exit(127)
        else:
            # Parent process
            os.close(slave_fd)

            # Set non-blocking
            set_nonblocking(master_fd)

            return master_fd, child_pid, tty_name

    master_fd, child_pid, tty_name = await run_in_thread(do_fork)

    async with state_lock:
        pid = next_pty_pid
        next_pty_pid += 1
        pty_processes[pid] = {
            "master_fd": master_fd,
            "child_pid": child_pid,
            "cmd": cmd,
            "exit_status": None,
        }

    return {"pid": pid, "os_pid": child_pid, "tty_name": tty_name}


async def pty_read(params: dict) -> dict:
    """Read from a PTY process."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    max_bytes = params.get("max_bytes", 65536)
    timeout_ms = params.get("timeout_ms", 0)

    async with state_lock:
        if pid not in pty_processes:
            return {
                "output": None,
                "output_encoding": None,
                "exited": True,
                "exit_code": None,
            }
        info = pty_processes[pid]
        master_fd = info["master_fd"]
        child_pid = info["child_pid"]

    def do_read():
        timeout = timeout_ms / 1000.0 if timeout_ms > 0 else 0.001

        # Use select to wait for data
        ready, _, _ = select.select([master_fd], [], [], timeout)

        if not ready:
            return b"", False, None

        try:
            data = os.read(master_fd, max_bytes)
            return data, False, None
        except OSError as e:
            if e.errno == errno.EAGAIN or e.errno == errno.EWOULDBLOCK:
                return b"", False, None
            elif e.errno == errno.EIO:
                # EIO often means child exited
                return b"", True, None
            raise

    data, maybe_exited, _ = await run_in_thread(do_read)

    # Check exit status
    exited = False
    exit_code = None

    async with state_lock:
        if pid in pty_processes:
            info = pty_processes[pid]
            if info["exit_status"] is not None:
                exited = True
                exit_code = info["exit_status"]
            else:
                # Try non-blocking wait
                try:
                    wpid, status = os.waitpid(child_pid, os.WNOHANG)
                    if wpid != 0:
                        if os.WIFEXITED(status):
                            exit_code = os.WEXITSTATUS(status)
                        elif os.WIFSIGNALED(status):
                            exit_code = 128 + os.WTERMSIG(status)
                        else:
                            exit_code = -1
                        info["exit_status"] = exit_code
                        exited = True
                except ChildProcessError:
                    exited = True

    if maybe_exited and not exited:
        # Re-check after EIO
        async with state_lock:
            if pid in pty_processes:
                info = pty_processes[pid]
                try:
                    wpid, status = os.waitpid(info["child_pid"], os.WNOHANG)
                    if wpid != 0:
                        if os.WIFEXITED(status):
                            exit_code = os.WEXITSTATUS(status)
                        elif os.WIFSIGNALED(status):
                            exit_code = 128 + os.WTERMSIG(status)
                        info["exit_status"] = exit_code
                        exited = True
                except ChildProcessError:
                    exited = True

    # Build response
    if data:
        output_str, output_enc = smart_encode(data)
        return {
            "output": output_str,
            "output_encoding": output_enc,
            "exited": exited,
            "exit_code": exit_code,
        }
    else:
        return {
            "output": None,
            "output_encoding": None,
            "exited": exited,
            "exit_code": exit_code,
        }


async def pty_write(params: dict) -> dict:
    """Write to a PTY process."""
    pid = params.get("pid")
    data_b64 = params.get("data")
    if pid is None or data_b64 is None:
        raise ValueError("pid and data are required")

    data = base64.b64decode(data_b64)

    async with state_lock:
        if pid not in pty_processes:
            raise ValueError(f"PTY process not found: {pid}")
        master_fd = pty_processes[pid]["master_fd"]

    def do_write():
        return os.write(master_fd, data)

    written = await run_in_thread(do_write)
    return {"written": written}


async def pty_resize(params: dict) -> bool:
    """Resize a PTY terminal."""
    pid = params.get("pid")
    rows = params.get("rows")
    cols = params.get("cols")
    if pid is None or rows is None or cols is None:
        raise ValueError("pid, rows, and cols are required")

    async with state_lock:
        if pid not in pty_processes:
            raise ValueError(f"PTY process not found: {pid}")
        info = pty_processes[pid]
        master_fd = info["master_fd"]
        child_pid = info["child_pid"]

    def do_resize():
        set_window_size(master_fd, rows, cols)
        # Send SIGWINCH to the child's process group
        try:
            # Try to get the foreground process group
            fg_pgrp = os.tcgetpgrp(master_fd)
            os.killpg(fg_pgrp, signal.SIGWINCH)
        except (OSError, ProcessLookupError):
            # Fallback: send to child's process group
            try:
                os.killpg(os.getpgid(child_pid), signal.SIGWINCH)
            except (OSError, ProcessLookupError):
                pass

    await run_in_thread(do_resize)
    return True


async def pty_kill(params: dict) -> bool:
    """Kill a PTY process."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    sig = params.get("signal", signal.SIGTERM)

    async with state_lock:
        if pid not in pty_processes:
            raise ValueError(f"PTY process not found: {pid}")
        child_pid = pty_processes[pid]["child_pid"]

    os.kill(child_pid, sig)

    # Remove from map if SIGKILL
    if sig == signal.SIGKILL:
        async with state_lock:
            if pid in pty_processes:
                os.close(pty_processes[pid]["master_fd"])
                del pty_processes[pid]

    return True


async def pty_close(params: dict) -> bool:
    """Close a PTY process and clean up."""
    pid = params.get("pid")
    if pid is None:
        raise ValueError("pid is required")

    async with state_lock:
        if pid not in pty_processes:
            raise ValueError(f"PTY process not found: {pid}")
        info = pty_processes.pop(pid)

    # Kill the process and close fd
    try:
        os.kill(info["child_pid"], signal.SIGKILL)
    except (OSError, ProcessLookupError):
        pass

    try:
        os.close(info["master_fd"])
    except OSError:
        pass

    return True


async def pty_list(params: dict) -> List[dict]:
    """List all PTY processes."""
    result = []

    async with state_lock:
        for pid, info in pty_processes.items():
            # Check exit status
            exited = False
            exit_code = info["exit_status"]

            if exit_code is None:
                try:
                    wpid, status = os.waitpid(info["child_pid"], os.WNOHANG)
                    if wpid != 0:
                        if os.WIFEXITED(status):
                            exit_code = os.WEXITSTATUS(status)
                        elif os.WIFSIGNALED(status):
                            exit_code = 128 + os.WTERMSIG(status)
                        info["exit_status"] = exit_code
                        exited = True
                except ChildProcessError:
                    exited = True
            else:
                exited = True

            result.append(
                {
                    "pid": pid,
                    "os_pid": info["child_pid"],
                    "cmd": info["cmd"],
                    "exited": exited,
                    "exit_code": exit_code,
                }
            )

    return result


# ============================================================================
# System Handlers
# ============================================================================


async def system_info(params: dict) -> dict:
    """Get system information."""
    return {
        "version": VERSION,
        "os": sys.platform,
        "arch": os.uname().machine,
        "hostname": socket.gethostname(),
        "uid": os.getuid(),
        "gid": os.getgid(),
        "home": os.environ.get("HOME"),
        "user": os.environ.get("USER"),
    }


async def system_getenv(params: dict) -> Optional[str]:
    """Get environment variable."""
    name = params.get("name")
    if not name:
        raise ValueError("name is required")
    return os.environ.get(name)


async def system_expand_path(params: dict) -> str:
    """Expand path with tilde and environment variables."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")
    return os.path.expanduser(path)


async def system_statvfs(params: dict) -> dict:
    """Get filesystem information."""
    path = params.get("path")
    if not path:
        raise ValueError("path is required")

    def do_statvfs():
        st = os.statvfs(path)
        block_size = st.f_frsize
        return {
            "total": st.f_blocks * block_size,
            "free": st.f_bfree * block_size,
            "available": st.f_bavail * block_size,
            "block_size": block_size,
        }

    return await run_in_thread(do_statvfs)


async def system_groups(params: dict) -> List[dict]:
    """Get groups for the current user."""

    def do_groups():
        groups = os.getgroups()
        result = []
        for gid in groups:
            gname = get_group_name(gid)
            result.append({"gid": gid, "name": gname})
        return result

    return await run_in_thread(do_groups)


# ============================================================================
# Batch Handler
# ============================================================================


async def batch_execute(params: dict) -> dict:
    """Execute multiple RPC requests in a single batch."""
    requests = params.get("requests", [])

    async def execute_one(req: dict):
        method = req.get("method")
        req_params = req.get("params", {})

        try:
            handler = METHOD_DISPATCH.get(method)
            if handler is None:
                return {
                    "error": {
                        "code": METHOD_NOT_FOUND,
                        "message": f"Method not found: {method}",
                    }
                }
            result = await handler(req_params)
            return {"result": result}
        except Exception as e:
            return {"error": {"code": INTERNAL_ERROR, "message": str(e)}}

    # Execute all requests concurrently
    results = await asyncio.gather(*[execute_one(req) for req in requests])

    return {"results": list(results)}


# ============================================================================
# Method Dispatch Table
# ============================================================================

METHOD_DISPATCH = {
    # File metadata
    "file.stat": file_stat,
    "file.stat_batch": file_stat_batch,
    "file.exists": file_exists,
    "file.readable": file_readable,
    "file.writable": file_writable,
    "file.executable": file_executable,
    "file.truename": file_truename,
    "file.newer_than": file_newer_than,
    # File I/O
    "file.read": file_read,
    "file.write": file_write,
    "file.copy": file_copy,
    "file.rename": file_rename,
    "file.delete": file_delete,
    "file.set_modes": file_set_modes,
    "file.set_times": file_set_times,
    "file.make_symlink": file_make_symlink,
    "file.make_hardlink": file_make_hardlink,
    "file.chown": file_chown,
    # Directory
    "dir.list": dir_list,
    "dir.create": dir_create,
    "dir.remove": dir_remove,
    "dir.completions": dir_completions,
    # Process
    "process.run": process_run,
    "process.start": process_start,
    "process.write": process_write,
    "process.read": process_read,
    "process.close_stdin": process_close_stdin,
    "process.kill": process_kill,
    "process.list": process_list,
    # PTY
    "process.start_pty": pty_start,
    "process.read_pty": pty_read,
    "process.write_pty": pty_write,
    "process.resize_pty": pty_resize,
    "process.kill_pty": pty_kill,
    "process.close_pty": pty_close,
    "process.list_pty": pty_list,
    # System
    "system.info": system_info,
    "system.getenv": system_getenv,
    "system.expand_path": system_expand_path,
    "system.statvfs": system_statvfs,
    "system.groups": system_groups,
    # Batch
    "batch": batch_execute,
}


# ============================================================================
# Request Processing
# ============================================================================


async def process_request(line: str) -> dict:
    """Process a single JSON-RPC request."""
    try:
        request = json.loads(line)
    except json.JSONDecodeError as e:
        return error_response(None, PARSE_ERROR, f"Parse error: {e}")

    request_id = request.get("id")

    # Validate JSON-RPC version
    if request.get("jsonrpc") != "2.0":
        return error_response(request_id, INVALID_REQUEST, "Invalid JSON-RPC version")

    method = request.get("method")
    if not method:
        return error_response(request_id, INVALID_REQUEST, "Method is required")

    params = request.get("params", {})

    # Dispatch to handler
    handler = METHOD_DISPATCH.get(method)
    if handler is None:
        return error_response(
            request_id, METHOD_NOT_FOUND, f"Method not found: {method}"
        )

    try:
        result = await handler(params)
        return success_response(request_id, result)
    except ValueError as e:
        return error_response(request_id, INVALID_PARAMS, str(e))
    except OSError as e:
        code, message = map_os_error(e, params.get("path", ""))
        return error_response(request_id, code, message)
    except Exception as e:
        return error_response(request_id, INTERNAL_ERROR, str(e))


# ============================================================================
# Main Entry Point
# ============================================================================


def main():
    """
    Main entry point - synchronous loop with async request handling.

    This approach is optimal for TRAMP usage where requests come sequentially.
    Each request's async handlers can still benefit from concurrent I/O
    (e.g., parallel file operations in batch requests).
    """
    # Create a single event loop for all requests
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)

    try:
        for line in sys.stdin:
            if not line.strip():
                continue

            # Process the request using async handlers
            response = loop.run_until_complete(process_request(line))

            # Write response
            response_json = json.dumps(response)
            sys.stdout.write(response_json + "\n")
            sys.stdout.flush()
    except KeyboardInterrupt:
        pass
    except BrokenPipeError:
        pass
    finally:
        # Clean up pending tasks
        pending = asyncio.all_tasks(loop)
        for task in pending:
            task.cancel()
        if pending:
            loop.run_until_complete(asyncio.gather(*pending, return_exceptions=True))
        loop.close()


if __name__ == "__main__":
    main()
