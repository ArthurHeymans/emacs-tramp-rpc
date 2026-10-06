#!/usr/bin/env python3
"""Relay tramp-rpc-server stdio with seeded random chunking and delays.

Run in place of the server binary (see scripts/chaos-test.sh).  Bytes are
never reordered or altered, only split at random offsets and delayed, so
frames arrive torn across reads or coalesced with the frames behind them.
"""

import argparse
import os
import random
import subprocess
import sys
import threading
import time


def write_all(fd, data):
    view = memoryview(data)
    while view:
        view = view[os.write(fd, view):]


def pump(src, dst, rng, args, on_eof):
    """Copy SRC to DST in random pieces until EOF or a broken pipe."""
    try:
        while data := os.read(src, 65536):
            while data:
                size = rng.randint(1, args.max_chunk)
                write_all(dst, data[:size])
                data = data[size:]
                if rng.random() < args.stall_prob:
                    time.sleep(rng.uniform(0, args.stall_ms) / 1000)
                elif rng.random() < 0.5:
                    time.sleep(rng.uniform(0, args.max_delay_ms) / 1000)
    except OSError:
        pass
    on_eof()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--max-chunk", type=int, default=256)
    parser.add_argument("--max-delay-ms", type=float, default=2.0)
    parser.add_argument("--stall-prob", type=float, default=0.01)
    parser.add_argument("--stall-ms", type=float, default=40.0)
    parser.add_argument("server")
    parser.add_argument("server_args", nargs=argparse.REMAINDER)
    args = parser.parse_args()

    child = subprocess.Popen([args.server, *args.server_args],
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    up = threading.Thread(
        target=pump, daemon=True,
        args=(sys.stdin.fileno(), child.stdin.fileno(),
              random.Random(f"{args.seed}-up"), args, child.stdin.close))
    up.start()
    # Server output ends when the server exits; stop it if Emacs went away.
    pump(child.stdout.fileno(), sys.stdout.fileno(),
         random.Random(f"{args.seed}-down"), args, lambda: None)
    if child.poll() is None:
        child.kill()
    sys.exit(child.wait())


if __name__ == "__main__":
    main()
