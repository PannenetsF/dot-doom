#!/usr/bin/env python3
"""Exercise actual Evil j/k input in fresh TTY Emacs processes, not batch eval."""
import argparse
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import struct
import subprocess
import tempfile
import termios
import time


def run_once(args, index):
    with tempfile.TemporaryDirectory(prefix="doom-startup-probe-") as tmp:
        root = Path(tmp)
        master, slave = pty.openpty()
        os.set_blocking(master, False)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
        env = dict(os.environ, TERM="xterm-256color", DOOM_STARTUP_PROBE_ROOT=tmp)
        if args.trace:
            env["DOOM_STARTUP_PROBE_TRACE"] = "1"
        command = [args.emacs, "-nw", "--init-directory", str(args.init_dir),
                   "-l", str(Path(__file__).with_name("startup-latency-probe.el"))]
        process = subprocess.Popen(command, stdin=slave, stdout=slave, stderr=slave,
                                   env=env, start_new_session=True)
        os.close(slave)
        started = time.monotonic()
        ready_at = None
        sent = False
        output = bytearray()
        try:
            while time.monotonic() - started < args.timeout:
                if select.select([master], [], [], 0.01)[0]:
                    try:
                        output.extend(os.read(master, 65536))
                    except BlockingIOError:
                        pass
                    except OSError:
                        break
                if ready_at is None and (root / "ready.json").exists():
                    ready_at = time.monotonic()
                if ready_at is not None and not sent:
                    if time.monotonic() - ready_at >= args.idle:
                        os.write(master, b"jk" * 10)
                        sent = True
                if process.poll() is not None:
                    break
            (root / "terminal.log").write_bytes(output)
            if process.poll() is None:
                raise RuntimeError("Probe timed out; terminal tail: " + repr(bytes(output[-4000:])))
            result_file = root / "result.json"
            if not result_file.exists():
                raise RuntimeError("No probe result; terminal tail: "
                                   + repr(bytes(output[-2000:])))
            result = json.loads(result_file.read_text())
            if process.returncode or "error" in result:
                raise RuntimeError(str(result))
            result["run"] = index
            result["ready_ms"] = round((ready_at - started) * 1000, 2)
            return result
        finally:
            # Terminate only the disposable process we started, never user Emacs.
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)
            os.close(master)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--init-dir", type=Path,
                        default=Path.home() / ".config/emacs")
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--idle", type=float, default=0,
                        help="Seconds to wait before sending j/k (test idle timers)")
    parser.add_argument("--timeout", type=float, default=90)
    parser.add_argument("--max-ms", type=float, default=100)
    parser.add_argument("--trace", action="store_true", help="Trace slow requires")
    parser.add_argument("--output", type=Path, help="Persist JSON evidence")
    args = parser.parse_args()
    if args.runs < 1 or args.idle < 0:
        parser.error("--runs must be positive and --idle nonnegative")
    results = []
    for index in range(1, args.runs + 1):
        result = run_once(args, index)
        results.append(result)
        samples = result["samples"]
        worst = max(sample["ms"] for sample in samples)
        worst_input = max(sample["ms"] for sample in result["input_events"])
        print(f"run={index} ready={result['ready_ms']:.1f}ms "
              f"first_j={samples[0]['ms']:.3f}ms worst_jk={worst:.3f}ms "
              f"worst_input={worst_input:.3f}ms "
              f"slow_requires={len(result['loads'])}", flush=True)
    if args.output:
        args.output.write_text(json.dumps(results, indent=2) + "\n")
    if any(sample["ms"] > args.max_ms
           for result in results for sample in result["input_events"]):
        raise SystemExit("FAIL: input latency exceeds threshold")


if __name__ == "__main__":
    main()
