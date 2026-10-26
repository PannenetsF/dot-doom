# Startup Latency Checks

These tests use the locally installed Doom configuration and packages. They
do not download packages, rebuild all of Doom, or control an existing Emacs.

## Unit Tests

```sh
emacs --batch -Q -l tests/startup-preheat-test.el -f ert-run-tests-batch-and-exit
emacs --batch -Q -l tests/python-lsp-test.el -f ert-run-tests-batch-and-exit
```

## Interactive Command Loop

Run from the dot-doom checkout, using an available Python environment:

```sh
python tests/startup-latency.py --runs 3 --trace --output /tmp/doom-startup.json
python tests/startup-latency.py --runs 3 --idle 2 --output /tmp/doom-idle.json
```

Each run opens a disposable terminal Emacs in a PTY, verifies that `j/k`
resolve to Evil motions, sends 20 real key events, and exits. It fails on
missing results, startup errors, or input processing above 100 ms (override
with `--max-ms`). A timeout only terminates the subprocess this test created.
`--trace` reports slow `require` calls and adds some measurement overhead.

The JSON includes command-hook latency, GC time, first-input hook latency,
slow requires, and a native/bytecode function inventory for Evil, company,
and pyim. Bytecode inventory entries are **not automatically AOT misses**:
generic dispatch functions, inline definitions, and wrappers can legitimately
remain bytecode even when the package has an `.eln` file.

Limits: command-hook timing excludes terminal redisplay after post-command
hooks. This is not a GUI/CoreText/frame-rendering benchmark, and it does not
exercise file-specific LSP, syntax checking, or large-buffer hooks. Startup
reads normal user state and may save routine history on exit. No user file is
opened or edited by the probe.

## 2026-10-26 Investigation

The installed Emacs 31.1 already runs `evil-next-line` and
`evil-previous-line` as native functions. AOT does not eliminate package
initialization: first input loaded pyim/liberime, Evil extensions, orderless,
and company-capf. An initial instrumented run took 3150 ms for first input,
including 2064 ms in pyim; the next motion took 0.065 ms. Those are cold-load
observations, not stable benchmark guarantees.

The former idle warm-up also synchronously required Python/Conda/Eshell/TRAMP
and Treemacs in one main-thread callback, observed at 5-6 seconds. Moving this
work to an idle timer does not make it asynchronous or preemptible.

The configuration now initializes pyim and Doom's first-input hooks before
accepting input, once per session. Python and Treemacs remain on demand; no
synthetic keyboard macro runs during warm-up. Errors retain Doom's hook
context rather than being silently ignored.

Three fresh-process runs with an already warm OS cache:

| Configuration | Ready time | Largest measured first-input/command interval |
| --- | --- | --- |
| Former preheat | 1376-1824 ms | 205-262 ms |
| Startup first-input initialization | 2297-2446 ms | 0.076-0.098 ms |
| Fixed, two runs after 2 s idle | 2602-2807 ms | 0.094-0.309 ms |

These intervals do not include every cost between physical keypress and
visible redraw. The tradeoff is explicit: initialization happens before the
editor accepts input, rather than being charged to the first command or a
large idle callback. GUI and real-project measurements remain separate work.
