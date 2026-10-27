# Startup Latency Checks

These tests use the installed Doom configuration and packages. They do not
install packages, rebuild Doom, control existing Emacs windows, or edit real
project files. Run from this checkout.

## Unit Tests

```sh
emacs --batch -Q -l tests/startup-preheat-test.el -f ert-run-tests-batch-and-exit
emacs --batch -Q -l tests/python-lsp-test.el -f ert-run-tests-batch-and-exit
emacs --batch -Q -l tests/org-startup-test.el -f ert-run-tests-batch-and-exit
emacs --batch -Q -l tests/doom_native_cache_test.el -f ert-run-tests-batch-and-exit
```

The Python tests macroexpand the deferred registration before loading LSP,
matching real config loading. Checking only the final workspace status misses
initialization callbacks that fail after setting that status.

## Real Input Checks

Use an available Python environment (on this Mac, activate
`/Users/bytedance/uv-envs/base/bin/activate` first):

```sh
python tests/startup-latency.py --runs 3 --input-interval .1 --output /tmp/doom-startup.json
python tests/startup-latency.py --runs 2 --idle 2 --input-interval .1 --output /tmp/doom-idle.json
python tests/startup-latency.py --scenario python --during-init --input-interval .2 --runs 3 --output /tmp/doom-python-typing.json
python tests/startup-latency.py --scenario python --runs 2 --output /tmp/doom-python-settled.json
python tests/startup-latency.py --scenario org --runs 3 --input-interval .1 --output /tmp/doom-org.json
python tests/startup-latency.py --scenario treemacs --runs 2 --input-interval .1 --output /tmp/doom-treemacs.json
```

Each run opens a disposable terminal Emacs in a PTY, verifies Evil's `j/k`
bindings, sends 20 real alternating key events, and exits. It rejects startup
errors, missing results, and input latency above 100 ms (`--max-ms` overrides
this). Timeout cleanup targets only the subprocess created by the test.

`dispatch_ms` measures harness send to Emacs post-command processing, including
queueing **before** pre-command-hook. `ms` measures only the command-hook
interval; it can be sub-millisecond even during a multi-second queue freeze.
Neither metric includes display rendering after post-command-hook. The probe
starts after `window-setup-hook`: earlier readiness incorrectly included xterm
initialization waits in first-input timing. A bare PTY does not answer terminal
capability queries, so startup ready time includes several seconds that need
not occur in a real terminal or GUI. Old startup-hook-only numbers are not
comparable.

`--trace` adds require/timer timing and CPU samples during input, with extra
measurement overhead. Stages distinguish startup, scenario setup, input, and
post-input verification. Nested require durations must not be summed. Native
function inventories are diagnostic only: bytecode wrappers, generic methods,
and inline definitions are not automatically AOT misses.

Scenarios create a disposable Git project under the system temporary directory:

- Python waits for successful Jedi/Ruff/ty initialization hooks, rejects protocol
  callback errors, verifies unique Jedi argument snippets, and checks actual ty
  and Ruff diagnostics both in workspace tables and Flycheck's rendered errors.
  During-init mode types while initialization runs; normal mode waits for it.
  Post-input verification loads Bash/clangd clients and checks ty stays an add-on.
- Org verifies the optional Pomodoro/Roam UI stays unloaded during normal editing,
  retains the org-lint checker, then loads optional tools to verify configuration
  without starting their timer, web server, or browser.
- Treemacs verifies the tree opens, then measures Evil movement in another window,
  not the tree's own navigation commands.

The probe reads normal user state and may save routine history. This is not a
GUI/CoreText benchmark, a large-project test, or a guarantee against arbitrary
machine load. `feature_open_ms` explicitly reports synchronous first-use cost;
removing first-command freezes does not make loading all mode dependencies free.

## Fixes And AOT Boundary

The original first `j/k` loaded pyim/liberime and Doom first-input extensions
(1-3 seconds despite native code). Initialize that small always-used set through
Doom's own hook dispatcher before accepting input. Python/Treemacs are still
on demand; there is no large speculative idle warm-up or synthetic key macro.

Python also had a real ty callback error: eager macroexpansion ran its struct
setter before lsp-mode defined it. Quoted deferred registration fixes the load
order. Visible file initialization starts LSP before accepting commands, loads
only the Python client bundle, and preselects LSP's native Flycheck checker.
This avoids launching a standalone Ruff checker just to block in delete-process
when LSP takes over (observed 143-313 ms). Successful real diagnostics are tested,
not inferred from an `initialized` status field.

Org's in-process Flycheck checker first loaded org-lint and export backends from
redisplay, queueing input for 0.3-1.6 seconds. Those dependencies now load during
Org file initialization; lint itself remains enabled. Optional Pomodoro and
Roam UI use command autoloads instead of loading with every Org buffer.

Package AOT uses **Doom's native mechanism**, not a second all-mode warm-up:

```sh
LSP_USE_PLISTS=1 ~/.config/emacs/bin/doom sync --aot --no-env
# Full rebuild after clearing native caches or changing Emacs version:
LSP_USE_PLISTS=1 ~/.config/emacs/bin/doom sync --aot --rebuild --no-env
```

Doom/straight compiles build directories recursively, including unloaded modes.
An audit found native artifacts for 150/151 startup package sources (the other
was a theme); Evil motions were already native. Preserve Doom's denylist,
including with-editor. AOT does not preserve runtime package initialization.

A separate **built-in**, cl-macs, loaded before Doom redirected its cache and
repeatedly queued JIT compilation. CPU profiles caught native-elisp-load during
input even though package AOT was complete. nix-hm's first-time bootstrap now
uses Emacs' own native-compile once in its initial cache, not another mode warm-up.
For an existing installation, repair that cache after an Emacs upgrade with:

```sh
emacs --batch -Q --eval '(when (and (fboundp (quote native-comp-available-p)) (native-comp-available-p)) (let ((source (locate-library "cl-macs.el"))) (when source (native-compile source))))'
```

Final acceptance on 2026-10-27, Emacs 31.1, without tracing:

| Scenario | Fresh processes | Largest send-to-post latency |
| --- | --- | --- |
| Startup | 3 | 2.98 ms |
| Startup after 2 s idle | 2 | 8.31 ms |
| Python, typing during initialization | 3 | 4.51 ms |
| Python, after initialization | 2 | 8.15 ms |
| Org, including working org-lint | 3 | 3.91 ms |
| Treemacs | 2 | 2.61 ms |

All Python runs verified both rendered diagnostic sources and unique Jedi
snippets. All 15 processes passed the default 100 ms threshold. These are local
warm-cache observations, not hard latency guarantees. Fresh-process TTY evidence
is under `/tmp/doom-wait-trace/*-acceptance.json` for this investigation
(ephemeral); re-run the commands for durable reproducibility.
