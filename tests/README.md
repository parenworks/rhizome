# Stumpway regression tests

Plain SBCL scripts that exercise window-management logic against
stubbed river requests. Run with:

    sbcl --non-interactive --load tests/workspace-restore.lisp
    sbcl --non-interactive --load tests/workspace-switch-away.lisp

- `workspace-restore.lisp` — switching back to a workspace must SHOW
  its previously hidden windows (regression: the show-gate was once
  inverted, hiding windows forever).
- `workspace-switch-away.lisp` — switching away must HIDE the old
  workspace's windows and SHOW the new one's, without redundant
  show/hide requests (which cause river manage/render loops).
- `window-rules.lisp` — app-id/title regex rules assign workspaces.
- `floating.lisp` — float/tile state, geometry clamps, interactive
  resize math, manage/render integration, and `:float` window rules.
- `frames.lisp` — frame tree operations: splits, merges, resizes,
  directional focus, cycling, tree construction from rectangles,
  layout templates, and manage/render integration.
- `parity.lisp` — Alt+drag pointer bindings (drag floats tiled
  windows), the C-t h help system, describe-key, move-window-to-frame,
  and `:output` window rules.
- `hooks-and-eval.lisp` — hook firing/removal/error isolation and the
  eval prompt's evaluator.
- `ipc.lisp` — command dispatch and a real unix-socket round-trip
  (server thread + client thread + main-thread queue drain).
