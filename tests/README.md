# Rhizome regression tests

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
- `focus-on-close.lisp` - focus recovery when a window closes: the next
  window in its frame (or the next column of a scroll ribbon), else the
  newest window on its workspace, else a cleared seat; focus and clear
  requests reach river only in the manage pass; unfocused closes and
  windows on other workspaces or outputs leave focus alone.
- `focus-after-capture.lisp` - keyboard focus returns to each seat's
  focused window when the prefix capture, a prompt, the eval prompt, a
  context menu, or the launcher closes; focus state does not change, and
  a missing or closed window gets nothing.
- `layer-focus.lisp` - layer shell focus events delivered as wayflan
  reads them: focus_none gives each seat's focused window the keyboard
  back in the next manage pass without changing focus state;
  focus_exclusive and focus_non_exclusive are logged and queue nothing;
  a missing or closed window gets nothing.
- `frames.lisp` — frame tree operations: splits, merges, resizes,
  directional focus, cycling, tree construction from rectangles,
  layout templates, and manage/render integration.
- `parity.lisp` — Alt+drag pointer bindings (drag floats tiled
  windows), the C-t h help system, describe-key, move-window-to-frame,
  and `:output` window rules.
- `quickwins.lisp` — StumpWM quick-wins: window numbering (1..9, 0,
  overflow, reuse, renumber), pull/exchange/only/balance/fclear,
  lastmsg/time, quit-confirm, fullscreen request queueing, F-keys.
- `interactivity.lisp` — multi-line messages, which-key hints,
  iresize (arrows resize until Enter/Esc), and command mode (sticky
  prefix: multiple commands until Escape).
- `medium3.lisp` — frame dump/restore round-trips (shape, rects,
  window placement, output scaling, missing/unmentioned windows),
  window marks lifecycle and move-marked, and set-prefix-key's key
  spec parsing and binding swaps.
- `bigger1.lisp` — minor modes (precedence over the prefix map,
  stacking order, enable/disable), WSE expressions (and/or/not/any,
  float/marked/number selectors, :wse window rules), and fuzzy
  matching (subsequence + substring-first ranking).
- `scroll.lisp` — the niri-style ribbon: enter/exit transitions
  (frame order becomes column order, dynamic re-adoption on exit),
  column geometry, viewport clamping, focus-follows-visibility,
  close-pruning with focus recovery, arrivals next to focus, floats
  leaving the ribbon, toggling, and pipeline dispatch routing.
- `dbus.lisp` — the pure-CL DBus wire layer: marshalling roundtrips
  for every wire type (including the header a(yv) array), then LIVE
  integration against the real session bus: SASL auth, Hello,
  ListNames, RequestName/GetNameOwner round-trip, Properties.Get on
  the broker, and the reader thread.
- `expose.lisp` — the C-t Return window overview: grid geometry
  (aspect-fit, bar offset), layout round-trip (floats, frame
  membership, per-frame order, focus), digit/arrow/Return/click
  picking, and refusal with too few windows.
- `bar-guard.lisp` — the bar fork-bomb regression: the bar process
  (*bar-process-mode*) must never spawn or stop bars, no matter what
  the user config asks for; bar-apply-config / toggle-bar / start /
  stop all refuse there, and start/stop-bar now reap orphan bars.
- `session-icons.lisp` — session management (lock command building,
  swayidle arming, logout confirm flow) and the bar icons module
  (icon discovery, 16x16 scaling, caching, per-entry actions).
- `hooks-and-eval.lisp` — hook firing/removal/error isolation and the
  eval prompt's evaluator.
- `ipc.lisp` — command dispatch and a real unix-socket round-trip
  (server thread + client thread + main-thread queue drain).
