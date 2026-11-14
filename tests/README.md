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
- `frame-pruning.lisp` - empty-frame pruning: frames a user split makes
  survive manage passes and the move of their last window; frames a new
  window split open still close with it; `(:frames (:prune-empty :never))`
  keeps every empty frame and `:always` prunes user splits too; removing a
  split or collapsing to one frame leaves nothing marked.
- `placement-policy.lisp` - frame placement policy: by default a split
  fills its new half with the most recently focused hidden window and a
  new window enters the focused frame; `:new-frame :empty`, the
  `:new-window` preferences (`:empty`, `:last`, `:unfocused`, functions),
  a custom policy class selected by name, the fallback for an unknown
  name, and the workspace focus history.
- `split-focus.lisp` - the current frame after a split: it stays on
  the original half, with or without a focused window, so a second
  split divides that half again; the placement policy still fills the
  new half.
- `parity.lisp` — Alt+drag pointer bindings (drag floats tiled
  windows), the C-t h help system, describe-key, move-window-to-frame,
  and `:output` window rules.
- `quickwins.lisp` — StumpWM quick-wins: window numbering (1..9, 0,
  overflow, reuse, renumber), pull/exchange/only/balance/fclear,
  lastmsg/time, quit-confirm, fullscreen request queueing, F-keys.
- `client-fullscreen.lisp` - client fullscreen and exit-fullscreen
  requests queue the same manage-pass request as C-t F11; the
  requested output is used when river still reports it, otherwise
  the window's own output.
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
- `bar-hide.lisp` - toggle-bar hides and shows the running bar
  process instead of restarting it, and the bar reads :bar-hidden from
  the WM snapshot. The bar's space is reserved only while river reports
  an exclusive zone at least as tall as the bar. A configure that
  leaves the :bar settings alone keeps the bar, and two concurrent
  starts launch one process.
- `session-icons.lisp` — session management (lock command building,
  swayidle arming, logout confirm flow) and the bar icons module
  (icon discovery, 16x16 scaling, caching, per-entry actions).
- `repl-server-config.lisp`: how the REPL server settings resolve.
  `:slynk` is read, an older `:swank` entry is honoured while `:slynk` is left at its
  defaults, `:slynk` wins when both are set, and the defaults apply.
- `repl-server-port.lisp`: `repl-server-port` reports the port once the server
  listens, and a second `start-slynk` starts no second server. Server creation
  is stubbed; no slynk server is started.
- `hooks-and-eval.lisp` — hook firing/removal/error isolation and the
  eval prompt's evaluator.
- `keymap-layers.lisp` - keymap objects (parents, shadowing,
  enumeration) and the user/default layering: user bindings win over
  defaults whether made before or after them, and survive repeated
  manage sequences, set-user-mod, and set-prefix-key; reload-config
  replaces the user layer; define-key, define-prefix-key and
  define-keymap-key keep working.
- `key-sequences.lisp` - kbd parsing (C M s H A S, sequences), a
  direct key bound to a keymap (s-x then f), modifiers on later keys
  of a sequence (s-x C-M-f), the prefix key as an ordinary binding
  (C-t, C-', set-prefix-key to s-t), zero-argument and event-taking
  bindings with their specials, sticky keymaps, and grabs skipped for
  an unmapped modifier until set-modifier maps it.
- `key-grabs.lisp` - grab sync against stubbed river requests: river
  grabs exactly the first keys the top keymaps bind; binding,
  unbinding, undefine-key, set-user-mod, set-modifier, set-prefix-key,
  clearing the user keymaps, and seats coming and going move only the
  grabs that change; a sync with nothing changed sends no requests; an
  undefined key inside a sequence runs nothing.
- `key-names.lisp` - key names and keysym printing: kbd reads
  ISO_Left_Tab and the brightness, volume, and mute keys,
  key-description prints them back, and a keysym prints as its name,
  as its character (Latin-1, legacy, and Unicode keysyms), or as #N,
  in which-key text and the key-pressed log, while its binding still
  runs.
- `ipc.lisp` — command dispatch and a real unix-socket round-trip
  (server thread + client thread + main-thread queue drain).
- `display-brightness.lisp` - display menu brightness argv: signed
  steps use brightnessctl's relative N%+ / N%- syntax, absolute levels
  plain N%.
- `prompt-free-text.lisp`: Enter on a prompt opened without candidates
  completes with the typed text (and with nothing when it is empty),
  Enter on a candidate prompt still needs a matching candidate, and the
  shell prompt launches the command typed into it.
- `palette-message.lisp` - the message a command leaves on screen when
  run from the command palette or over IPC: the command's own message
  stays, a non-empty reply is shown when the command showed nothing,
  and an empty reply never replaces a message.
- `pull-hidden.lisp` - pull-hidden-next and its previous direction:
  hidden windows cycle in window-number order whatever order the
  workspace lists them in, wrapping at either end; unnumbered windows
  come after the numbered ones.
- `server-side-decorations.lisp`: decoration_hint handling, use_ssd
  and use_csd sent only in the manage pass, the
  `(:windows :decorations)` key, and set_tiled edges for tiled windows.
- `workspace-focus.lisp` - keyboard focus across workspace changes:
  switching to an empty workspace clears focus; switching back focuses
  the window last focused there, or the newest unminimized window
  there; moving the focused marked window off the visible workspace
  hands focus to a window that stays; and when the focused window is
  moved, the view follows the window and it keeps focus.
- `shm-buffers.lisp` - layer-surface shm buffers: repeated configures
  of one surface hold one shm file, and tearing down the bar, the
  message window, the prefix capture surface, the prompt or the eval
  prompt leaves none; a second teardown is harmless. Covers the
  compositor closing each overlay, a second message arriving before
  the first is configured or during a resize (one surface, one
  buffer), and reopening an overlay over a state that still holds a
  buffer.
