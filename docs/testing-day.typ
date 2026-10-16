// Stumpway Testing Day — a manual QA strategy
// Compile:  typst compile docs/testing-day.typ docs/testing-day.pdf

#set document(
  title: "Stumpway Testing Day",
  author: "Glenn Thompson",
)
#set page(paper: "a4", margin: 2.2cm, numbering: "1 / 1")
#set text(font: ("Adwaita Sans", "Noto Sans"), size: 10pt, lang: "en")
#show raw: set text(font: ("JetBrainsMono NF", "Adwaita Mono"))
#set heading(numbering: "1.1")
#set par(leading: 0.65em)
#set enum(indent: 1em)
#set list(indent: 1em, marker: ([–]))

#let cb = box(baseline: 0.05em)[#h(0.1em) ☐#h(0.25em)]

#align(center)[
  #text(size: 24pt, weight: "bold")[Stumpway Testing Day]
  #v(0.3em)
  #text(size: 12pt)[A manual QA strategy for the stumpway window manager]
  #v(0.3em)
  #text(size: 10pt, fill: luma(100))[River + StumpWM-parity stack — Sunday edition]
]

#v(1em)

= Goal and method

Stumpway now has broad feature coverage; this plan puts it through its
paces as a *daily driver* under real usage, not just unit conditions.
The strategy has three layers:

+ *Regression sweep* — run the automated suite first, so manual
  testing starts from a known-good base.
+ *Feature circuits* — scripted walks through each feature area with
  concrete steps and expected results (the bulk of this document).
+ *Soak testing* — use stumpway as your only window manager for the
  day: browse, edit, chat, take screenshots, switch workspaces under
  load, and note anything that breaks, flickers, or feels wrong.

Mark each item with a result. Anything that fails: note the section
number, what you did, what happened — the capture guide in the
appendix turns that into a fixable report.

== Preconditions

#cb Install missing helpers: `sudo pacman -S swaylock swayidle` (lock
screen — MOD+Shift+L is a no-op without them).
#cb Fresh login: pick the stumpway (river) session at the SDDM
greeter. Watch the first 10 seconds: one bar, one wallpaper process,
both autostart apps (variety tray-less in tray, flameshot daemon).
#cb Automated suite green (next section).

== The automated suite (15 min)

Run each from the repo root; every line must end in
`0 failed`:

```
for t in workspace-restore workspace-switch-away window-rules \
         hooks-and-eval ipc prefix prompt-and-message floating \
         frames parity quickwins interactivity medium3 bigger1 \
         session-icons bar-guard expose; do
  echo "== $t"; sbcl --non-interactive --load tests/$t.lisp
done
```

#table(
  columns: (auto, 1fr, auto),
  [Suite], [What it covers], [Result],
  [frames (57)], [Tree ops, dynamic tiling, dumps, refits], [#cb],
  [expose (22)], [Grid, pick paths, exact restore], [#cb],
  [bigger1 (27)], [Minor modes, WSE, fuzzy], [#cb],
  [session-icons (25)], [Lock/logout commands, bar icons], [#cb],
  [the rest (179)], [Everything else, per README], [#cb],
)

Total: 335 checks. If any fail, stop — manual testing is pointless
on a broken base.

= Feature circuits

== B. Dynamic tiling (the new core)

The behavior under the most scrutiny today: every new window should
get its own frame.

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [On an empty workspace press MOD+Return twice], [First terminal fills the screen; the second splits it side-by-side],
  [#cb], [MOD+Return again (3rd, 4th, 5th window)], [Each arrival splits the frame you are in along its longer axis; the pointer follows the newest window],
  [#cb], [C-t R (remove split) after several arrivals], [Frames merge pairwise into their sibling; windows stack one-per-frame again],
  [#cb], [Close windows one by one with several open], [The empty frame is reclaimed: neighbors grow automatically — no manual C-t R needed],
  [#cb], [C-t Q (only) with several frames], [Whole workspace collapses to one frame; all windows in it, cycle with C-t n],
  [#cb], [C-t B (balance) after manual resizing], [Every split returns to 50/50],
  [#cb], [Open 8+ windows], [Splits stop below twice the minimum frame size (no sliver frames); still stable],
  [#cb], [C-t n / C-t p inside a multi-window frame], [Pulls the next/previous hidden window into the current frame],
)

== C. Frame tree surgery

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [C-t s, C-t S], [Manual side-by-side and stacked splits; focus moves to the new empty frame],
  [#cb], [C-t arrows], [Focus moves to the nearest frame in that direction, preferring edge-sharing neighbors],
  [#cb], [C-t +/-], [Dividing line nudges ±30px; iresize (C-t r) then arrows works continuously, Enter commits, Escape cancels],
  [#cb], [C-t M + arrows], [Moves the focused window into that frame],
  [#cb], [C-t e + arrows], [Swaps the current frame's window with the target frame's (focus follows the arrival)],
  [#cb], [Switch layout: `stumpway-cmd switch-layout master-stack`], [Tree rebuilds as master/stack; windows distributed one per frame],
  [#cb], [`stumpway-cmd dump-frame proj` then open/close windows, then `restore-frame proj`], [Layout returns; windows placed by number; missing windows skipped without crash],
)

== D. Expose (C-t Return)

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [With 3+ windows on the workspace: C-t Return], [All windows (including hidden-in-frame ones) float into an aspect-fitted grid; a hint message appears],
  [#cb], [Press 1–9 / 0], [That grid slot is picked; layout restores exactly and that window focuses],
  [#cb], [Arrows, then Return], [Focus walks the grid; Return picks the focused window],
  [#cb], [Click a window in the grid], [Pick completes with that window],
  [#cb], [Escape], [Layout and focus restore to pre-expose state exactly — float positions, frame order, focus],
  [#cb], [Wait 60s mid-expose], [Timeout restores safely on its own],
  [#cb], [One window only: C-t Return], ["nothing to expose" message, no state change],
)

== E. Floats and pointer operations

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [C-t f on a tiled window], [Floats in place (no jump); "floated" message],
  [#cb], [MOD+Left-drag a float], [Moves; alpha-blended border follows],
  [#cb], [MOD+Left-drag a *tiled* window], [StumpWM-style: it floats first, then moves],
  [#cb], [MOD+Right-drag a float], [Resizes from the bottom-right; respects min/max size hints],
  [#cb], [Drag a float mostly off-screen, release], [Clamped back so ~30px stays reachable],
  [#cb], [Titlebar-drag a CSD app (GIMP toolbox, if installed)], [Moves without floating it again],
  [#cb], [C-t f again to unfloat], [Returns to tiling in the current frame],
  [#cb], [Drag a float across a workspace switch (MOD+1..9 while dragging is fine to skip) ], [No crash; float lands on its workspace],
)

== F. Workspaces, rules, marks

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [MOD+1..9 / C-t 1..9], [Direct switch; bar highlights; hidden windows restore],
  [#cb], [MOD+Shift+1..9], [Moves the focused window and follows it],
  [#cb], [C-t N / C-t P], [Next/previous workspace],
  [#cb], [`stumpway-cmd renumber` after closing windows], [Numbers compact 1..N; C-t ' selects by number],
  [#cb], [C-t \# then `stumpway-cmd move-marked 3`], [Marked window (star) moves; marks clear],
  [#cb], [Set a rule: `(configure '(:window-rules ((:match "foot" :workspace 4))))`, then launch a terminal], [It opens on workspace 4; check via C-t w (windowlist shows workspace numbers)],
  [#cb], [Same with a `:wse` rule: `(:wse (:and (:app-id "foot") (:float nil)) :float t)`], [Matching windows float automatically],
)

== G. Prompts, launcher, menus (fuzzy)

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [MOD+D, type "ff"], ["Firefox"-style fuzzy hits rank after exact substring matches; the just-installed Variety appears],
  [#cb], [MOD+W (windowlist)], [Numbers shown as `N: [ws] title`; marked windows show a star; Enter jumps to its workspace and focuses],
  [#cb], [MOD+X (command palette), type "bal"], [Fuzzy filters; Enter runs balance; the message window confirms],
  [#cb], [C-t ' then a number], [Selects that window by number],
  [#cb], [Tab/Up/Down in any prompt], [Selection cycles; Escape cancels cleanly every time],
  [#cb], [Leave a prompt open 30s], [Auto-closes; keyboard never trapped],
)

== H. Bar

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [Click workspace numbers in the bar], [Switches workspaces via IPC],
  [#cb], [Click the flameshot icon], [Opens flameshot gui],
  [#cb], [Click the variety icon], [Launches/raises Variety preferences],
  [#cb], [Scroll on the volume module], [Volume steps 5%],
  [#cb], [C-t b / MOD+B], [Bar hides and returns; frame trees refit — windows never tiled under or floating over the bar],
  [#cb], [`(configure '(:bar (:height 30)))`], [Bar restarts taller; windows re-tile under it immediately],
)

== I. Session management

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [MOD+Shift+L], [Locks (needs swaylock installed); password unlocks],
  [#cb], [MOD+Shift+E once], ["press again to log out" message],
  [#cb], [MOD+Shift+E twice within 3s], [Session ends; SDDM greeter returns; log back in cleanly],
  [#cb], [`stumpway-cmd suspend`], [Machine suspends; wake restores session with input working],
  [#cb], [MOD+Shift+Q / R], [Confirm-style behavior consistent; power actions prompt polkit as configured],
  [#cb], [C-t q twice / once], [Quit needs the second press; a single press just warns],
)

== J. Prefix chains, help, modes

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [Press C-t and pause], [Which-key list shows every binding with docs (multi-line message window)],
  [#cb], [C-t h then descend b/c/f/v/k/w], [Each shows its output; k then a key describes it without running it],
  [#cb], [C-t C (command mode), run 3 commands, Escape], [Stays armed between commands; which-key visible; Escape exits],
  [#cb], [`(enable-minor-mode :test)` with a bound key], [Mode binding shadows the prefix binding while enabled; disable restores],
  [#cb], [C-t m (lastmsg), C-t a (time)], [Both show in the message window; lastmsg re-shows the previous text],
  [#cb], [Unbound key in a chain], ["unbound key" message, clean exit],
  [#cb], [`(set-prefix-key "C-z")`, try C-z then C-t], [C-z now opens chains; C-t is inert; set it back with `"(stumpway:set-prefix-key \"C-t\")"`],
  [#cb], [`(set-user-mod :alt)` and back to `:super`], [MOD bindings re-grab in both directions; MOD+drag follows],
)

== K. IPC and REPL robustness

#table(
  columns: (auto, 1fr, 1fr),
  [ ], [Action], [Expected],
  [#cb], [`stumpway-cmd current-workspace`, `wse (:float t)`, `renumber`], [All reply in the message window without breaking the WM],
  [#cb], [`stumpway-cmd '(+ 1 2)'`], [Evaluates; reply shows in the message window],
  [#cb], [M-x slime-connect (or `stumpway-cmd`) while windows are moving], [No request from a foreign thread errors in the log],
  [#cb], [Flood: hold MOD+J/K for 10s, then C-t chains immediately], [No input loss, no stuck modes],
  [#cb], [`stumpway-cmd lock` twice in a row], [Second lock doesn't spawn duplicates (swaylock may refuse politely)],
)

= Soak testing (the afternoon)

Use stumpway as your only session for 3–4 hours. Keep this list open
and note anything that matches:

#table(
  columns: (auto, 1fr),
  [Watch for], [Notes],
  [#cb Flicker], [Bar redraws, message window flashes, window borders re-drawn repeatedly — check the WM log for manage/render churn (a past bug class)],
  [#cb Focus fights], [Auto-focus stealing, clicks not moving focus, expose leaving stale focus],
  [#cb Slow chains], [C-t taking > 100ms, launcher scrolling hitching],
  [#cb Workspace drift], [Windows landing on wrong workspaces via rules, or losing their frame order after switches],
  [#cb Log growth], [Tail `~/.cache/stumpway/stumpway.log` and `bar.log` — idle should add nothing; bursts during drag/resize are expected but should stop on release],
  [#cb Memory], [`ps -o rss -p $(pgrep -f stumpway:run)` at start vs end of the afternoon; >2x growth is worth investigating],
)

Suggested load: 8–12 windows across 4+ workspaces, a browser with
video, an editor, two terminals, one GIMP-style float if available.
Exercise expose, splits, and workspace switches while the video plays.

= Advanced circuits (if the morning goes well)

- *Hot-reload resilience:* `(stumpway:configure '(:bar (:height 26)))`
  from `stumpway-cmd`, then `ql:quickload` a changed file via the eval
  prompt (C-t :). The WM should survive; note any stuck mode or lost
  focus. Recovery lever: `C-t q q` then relogin.
- *Multi-bar safety:* confirm exactly one bar process at all times:
  `pgrep -fc "stumpway:run-bar"` must read `1` (the fork-bomb guard).
- *Timeout paths:* enter iresize, expose, command mode and just wait
  60s in each — all three must restore their state cleanly on expiry.
- *Float + expose interplay:* a pre-existing float plus dynamic tiles,
  expose, pick, repeat — float geometry must survive every round trip.

= Appendix A: capture guide (how to report a failure)

- Section number + exact steps + what happened vs expected.
- `tail -50 ~/.cache/stumpway/stumpway.log` (WM) and
  `~/.cache/stumpway/bar.log`.
- For input/mode bugs: which modes were active (C-t h b lists minor
  modes), whether a timeout or manual exit was involved.
- One-liner repro if you can: the `stumpway-cmd` or key sequence.

= Appendix B: quick reference

#table(
  columns: (auto, 1fr, auto, 1fr),
  [Key], [Action], [Key], [Action],
  [MOD+Return], [Terminal], [C-t s / S], [Split / stacked split],
  [MOD+J / K], [Cycle frame window], [C-t R], [Remove split],
  [MOD+W / G / X], [Windowlist / picker / palette], [C-t +/-], [Resize split],
  [MOD+D], [Launcher (fuzzy)], [C-t r], [Interactive resize],
  [MOD+B], [Toggle bar], [C-t Q], [Only (one frame)],
  [MOD+1..9], [Workspace], [C-t B], [Balance splits],
  [MOD+Shift+1..9], [Move window → ws], [C-t Return], [Expose],
  [MOD+Shift+L], [Lock], [C-t f], [Float/tile],
  [MOD+Shift+E], [Logout (×2)], [C-t \#], [Mark window],
  [C-t q], [Quit (×2)], [C-t m], [Last message],
  [C-t h], [Help chains], [C-t C], [Command mode],
)

#v(1em)
#align(center)[Text size: 10pt. Print me, tick me, break things.]
