;;; Rhizome example configuration
;;;
;;; Copy to ~/.config/rhizome/config.lisp and edit to taste. The
;;; file is loaded at startup (and by (rhizome:reload-config));
;;; (rhizome:configure ...) merges overrides into the defaults and
;;; applies them live — the bar restarts automatically, in-process
;;; surfaces (launcher, message window, prompts) redraw immediately.
;;;
;;; Everything below is optional: each block is independent and the
;;; defaults are shown commented where they differ.

(in-package #:rhizome)

;;; --- Theming: fonts and window borders -----------------------------
;;;
;;; The font goes through fontconfig, so plain family names work
;;; ("monospace", "sans-serif", "JetBrains Mono") and a missing
;;; family falls back to the system's best match. Without freetype
;;; or fontconfig every surface degrades to the built-in 8x16
;;; bitmap font with identical metrics.

(configure '(:theme (:font-family "monospace"   ; default
                    :font-size 14)))           ; default

;;; Colours come from a theme: themes/NAME.theme in
;;; ~/.config/rhizome/themes/ or in rhizome's own themes/ directory.
;;; A role colours every element built on it; an element override
;;; colours one surface. See "Theming" in README.org for the roles,
;;; and *theme-elements* in theme.lisp for the elements.

(configure '(:theme "tokyo-night"))             ; default
#-(and) (configure
  '(:theme (:roles (:accent "#7aa2f7"          ; borders, bar, prompts
                    :unfocus "#404040")
            :elements (:bar-workspace-empty "#565f89"
                       :launcher-match :accent))))

;;; --- Bar ------------------------------------------------------------
;;;
;;; Modules: :workspaces (active on an accent pill, occupied in
;;; the foreground colour, empty muted), :layout, :windows, :focused,
;;; :clock, :battery, :volume, :network, :tray, and (:icons (...))
;;; blocks with PNG app icons and click actions.

(configure '(:bar (:opacity 0.9                 ; translucent surfaces
                  :height 20                 ; px; match :font-size + headroom
                  :padding 6                  ; px at the segment edges
                  :modules (:left (:workspaces :layout :windows)
                           :center (:clock)
                           :right (:tray :network :volume :battery)))))

;;; --- Workspaces ------------------------------------------------------
;;;
;;; Each workspace has its own TYPE and LAYOUT. Types in :types:
;;;   :tile     - frame trees (the default)
;;;   :floating - StumpWM's floating group: every window floats,
;;;               arrivals cascade
;;; The scroll ribbon is a third type, but only live: C-t l cycles the
;;; current workspace through tile, scroll, and floating. With the
;;; rhizome-display module loaded, rhizome-cmd set-group-type
;;; tile|scroll|floating sets it too. A :scroll entry in :types makes
;;; no ribbon.

(configure '(:workspaces (:types nil
                         ;; (2 :floating)
                         :default-layout :master-stack)))
                         ;; the layout untouched workspaces use;
                         ;; switch-layout stores per-workspace

;;; --- Gaps ------------------------------------------------------------
;;;
;;; Breathing room around each tiled window: adjacent windows show
;;; twice this between them, screen edges once. 0 = flush tiling.

(configure '(:gaps (:inner 6)))              ; default

;;; --- Frames ----------------------------------------------------------
;;;
;;; Which empty frames close. :auto keeps the frames you split and
;;; closes a frame a new window split open once its window goes.
;;; :never keeps every empty frame, as StumpWM does. :always closes
;;; every empty frame.
;;;
;;; Placement is a policy, StumpWM's by default:
;;;
;;;   (configure '(:tiling (:placement (:policy :stumpwm
;;;                                     :new-frame :last-window
;;;                                     :new-window (:focused)))))
;;;
;;; A split shows the most recently focused hidden window in its new
;;; half (:new-frame :empty leaves it empty), and a new window enters
;;; the focused frame. :new-window is a preference list over :focused,
;;; :last, :empty, :unfocused, or a function of (output workspace
;;; window history); the first entry that yields a frame wins.
;;; Auto-split still divides an occupied frame. To send the next new
;;; window into the empty frame a split left:
;;;
;;;   (configure '(:tiling (:placement (:new-window (:empty :focused)))))

(configure '(:frames (:prune-empty :auto)))  ; default

;;; --- Animations ------------------------------------------------------
;;;
;;; Window glide: workspace slide-ins, expose gliding into the
;;; overview grid, float/maximize transitions. The layout math is
;;; untouched — a per-window decaying offset interpolates toward
;;; the final position, pumped at ~60fps.

(configure '(:animations (:enabled t          ; default
                         :duration-ms 200
                         :easing :ease-out-cubic)))

;;; --- Debug logging ---------------------------------------------------
;;;
;;; Per-event [debug] lines are OFF by default: the animation pump
;;; drives sequences at frame cadence, and a format per event is
;;; hundreds of log lines per second of glide. Turn on when
;;; diagnosing protocol behavior:

#-(and) (configure '(:debug (:events t)))

;;; --- Pointer menus ---------------------------------------------------
;;;
;;; MOD+Middle-click on any window opens the context menu at the
;;; pointer (hover to highlight, click to select, Escape closes).

(configure '(:menus (:opacity 0.95)))          ; default; colours: :menu-*

;;; --- Titlebars ----------------------------------------------------
;;;
;;; :server asks every client that supports it for server-side
;;; decorations, so windows draw no titlebar of their own. :client
;;; leaves clients to draw their own titlebars and buttons.

(configure '(:windows (:decorations :server)))  ; default

;;; --- Decoration shadows ----------------------------------------------
;;;
;;; river_decoration_v1 pseudo-shadows on floating windows,
;;; accent-tinted when focused. Set :enabled nil for flat floats.

(configure '(:decorations (:enabled t         ; default
                           :radius 12
                           :alpha 0.4
                           :focused-alpha 0.6)))

;;; Breathing room between workspace numbers (px).
(configure '(:bar (:workspaces (:gap 8))))    ; default

;;; Recording indicator: a red REC marker while a screencast or
;;; portal capture session is active (river v5). Opt in by adding
;;; :recording to a bar segment:
#-(and) (configure
  '(:bar (:modules (:right (:recording :tray :network :volume :battery)))
    :theme (:elements (:bar-recording "#f7768e"))))

(configure '(:modules (:clock (:format "%a %d %b %H:%M")  ; default
              :battery (:interval 5)
              :volume (:interval 5
                       :on-click "pavucontrol"
                       :on-scroll-up "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"
                       :on-scroll-down "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
              :network (:interval 10
                        :on-click "nm-connection-editor"))))

;;; Launcher icons in the bar, each with its own click action:
#-(and) (configure
  '(:bar (:modules (:left ((:icons ((:icon "flameshot" :on-click "flameshot gui")
                                     (:icon "variety" :on-click "variety")))
                           :workspaces)
                   :center (:clock)
                   :right (:volume :network :battery)))))

;;; --- App launcher (MOD+D) --------------------------------------------
;;;
;;; Every surface colour is a theme element; the selected row sits on
;;; a pill. Entries render their .desktop icons; surfaces are
;;; translucent (premultiplied ARGB).

(configure '(:launcher (:opacity 0.9
                       :icon-size 20)))
#-(and) (configure
  '(:theme (:elements (:launcher-background "#23142d"  ; deep purple
                       :launcher-match "#7aa2f7"       ; typed text in entries
                       :launcher-prompt "#8c64aa"      ; "Run: " label
                       :launcher-query "#dcc8f0"       ; typed text
                       :launcher-cursor "#b4a0d2"
                       :launcher-selection "#503278"
                       :launcher-selected "#f0e6ff"
                       :launcher-entry "#aa96c8"))))

;;; --- Window rules ----------------------------------------------------
;;;
;;; New windows (tiled or floated) automatically get a
;;; configure_bounds hint of the output's usable area
;;; (river_window_v1.set_dimension_bounds), so apps stop opening
;;; larger than the screen.
;;;
;;; :match is a case-insensitive regex against the app-id (use
;;; :key :title or :key :any for titles). :float t starts the
;;; window floating; :workspace N + :follow t opens it on that
;;; workspace and switches to it; :layout gives the window's
;;; workspace that layout and rebuilds its frames, as switch-layout
;;; does. A workspace that already has the rule's layout keeps its
;;; frames when the rule matches again.

#-(and) (configure
  '(:window-rules ((:match "vivaldi" :workspace 2 :follow t)
                   (:match "emacs" :workspace 3 :layout :monocle)
                   (:match "spotify" :key :title :workspace 4)
                   (:match "known-id" :key :identifier :workspace 5)
                   (:match "gimp" :float t))))

;;; Windows with a stable identifier (river v4) are remembered:
;;; their workspace and float state survive WM restarts and
;;; relogins, restored before the rules above run. Disable if you
;;; want every login to start fresh:

(configure '(:window-state (:enabled t)))      ; default

;;; --- Presentation mode (VRR / tearing) -----------------------------
;;;
;;; :vsync (default) page-flips at vblank; :async flips immediately
;;; — visible tearing in exchange for the lowest latency. Per-output,
;;; a render-sequence request (river v4).

(configure '(:output (:presentation-mode :vsync)))   ; default

;;; --- Idle lock -------------------------------------------------------
;;;
;;; Any ext-session-lock-v1 client works (swaylock, hyprlock,
;;; waylock). :idle-seconds arms swayidle at startup.

#-(and) (configure
  '(:lock (:command "swaylock"
            :args ("--daemonize")
            :idle-seconds 600)))

;;; --- Pointer ---------------------------------------------------------
;;;
;;; On workspace switch the pointer warps to the output's center
;;; (river_seat_v1.pointer_warp) so it lands on the new
;;; workspace's windows. Some people find pointer jumps jarring:

(configure '(:pointer (:warp-on-switch t)))  ; default

;;; Compositor-rendered cursor theme and size; defaults come from
;;; the XCURSOR_THEME/XCURSOR_SIZE the session exports.
#-(and) (configure
  '(:pointer (:xcursor-theme "Bibata-Modern-Classic"
              :xcursor-size 24)))
