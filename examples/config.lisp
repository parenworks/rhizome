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
                    :font-size 12              ; default
                    :focused-border "#7aa2f7"  ; Tokyo Night blue
                    :unfocused-border "#404040")))

;;; --- Bar ------------------------------------------------------------
;;;
;;; Modules: :workspaces (active underlined in accent, occupied in
;;; the foreground color, empty muted), :layout, :windows, :focused,
;;; :clock, :battery, :volume, :network, :tray, and (:icons (...))
;;; blocks with PNG app icons and click actions.

(configure '(:bar (:opacity 0.9                 ; translucent surfaces
                  :height 20                 ; px; match :font-size + headroom
                  :padding 6                  ; px at the segment edges
                  :background "#1a1b26"
                  :foreground "#c0caf5"
                  :accent "#7aa2f7"
                  :muted "#565f89"
                  :modules (:left (:workspaces :layout :windows)
                           :center (:clock)
                           :right (:tray :network :volume :battery)))))

;;; --- Workspaces ------------------------------------------------------
;;;
;;; Each workspace has its own TYPE and LAYOUT. Types:
;;;   :tile     — frame trees (the default)
;;;   :floating — StumpWM's floating group: every window floats,
;;;               arrivals cascade (C-t l cycles live, or
;;;               rhizome-cmd set-workspace-type <type> [N])
;;;   :scroll   — the ribbon

(configure '(:workspaces (:types nil
                         ;; (2 :floating 5 :scroll)
                         :default-layout :master-stack)))
                         ;; the layout untouched workspaces use;
                         ;; switch-layout stores per-workspace

;;; --- Gaps ------------------------------------------------------------
;;;
;;; Breathing room around each tiled window: adjacent windows show
;;; twice this between them, screen edges once. 0 = flush tiling.

(configure '(:gaps (:inner 6)))              ; default

;;; --- Animations ------------------------------------------------------
;;;
;;; Window glide: workspace slide-ins, expose gliding into the
;;; overview grid, float/maximize transitions. The layout math is
;;; untouched — a per-window decaying offset interpolates toward
;;; the final position, pumped at ~60fps.

(configure '(:animations (:enabled t          ; default
                         :duration-ms 200
                         :easing :ease-out-cubic)))

;;; Breathing room between workspace numbers (px).
(configure '(:bar (:workspaces (:gap 8))))    ; default

;;; Recording indicator: a red REC marker while a screencast or
;;; portal capture session is active (river v5). Opt in by adding
;;; :recording to a bar segment:
#-(and) (configure
  '(:bar (:modules (:right (:recording :tray :network :volume :battery))
          :recording-color "#f7768e")))

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
;;; Every surface color is configurable; the selected row also gets
;;; a 3px accent edge (the launcher's underline). Entries render
;;; their .desktop icons; surfaces are translucent (premultiplied
;;; ARGB).

(configure '(:launcher (:background "#23142d"    ; deep purple
                       :opacity 0.9
                       :icon-size 20
                       :accent "#7aa2f7"          ; selected-row edge
                       :prompt-color "#8c64aa"    ; "Run: " label
                       :query-color "#dcc8f0"     ; typed text
                       :cursor-color "#b4a0d2"
                       :selection-background "#503278"
                       :selected-color "#f0e6ff"
                       :entry-color "#aa96c8")))

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
;;; workspace and switches to it; :layout switches the output's
;;; layout when the rule fires.

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
