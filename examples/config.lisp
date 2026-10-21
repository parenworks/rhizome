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

(configure '(:bar (:height 20                 ; px; match :font-size + headroom
                  :padding 6                  ; px at the segment edges
                  :background "#1a1b26"
                  :foreground "#c0caf5"
                  :accent "#7aa2f7"
                  :muted "#565f89"
                  :modules (:left (:workspaces :layout :windows)
                           :center (:clock)
                           :right (:tray :network :volume :battery)))))

;;; Breathing room between workspace numbers (px).
(configure '(:bar (:workspaces (:gap 8))))    ; default

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
;;; a 3px accent edge (the launcher's underline).

(configure '(:launcher (:background "#23142d"    ; deep purple
                       :accent "#7aa2f7"          ; selected-row edge
                       :prompt-color "#8c64aa"    ; "Run: " label
                       :query-color "#dcc8f0"     ; typed text
                       :cursor-color "#b4a0d2"
                       :selection-background "#503278"
                       :selected-color "#f0e6ff"
                       :entry-color "#aa96c8")))

;;; --- Window rules ----------------------------------------------------
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
                   (:match "gimp" :float t))))

;;; --- Idle lock -------------------------------------------------------
;;;
;;; Any ext-session-lock-v1 client works (swaylock, hyprlock,
;;; waylock). :idle-seconds arms swayidle at startup.

#-(and) (configure
  '(:lock (:command "swaylock"
            :args ("--daemonize")
            :idle-seconds 600)))
