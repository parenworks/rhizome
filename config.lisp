;;; Rhizome configuration
;;;
;;; Configuration is a nested plist stored in *config*. Defaults are
;;; defined by DEFAULT-CONFIG; user overrides live in
;;; ~/.config/rhizome/config.lisp, which typically contains calls to
;;; (rhizome:configure '(...)). Configuration can be changed live from
;;; the REPL - the bar and other consumers pick changes up immediately.

(in-package #:rhizome)

;;; --- Config data structure helpers ---

(defun config-plist-p (list)
  "Whether LIST should be merged recursively as a config plist.
Config plists have keywords at even positions and non-keyword values
at odd positions. Module lists like (:layout :windows) have keywords in
value position and are therefore replaced wholesale on override."
  (and (listp list)
       (plusp (length list))
       (evenp (length list))
       (every #'keywordp (loop for x on list by #'cddr collect (first x)))
       (notany #'keywordp (loop for x on (rest list) by #'cddr collect (first x)))))

(defun merge-plist (base override)
  "Recursively merge OVERRIDE into BASE, returning a new plist.
Plist values merge recursively; everything else is replaced."
  (loop with merged = (copy-list base)
        for (key value) on override by #'cddr
        do (let ((existing (getf merged key)))
             (setf (getf merged key)
                   (if (and (config-plist-p existing)
                            (config-plist-p value))
                       (merge-plist existing value)
                       value)))
        finally (return merged)))

(defun config-get (path &key (config *config*))
  "Look up a config value by a list of keys, e.g. (:bar :height).
Returns (values value found-p)."
  (labels ((walk (keys plist)
             (let ((value (getf plist (first keys) '%missing)))
               (cond
                 ((eq value '%missing) (values nil nil))
                 ((rest keys)
                  (if (listp value)
                      (walk (rest keys) value)
                      (values nil nil)))
                 (t (values value t))))))
    (walk path config)))

(defun config-getf (path key &key (config *config*) (default nil default-p))
  "Convenience: get KEY inside the config group at PATH."
  (multiple-value-bind (group foundp)
      (config-get path :config config)
    (if foundp
        (multiple-value-bind (value vfoundp)
            (getf group key '%missing)
          (if (eq value '%missing)
              (if default-p default nil)
              (values value vfoundp)))
        (if default-p default nil))))

(defun parse-hex-color (string)
  "Parse \"#1a1b26\" into (values r g b)."
  (let ((clean (string-trim '(#\#) string)))
    (values
      (parse-integer clean :start 0 :end 2 :radix 16)
      (parse-integer clean :start 2 :end 4 :radix 16)
      (parse-integer clean :start 4 :end 6 :radix 16))))

;;; --- Defaults ---

(defun default-config ()
  "Return the default configuration plist."
  '(:bar (:enabled t
          :height 20
          :interval 0.15
          :padding 6
          :opacity 0.9
          :modules (:left (:workspaces :layout :windows)
                    :center (:clock)
                    :right (:tray :network :volume :battery)))
    :modules (:clock (:format "%a %d %b %H:%M")
              :battery (:interval 5)
              :volume (:interval 5
                       :on-click "pavucontrol"
                       :on-scroll-up "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"
                       :on-scroll-down "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
              :network (:interval 10
                        :on-click "nm-connection-editor"))
    :window-rules ()
    :windows (:decorations :server
              ;; :server asks every client that supports it for
              ;; server-side decorations (no client titlebar; rhizome
              ;; draws only borders). :client leaves clients to draw
              ;; their own titlebars.
              )
    :workspaces (:types nil
                 ;; (2 :floating): per-workspace group types,
                 ;; :tile (frame trees, the default) or :floating
                 ;; (StumpWM's floating group). The scroll ribbon
                 ;; is set live, through C-t l, or set-group-type
                 ;; from the rhizome-display module.
                 :default-layout :master-stack
                 ;; the layout untouched workspaces use;
                 ;; switch-layout stores per-workspace
                 )
    :gaps (:inner 6
           ;; Pixels of breathing room around each tiled window.
           ;; Adjacent windows show twice this between them, screen
           ;; edges once. 0 restores the classic flush tiling.
           )
    :frames (:prune-empty :auto
             ;; Which empty frames close. :auto keeps the frames a
             ;; user split makes and prunes a frame a new window split
             ;; open once it is empty; :never keeps every empty frame,
             ;; as StumpWM does; :always prunes every empty frame.
             )
    :animations (:enabled t
                 :duration-ms 200
                 ;; :easing :ease-out-cubic (default) | :linear |
                 ;; :ease-in-out — window glide: workspace slide-ins,
                 ;; expose gliding into the grid, float/maximize
                 :easing :ease-out-cubic)
    :window-state (:enabled t
                   ;; Remember identifier-based window placements
                   ;; across WM restarts (river v4 identifiers)
                   )
    :output (:presentation-mode :vsync
             ;; :async for tearing/lowest latency on high-refresh
             ;; panels; a render-sequence request (river v4)
             )
    ;; :warp-on-switch t warps the pointer to the output center on
    ;; workspace switch (river_seat_v1.pointer_warp); set it nil if
    ;; pointer jumps bother you. xcursor theme/size default to the
    ;; XCURSOR_THEME/XCURSOR_SIZE the session exports.
    :pointer (:warp-on-switch t)
    ;; :name selects themes/NAME.theme; :roles and :elements adjust
    ;; it (see theme.lisp)
    :theme (:name "tokyo-night"
            :font-family "monospace"
            :font-size 14)
    :launcher (:opacity 0.9
               :icon-size 20)
    :ipc (:enabled t)
    ;; Slynk REPL server on 127.0.0.1 (M-x sly-connect). A :swank
    ;; group in user config is still read when :slynk is left at
    ;; these defaults; see REPL-SERVER-SETTINGS.
    :slynk (:enabled t
            :port 4005)))

(defun repl-server-settings (&key (config *config*))
  "Return (values enabled-p port) for the REPL server in CONFIG.
Read the :slynk group. When CONFIG holds a :swank group and the
:slynk group still equals its default, read :swank instead, so a
configuration written for the earlier swank server keeps its port and
its :enabled switch. Missing keys default to enabled and port 4005."
  (let ((group (if (and (nth-value 1 (config-get '(:swank) :config config))
                        (equal (getf config :slynk)
                               (getf (default-config) :slynk)))
                   '(:swank)
                   '(:slynk))))
    (let ((env-port (let ((p (uiop:getenv "RHIZOME_SLYNK_PORT")))
                      (and p (plusp (length p))
                           (parse-integer p :junk-allowed t)))))
      (cond ((null env-port)
             (values (config-getf group :enabled :config config :default t)
                     (config-getf group :port :config config :default 4005)))
            ((zerop env-port)
             ;; RHIZOME_SLYNK_PORT=0: no REPL (the nested instance)
             (values nil 0))
            (t
             ;; the env var wins: a second rhizome cannot share the
             ;; config's port
             (values t env-port))))))

(defun border-rgba32 (r g b &optional (a #xff))
  "Return 8-bit channels R G B A (0-255, straight alpha) as four 32-bit
values for river_window_v1.set_borders: 0 is 0%, #xffffffff is 100%,
colour channels pre-multiplied by alpha."
  (flet ((scale (channel)
           (floor (* channel a #x01010101) #xff)))
    (values (scale r) (scale g) (scale b) (* a #x01010101))))

(defvar *config* (default-config)
  "The current rhizome configuration.")

;;; --- Public config API ---

(defun normalize-theme-selection (plist)
  "PLIST with a (:theme \"NAME\") entry rewritten as
(:theme (:name \"NAME\")), so a theme chosen by name merges into the
:theme group instead of replacing it."
  (let ((theme (getf plist :theme)))
    (if (stringp theme)
        (let ((copy (copy-list plist)))
          (setf (getf copy :theme) (list :name theme))
          copy)
        plist)))

(defun configure (plist)
  "Merge PLIST into *config* and apply it. For example:

  (rhizome:configure '(:bar (:modules (:left (:layout :windows)
                                          :center (:clock)
                                          :right (:battery))
                           :height 24)))
  (rhizome:configure '(:bar (:enabled nil)))   ; disable the bar
  (rhizome:configure '(:frames (:prune-empty :never)))  ; keep every empty frame
  ;; new windows fill an empty frame first (see placement.lisp)
  (rhizome:configure '(:tiling (:placement (:new-window (:empty :focused)))))
  (rhizome:configure '(:theme \"tokyo-night\"))  ; a theme by name

A theme name given as (:theme \"NAME\") sets (:theme :name) and keeps
the rest of the :theme group. Each deprecated colour key in PLIST
warns once (see theme.lisp).

Restarting running components (e.g. the bar) is handled automatically."
  (let ((plist (normalize-theme-selection plist)))
    (when (fboundp 'warn-deprecated-colour-keys)
      (funcall (symbol-function 'warn-deprecated-colour-keys) plist))
    ;; window rules belong to SOURCES now: the current one owns them
    (when (getf plist :window-rules)
      (set-window-rules (getf plist :window-rules))
      (remf plist :window-rules))
    (setf *config* (merge-plist *config* plist)))
  (apply-config)
  *config*)

(defun apply-config ()
  "Apply the current configuration to running components. Never
runs component restarts inside the bar process (the bar loading the
user config would otherwise spawn bars — see *bar-process-mode*)."
  (unless (and (boundp '*bar-process-mode*) *bar-process-mode*)
    (when (fboundp 'bar-apply-config)
      (funcall (symbol-function 'bar-apply-config)))
    (when (fboundp 'font-apply-config)
      (funcall (symbol-function 'font-apply-config)))
    (when (fboundp 'output-apply-config)
      (funcall (symbol-function 'output-apply-config)))))

(defun rhizome-home ()
  "The base directory for rhizome's per-instance state (snapshot,
takeover, window-state, bar log): $RHIZOME_STATE_HOME when set - a
second rhizome against a NESTED river keeps its own state instead
of clobbering the live session's - else the user's home."
  (or (let ((dir (uiop:getenv "RHIZOME_STATE_HOME")))
        (and dir (plusp (length dir))
             ;; ensure-directory-pathname: RHIZOME_STATE_HOME=/tmp/x
             ;; parses as a FILE named x in /tmp without it, and
             ;; merge-pathnames then drops it from every state path
             (uiop:ensure-directory-pathname dir)))
      (user-homedir-pathname)))

;;; *current-source* is defined (with its documentation) in
;;; sources.lisp, which loads after this file. Declared special
;;; here so reload-config's (let ((*current-source* :config)) ...)
;;; binds the dynamic variable, not a lexical one - a config
;;; file's rules landed under :repl for exactly that reason.
(defvar *current-source*)

(defun config-pathname ()
  "Pathname of the user config file."
  (merge-pathnames #P".config/rhizome/config.lisp"
                   (user-homedir-pathname)))

;;; --- Modifier keys (define once, use many) ---
;;;
;;; River's xkb bindings take modifier lists; these variables name the
;;; common ones so bindings read as intent, not bitmasks. *mod* is THE
;;; user modifier — every direct binding (MOD+Return terminal,
;;; MOD+1..9 workspaces, MOD+drag window moves, ...) routes through
;;; it. Default Super (:mod4); change at runtime with
;;; (rhizome:set-user-mod :alt) or :super.

(defparameter *mod-none*  '(:none)
  "No modifier.")
(defparameter *mod-shift* '(:shift)
  "Shift.")
(defparameter *mod-ctrl*  '(:ctrl)
  "Control (the C-t prefix chord).")
(defparameter *mod-alt*   '(:mod1)
  "Alt.")
(defparameter *mod-super* '(:mod4)
  "Super / Logo / Windows key.")

(defparameter *mod* *mod-super*
  "The user modifier: what direct keybindings and pointer grabs
(MOD+drag moves a window) are bound to. Defaults to Super; set to
*mod-alt* for the classic StumpWM-on-X11 Alt feel.")

(defun resolve-mod (mod)
  "Map a friendly modifier name (:alt, :super, :ctrl) to its modifier
list, or NIL if unknown."
  (case mod
    ((:alt :mod1) *mod-alt*)
    ((:super :logo :windows :mod4) *mod-super*)
    ((:ctrl :control) *mod-ctrl*)
    (t nil)))

;;; --- Fuzzy matching (prompt + launcher filtering) ---

(defun fuzzy-match-p (query candidate)
  "Whether QUERY's characters appear in CANDIDATE in order,
case-insensitively — a subsequence match that subsumes substring
matching (\"ff\" matches \"Firefox\", \"xf\" does not)."
  (loop with i = 0
        for ch across query
        for pos = (position ch candidate :start i :test #'char-equal)
        unless pos
          return nil
        do (setf i (1+ pos))
        finally (return t)))

(defun fuzzy-filter (query candidates)
  "Candidates matching QUERY, substring matches first, fuzzy-only
matches after; original order preserved within each group. An empty
query returns the candidates unchanged."
  (if (zerop (length query))
      candidates
      (let ((direct '()) (fuzzy '()))
        (dolist (c candidates)
          (cond ((null c))
                ((search query c :test #'char-equal) (push c direct))
                ((fuzzy-match-p query c) (push c fuzzy))))
        (nconc (nreverse direct) (nreverse fuzzy)))))

(defun load-user-config ()
  "Load the user's config file if it exists. The load IS the
:config source - whatever registers inside (rules, keybindings)
files under :config, whether from the startup load, a
reload-config, or a direct call."
  (let ((*current-source* :config)
        (path (config-pathname)))
    (when (probe-file path)
      (format t "~&; Rhizome: loading config from ~A~%" path)
      (handler-case
          (load path)
        (error (e)
          (format t "~&; Rhizome: error loading config: ~A~%" e))))))

(defun reload-config ()
  "Reset to defaults, forget loaded themes so theme files are read
again, replace the :config source's registrations (rules and
keybindings) by re-reading the file, and re-apply. A
binding or rule the file no longer makes is gone; everything a
MODULE or the REPL registered survives untouched (the source
model - see sources.lisp). The default keymaps stay in force."
  (setf *config* (default-config))
  (reset-config-source)
  (when (fboundp 'clear-theme-cache)
    (funcall 'clear-theme-cache))
  (let ((*current-source* :config))
    (load-user-config))
  (apply-config)
  *config*)
