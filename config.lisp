;;; Stumpway configuration
;;;
;;; Configuration is a nested plist stored in *config*. Defaults are
;;; defined by DEFAULT-CONFIG; user overrides live in
;;; ~/.config/stumpway/config.lisp, which typically contains calls to
;;; (stumpway:configure '(...)). Configuration can be changed live from
;;; the REPL - the bar and other consumers pick changes up immediately.

(in-package #:stumpway)

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
          :background "#1a1b26"
          :foreground "#c0caf5"
          :accent "#7aa2f7"
          :muted "#565f89"
          :modules (:left (:workspaces :layout :windows)
                    :center (:clock)
                    :right (:network :volume :battery)))
    :modules (:clock (:format "%a %d %b %H:%M")
              :battery (:interval 5)
              :volume (:interval 5
                       :on-click "pavucontrol"
                       :on-scroll-up "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"
                       :on-scroll-down "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
              :network (:interval 10
                        :on-click "nm-connection-editor"))
    :window-rules ()
    :ipc (:enabled t)
    :swank (:enabled t
            :port 4005)))

(defvar *config* (default-config)
  "The current stumpway configuration.")

;;; --- Public config API ---

(defun configure (plist)
  "Merge PLIST into *config* and apply it. For example:

  (stumpway:configure '(:bar (:modules (:left (:layout :windows)
                                          :center (:clock)
                                          :right (:battery))
                           :height 24)))
  (stumpway:configure '(:bar (:enabled nil)))   ; disable the bar

Restarting running components (e.g. the bar) is handled automatically."
  (setf *config* (merge-plist *config* plist))
  (apply-config)
  *config*)

(defun apply-config ()
  "Apply the current configuration to running components."
  (when (fboundp 'bar-apply-config)
    (funcall (symbol-function 'bar-apply-config))))

(defun config-pathname ()
  "Pathname of the user config file."
  (merge-pathnames #P".config/stumpway/config.lisp"
                   (user-homedir-pathname)))

;;; --- Modifier keys (define once, use many) ---
;;;
;;; River's xkb bindings take modifier lists; these variables name the
;;; common ones so bindings read as intent, not bitmasks. *mod* is THE
;;; user modifier — every direct binding (MOD+Return terminal,
;;; MOD+1..9 workspaces, MOD+drag window moves, ...) routes through
;;; it. Default Super (:mod4); change at runtime with
;;; (stumpway:set-user-mod :alt) or :super.

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
  "Load the user's config file if it exists."
  (let ((path (config-pathname)))
    (when (probe-file path)
      (format t "~&; Stumpway: loading config from ~A~%" path)
      (handler-case
          (load path)
        (error (e)
          (format t "~&; Stumpway: error loading config: ~A~%" e))))))

(defun reload-config ()
  "Reset to defaults, reload the user config file, and re-apply."
  (setf *config* (default-config))
  (load-user-config)
  (apply-config)
  *config*)
