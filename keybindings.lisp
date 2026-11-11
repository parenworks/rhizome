;;; Keyboard bindings for Rhizome
;;;
;;; Uses the river-xkb-bindings-v1 protocol to register key
;;; bindings with the compositor. Bindings are defined with
;;; keysyms and modifiers, and trigger callbacks when pressed.
;;;
;;; Default bindings (StumpWM-style; MOD is the user modifier — Super
;;; by default, see *mod* in config.lisp):
;;;   Ctrl+T       - prefix key (StumpWM-style keychains)
;;;   MOD+Return   - launch terminal (foot)
;;;   MOD+Q        - close focused window
;;;   MOD+J        - focus next window
;;;   MOD+K        - focus previous window
;;;   MOD+Space    - cycle layout
;;;   MOD+1..9     - switch to workspace 1..9
;;;   MOD+Shift+1..9 - move focused window to workspace 1..9
;;;   MOD+E        - launch emacs
;;;   MOD+D        - application launcher (native layer-shell popup)
;;;   MOD+B        - toggle the status bar
;;;   Print        - screenshot of full screen (grim)
;;;   Shift+Print  - screenshot of selected region (grim + slurp)
;;;   MOD+W        - window list (pick, jump to window)
;;;   MOD+G        - workspace picker
;;;   MOD+X        - command palette
;;;   MOD+Shift+Q  - power off the machine
;;;   MOD+Shift+R  - reboot the machine
;;;   MOD+;        - live eval prompt (evaluate Lisp in the running WM)

(in-package #:rhizome)

(defvar *xkb-bindings-proxy* nil
  "The river_xkb_bindings_v1 proxy.")

(defvar *default-top-map*
  (make-keymap :name :default-top
               :doc "Rhizome's direct key bindings, built from *mod* and
*prefix-key-binding*.")
  "The default layer of direct bindings: (keysym . modifiers) ->
function. Only the default builders write it.")

(defvar *top-map*
  (make-keymap :name :top
               :doc "User direct key bindings."
               :parents (list *default-top-map*))
  "The user layer of direct bindings: (keysym . modifiers) -> function.
define-key writes it; *default-top-map* is its parent, so a user
binding shadows a default on the same key.")

(defvar *active-bindings* nil
  "List of active river_xkb_binding_v1 proxy objects.")

(defvar *grabbed-keys* (make-hash-table :test 'equal)
  "Maps (seat-proxy . key) to the river_xkb_binding_v1 grabbing it.")

(defvar *default-keymaps-built* nil
  "True once setup-default-keybindings has built the default keymaps.")

(defvar *default-top-map-mod* nil
  "The *mod* value *default-top-map* was last built with.")

;;; --- xkb keysym constants ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +xkb-key-Return+ 65293)    ; #xff0d
  (defconstant +xkb-key-Tab+     65289)    ; #xff09
  (defconstant +xkb-key-Space+   32)       ; #x0020
  (defconstant +xkb-key-Q+       113)      ; #x0071
  (defconstant +xkb-key-J+       106)      ; #x006a
  (defconstant +xkb-key-K+       107)      ; #x006b
  (defconstant +xkb-key-B+        98)       ; #x0062
  (defconstant +xkb-key-D+       100)      ; #x0064
  (defconstant +xkb-key-F+       102)      ; #x0066
  (defconstant +xkb-key-1+       49)       ; #x0031
  (defconstant +xkb-key-2+       50)       ; #x0032
  (defconstant +xkb-key-3+       51)       ; #x0033
  (defconstant +xkb-key-4+       52)       ; #x0034
  (defconstant +xkb-key-5+       53)       ; #x0035
  (defconstant +xkb-key-6+       54)       ; #x0036
  (defconstant +xkb-key-7+       55)       ; #x0037
  (defconstant +xkb-key-8+       56)       ; #x0038
  (defconstant +xkb-key-9+       57)       ; #x0039
  (defconstant +xkb-key-E+       101)      ; #x0065
  (defconstant +xkb-key-P+       112)      ; #x0070
  (defconstant +xkb-key-Print+   65377)    ; #xff61
  (defconstant +xkb-key-Semicolon+ 59)      ; #x003b
  (defconstant +xkb-key-R+       114)      ; #x0072
  (defconstant +xkb-key-W+       119)      ; #x0077
  (defconstant +xkb-key-T+       116)      ; #x0074
  (defconstant +xkb-key-G+       103)      ; #x0067
  (defconstant +xkb-key-X+       120)      ; #x0078
  (defconstant +xkb-key-Escape+  65307)   ; #xff1b
  (defconstant +xkb-key-Comma+   44)       ; #x002c
  (defconstant +xkb-key-Period+  46))      ; #x002e

;;; --- Modifier bitfield constants ---
;;; Wayflan expects bitfield enums as lists of keywords. The
;;; definitions (*mod*, *mod-alt*, *mod-super*, ...) live in
;;; config.lisp, which loads before both this file and seat.lisp.

;;; --- Keybinding registration ---

(defun setup-xkb-bindings (registry name interface version)
  "Bind to river_xkb_bindings_v1 if advertised."
  (when (string= interface "river_xkb_bindings_v1")
    (let ((iface-class (wl:find-interface-named interface)))
      (unless iface-class
        (format t "~&; Rhizome: xkb-bindings interface not found!~%")
        (return-from setup-xkb-bindings))
      (setf *xkb-bindings-proxy*
            (wl:wl-registry.bind registry name iface-class version))
      (format t "~&; Rhizome: bound river_xkb_bindings_v1 (version ~D)~%" version))))

(defun define-key (keysym modifiers callback)
  "Bind KEYSYM with MODIFIERS (a river modifier list) to CALLBACK, a
function of no arguments, in the user layer *top-map*. While the
defaults are built, write *default-top-map* instead. A NIL CALLBACK
removes the user binding. Returns CALLBACK."
  (keymap-bind (if *defining-default-bindings* *default-top-map* *top-map*)
               (cons keysym modifiers)
               callback))

(defun key-binding (keysym modifiers)
  "Return the effective direct binding of KEYSYM with MODIFIERS, the
user layer first, or NIL when the key is unbound."
  (values (keymap-lookup *top-map* (cons keysym modifiers))))

(defun grab-key (seat-proxy key)
  "Create and enable a river grab of KEY, a (keysym . modifiers) cons,
on SEAT-PROXY. A press runs the binding key-binding returns at press
time, so rebinding a grabbed key needs no new grab."
  (let* ((keysym (car key))
         (modifiers (cdr key))
         (binding (rwm:river-xkb-bindings-v1.get-xkb-binding
                   *xkb-bindings-proxy* seat-proxy keysym modifiers)))
    (push (lambda (event-name &rest args)
            (declare (ignore args))
            (when (eq event-name :pressed)
              (format t "~&; Rhizome: keybinding pressed (keysym=~A mod=~A)~%"
                      keysym modifiers)
              (let ((callback (key-binding keysym modifiers)))
                (when (functionp callback)
                  (funcall callback)))))
          (wl:wl-proxy-hooks binding))
    (rwm:river-xkb-binding-v1.enable binding)
    (setf (gethash (cons seat-proxy key) *grabbed-keys*) binding)
    (push binding *active-bindings*)
    binding))

(defun install-keybindings ()
  "Grab every key bound in *top-map* (inherited defaults included) on
every seat that lacks a grab for it. Must be called during a manage
sequence."
  (when (null *xkb-bindings-proxy*)
    (format t "~&; Rhizome: no xkb-bindings proxy, skipping keybindings~%")
    (return-from install-keybindings))
  (let ((created 0))
    (loop for seat-proxy being the hash-keys of *seats*
          do (map-keymap (lambda (key binding)
                           (declare (ignore binding))
                           (unless (gethash (cons seat-proxy key) *grabbed-keys*)
                             (grab-key seat-proxy key)
                             (incf created)))
                         *top-map*))
    (when (plusp created)
      (format t "~&; Rhizome: installed ~D key grabs on ~D seats~%"
              created (hash-table-count *seats*)))))

(defun release-key-grabs ()
  "Destroy every river key grab. The next manage sequence re-creates
grabs for the keys bound then."
  (dolist (binding *active-bindings*)
    (ignore-errors
      (rwm:river-xkb-binding-v1.destroy binding)))
  (setf *active-bindings* nil)
  (clrhash *grabbed-keys*))

;;; --- Default keybindings ---

;;; --- Prefix key (define once, change at runtime) ---

(defvar *prefix-key-binding* (cons +xkb-key-T+ *mod-ctrl*)
  "The keysym and modifiers of the current prefix binding (set-prefix-key).")

(defun parse-key-spec (spec)
  "Parse a key spec like \"C-t\", \"C-M-s\", \"s-Space\" or \"M-Return\"
into (MODIFIERS KEYSYM). C = ctrl, M = alt, s = super, S = shift."
  (let ((mods '()) (rest (string spec)))
    (loop while (and (> (length rest) 1)
                     (member (char rest 0) '(#\C #\M #\s #\S) :test #'char=)
                     (char= (char rest 1) #\-))
          do (case (char rest 0)
               (#\C (push :ctrl mods))
               (#\M (push :mod1 mods))
               (#\s (push :mod4 mods))
               (#\S (push :shift mods)))
             (setf rest (subseq rest 2)))
    (let ((down (string-downcase rest)))
      (values (reverse mods)
              (cond ((string= down "space") 32)
                    ((string= down "return") 65293)
                    ((string= down "enter") 65293)
                    ((string= down "tab") 65289)
                    ((string= down "escape") 65307)
                    ((string= down "backspace") 65288)
                    ((string= down "left") 65361)
                    ((string= down "right") 65363)
                    ((string= down "up") 65362)
                    ((string= down "down") 65364)
                    (t (when (= 1 (length rest))
                         (char-code (char-downcase (char rest 0))))))))))

(defun set-prefix-key (spec)
  "Change the prefix key at runtime, e.g. (rhizome:set-prefix-key \"C-z\").
Rebuilds *default-top-map* with the prefix on the new key; user
bindings are untouched. Current grabs are destroyed and re-created
during the next manage sequence, like set-user-mod."
  (multiple-value-bind (mods keysym) (parse-key-spec spec)
    (unless keysym
      (message "bad key spec: ~A (e.g. \"C-z\", \"s-t\")" spec)
      (return-from set-prefix-key))
    (setf *prefix-key-binding* (cons keysym mods))
    (build-default-top-map)
    (release-key-grabs)
    (request-manage-dirty)
    (message "prefix: ~A" spec)))

(defun bind-default-direct-keys ()
  "Bind rhizome's direct keys on *mod*, plus the prefix key. Called by
build-default-top-map, which directs the writes to *default-top-map*."

  ;; MOD+Return - launch terminal
  (define-key +xkb-key-Return+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching terminal~%")
      (uiop:launch-program '("foot"))))

  ;; MOD+, / MOD+. - scroll the ribbon viewport (niri-style)
  (define-key +xkb-key-Comma+ *mod*
    (lambda () (scroll-viewport-left)))
  (define-key +xkb-key-Period+ *mod*
    (lambda () (scroll-viewport-right)))

  ;; MOD+Q - close focused window
  (define-key +xkb-key-Q+ *mod*
    (lambda ()
      (let* ((seats (all-seats))
             (focused (and seats (seat-focused-window (first seats)))))
        (when focused
          (format t "~&; Rhizome: closing window ~A~%" (window-title focused))
          (close-window focused)
          (request-manage-dirty)))))

  ;; MOD+J - focus next
  (define-key +xkb-key-J+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-next (first outputs))))))

  ;; MOD+K - focus previous
  (define-key +xkb-key-K+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-previous (first outputs))))))

  ;; MOD+Space - cycle layout
  (define-key +xkb-key-Space+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (let* ((out (first outputs))
                 (current (output-current-layout out))
                 (next (case current
                         (:master-stack :even)
                         (:even :monocle)
                         (:monocle :master-stack)
                         (otherwise :master-stack))))
            (switch-layout out next))))))

  ;; MOD+1..9 - switch to workspace N
  ;; MOD+Shift+1..9 - move focused window to workspace N
  ;; (MOD+Space cycles layouts; the old Alt+1/2/3 layout bindings
  ;; were replaced by workspaces)
  (loop for i from 1 to 9
        for keysym = (+ 48 i)
        do (let ((ws i))
             (define-key keysym *mod*
               (lambda ()
                 (switch-workspace ws)))
             (define-key keysym (append *mod* *mod-shift*)
               (lambda ()
                 (move-focused-window-to-workspace ws)))))

  ;; MOD+E - launch emacs
  (define-key +xkb-key-E+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching emacs~%")
      (uiop:launch-program '("emacs"))))

  ;; MOD+D - application launcher (native layer-shell popup)
  (define-key +xkb-key-D+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching application launcher~%")
      (handler-case
          (show-launcher)
        (error (e)
          (format t "~&; Rhizome: launcher error: ~A~%" e)))))

  ;; MOD+B - toggle the status bar
  (define-key +xkb-key-B+ *mod*
    (lambda ()
      (format t "~&; Rhizome: toggling bar~%")
      (handler-case
          (toggle-bar)
        (error (e)
          (format t "~&; Rhizome: bar error: ~A~%" e)))))

  ;; Ctrl+T - prefix key (StumpWM-style keychains, see prefix.lisp):
  ;;   C-t w windowlist | C-t g workspace picker | C-t x command palette
  ;;   C-t : eval       | C-t ! shell command    | C-t b toggle bar
  ;;   C-t n/p next/prev workspace | C-t q quit rhizome
  ;; The prefix key (changeable at runtime: set-prefix-key)
  (define-key (car *prefix-key-binding*) (cdr *prefix-key-binding*)
    (lambda ()
      (enter-prefix-mode)))

  ;; MOD+W - window list (pick a window, jump to it)
  ;; MOD+G - workspace picker
  ;; MOD+X - command palette
  (define-key +xkb-key-W+ *mod*
    (lambda () (windowlist)))
  (define-key +xkb-key-G+ *mod*
    (lambda () (workspace-list)))
  (define-key +xkb-key-X+ *mod*
    (lambda () (command-palette)))

  ;; MOD+Shift+Q - power off the machine
  ;; MOD+Shift+R - reboot the machine
  (define-key +xkb-key-Q+ (append *mod* *mod-shift*)
    (lambda ()
      (shutdown-machine)))
  (define-key +xkb-key-R+ (append *mod* *mod-shift*)
    (lambda ()
      (reboot-machine)))
  ;; MOD+Shift+L - lock the session (see the (:lock ...) config)
  (define-key 76 (append *mod* *mod-shift*)    ; uppercase L
    (lambda ()
      (lock-screen)))
  ;; MOD+Shift+E - log out to the display manager (press twice)
  (define-key 101 (append *mod* *mod-shift*)  ; base keysym e — river matches unshifted
    (lambda ()
      (logout-confirm)))

  ;; MOD+; - live eval prompt (type a Lisp expression, Enter evaluates
  ;; it in the running WM image; Escape cancels)
  (define-key +xkb-key-Semicolon+ *mod*
    (lambda ()
      (format t "~&; Rhizome: opening eval prompt~%")
      (handler-case
          (show-eval-prompt)
        (error (e)
          (format t "~&; Rhizome: eval prompt error: ~A~%" e)))))

  ;; Print - screenshot of the full screen
  ;; Shift+Print - screenshot of a selected region (slurp)
  (define-key +xkb-key-Print+ *mod-none*
    (lambda ()
      (take-screenshot)))
  (define-key +xkb-key-Print+ *mod-shift*
    (lambda ()
      (take-screenshot :region t))))

(defun build-default-top-map ()
  "Rebuild *default-top-map* from *mod* and *prefix-key-binding*. The
user layer is untouched."
  (clear-keymap *default-top-map*)
  (let ((*defining-default-bindings* t))
    (bind-default-direct-keys))
  (setf *default-top-map-mod* *mod*)
  *default-top-map*)

(defun setup-default-keybindings ()
  "Build the default keymaps, *default-top-map* and the default prefix
map. Idempotent; user bindings are untouched."
  (build-default-top-map)
  (setup-default-prefix-bindings)
  (setf *default-keymaps-built* t))

(defun ensure-default-keybindings ()
  "Build the default keymaps when they are unbuilt, and rebuild
*default-top-map* when *mod* differs from the value it was built with.
Returns true when anything was rebuilt."
  (cond ((not *default-keymaps-built*)
         (setup-default-keybindings)
         t)
        ((not (equal *mod* *default-top-map-mod*))
         (build-default-top-map)
         t)))

;;; --- Application launcher ---

(defparameter *app-launcher-entries*
  '(("Foot (terminal)" . "foot")
    ("Emacs" . "emacs")
    ("Vim (in terminal)" . "foot -e vim")
    ("Htop (in terminal)" . "foot -e htop")
    ("Neovim (in terminal)" . "foot -e nvim")
    ("Bash (in terminal)" . "foot -e bash")
    ("MPV (media player)" . "mpv")
    ("Xterm" . "xterm")
    ("Zutty" . "zutty"))
  "Alist of (display-name . command) entries for the application launcher.")

(defun launch-application ()
  "Show an application launcher using fzf in a terminal.
Shows a curated list of Wayland-relevant applications."
  (setf (uiop:getenv "WAYLAND_DISPLAY") "wayland-1")
  (let ((names-file "/tmp/rhizome-launcher-names.txt")
        (cmds-file "/tmp/rhizome-launcher-cmds.txt")
        (result-file "/tmp/rhizome-launcher-result.txt"))
    ;; Write names and commands to temp files
    (with-open-file (f names-file :direction :output :if-exists :supersede)
      (dolist (entry *app-launcher-entries*)
        (format f "~A~%" (car entry))))
    (with-open-file (f cmds-file :direction :output :if-exists :supersede)
      (dolist (entry *app-launcher-entries*)
        (format f "~A|~A~%" (car entry) (cdr entry))))
    ;; Clear result file
    (with-open-file (f result-file :direction :output :if-exists :supersede)
      (format f ""))
    ;; Launch foot with fzf, writing the selection to result-file
    (uiop:launch-program
      (list "foot"
            "-e"
            "bash" "-c"
            (concatenate 'string
              "cat " names-file " | fzf --prompt='Run: ' > " result-file)))
    ;; Poll for the result file to be populated (terminal closed)
    (bordeaux-threads:make-thread
      (lambda ()
        (let ((cmd nil))
          ;; Wait up to 30 seconds for user to select
          (loop for i below 300
                until cmd
                do (sleep 0.1)
                   (setf cmd (handler-case
                                 (with-open-file (f result-file
                                                    :direction :input
                                                    :if-does-not-exist nil)
                                   (when (and f (> (file-length f) 0))
                                     (file-position f 0)
                                     (read-line f nil)))
                               (error () nil))))
          (when cmd
            (let ((entry (find cmd *app-launcher-entries*
                               :key #'car :test #'string=)))
              (when entry
                (format t "~&; Rhizome: launching ~A~%" (cdr entry))
                (uiop:launch-program
                  (list "bash" "-c" (cdr entry)))))))))))

;;; --- Integration with manage sequence ---

(defun install-keybindings-if-needed ()
  "Sync key grabs during a manage sequence: build the default keymaps
if needed (dropping stale grabs when they change), then grab every
bound key not yet grabbed. Never writes the user layer."
  (when (ensure-default-keybindings)
    (release-key-grabs))
  (when *xkb-bindings-proxy*
    (install-keybindings)))

(defun set-user-mod (mod)
  "Change the user modifier — the MOD in MOD+Return, MOD+1..9,
MOD+drag, ... — live. MOD is a friendly name: :alt, :super or :ctrl.
The default bindings move to the new modifier; user bindings keep
the modifiers they were made with. Current grabs are destroyed and
re-created during the next manage sequence (enable requests are
manage-sequence-only in river's protocol).

  (rhizome:set-user-mod :alt)     ; classic StumpWM-on-X11 feel
  (rhizome:set-user-mod :super)   ; the default"
  (let ((resolved (resolve-mod mod)))
    (unless resolved
      (message "unknown modifier: ~A (try :alt, :super, :ctrl)" mod)
      (return-from set-user-mod))
    (setf *mod* resolved)
    ;; Rebuild the default layer on the new modifier; the user layer
    ;; keeps its bindings, literal modifiers included
    (build-default-top-map)
    ;; Destroy the current grabs; install-keybindings-if-needed
    ;; re-creates them during the next manage sequence
    (release-key-grabs)
    ;; MOD+drag pointer grabs likewise
    (dolist (binding *pointer-bindings*)
      (ignore-errors
        (rwm:river-pointer-binding-v1.destroy binding)))
    (setf *pointer-bindings* nil
          *pointer-bindings-enabled* nil)
    (clrhash *seats-with-pointer-bindings*)
    (request-manage-dirty)
    (message "mod key: ~(~A~)" mod)))
