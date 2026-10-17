;;; Keyboard bindings for Stumpway
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

(in-package #:stumpway)

(defvar *xkb-bindings-proxy* nil
  "The river_xkb_bindings_v1 proxy.")

(defvar *keybindings* (make-hash-table :test 'equal)
  "Maps (keysym . modifiers) to callback functions.")

(defvar *active-bindings* nil
  "List of active river_xkb_binding_v1 proxy objects.")

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
        (format t "~&; Stumpway: xkb-bindings interface not found!~%")
        (return-from setup-xkb-bindings))
      (setf *xkb-bindings-proxy*
            (wl:wl-registry.bind registry name iface-class version))
      (format t "~&; Stumpway: bound river_xkb_bindings_v1 (version ~D)~%" version))))

(defun define-key (keysym modifiers callback)
  "Register a keybinding. KEYSYM is an xkb keysym, MODIFIERS is a bitmask,
CALLBACK is a function called when the key is pressed."
  (setf (gethash (cons keysym modifiers) *keybindings*) callback))

(defun install-keybindings ()
  "Create and enable all registered keybindings. Must be called during
a manage sequence."
  (when (null *xkb-bindings-proxy*)
    (format t "~&; Stumpway: no xkb-bindings proxy, skipping keybindings~%")
    (return-from install-keybindings))

  (when (and *active-bindings* (plusp (hash-table-count *keybindings*)))
    ;; Already installed and we have bindings
    (return-from install-keybindings))

  ;; Destroy any previously active bindings
  (dolist (binding *active-bindings*)
    (rwm:river-xkb-binding-v1.destroy binding))
  (setf *active-bindings* nil)

  (format t "~&; Stumpway: installing ~D keybindings on ~D seats~%"
          (hash-table-count *keybindings*) (hash-table-count *seats*))

  ;; Create and enable new bindings for each seat
  (loop for seat-proxy being the hash-keys of *seats*
        do (loop for keyspec being the hash-keys of *keybindings*
                 using (hash-value cb)
                 do (let ((callback cb)
                          (keysym (car keyspec))
                          (modifiers (cdr keyspec)))
                      (let ((binding (rwm:river-xkb-bindings-v1.get-xkb-binding
                                       *xkb-bindings-proxy*
                                       seat-proxy
                                       keysym
                                       modifiers)))
                        ;; Set up event handler
                        (push (lambda (event-name &rest args)
                                (declare (ignore args))
                                (case event-name
                                  (:pressed
                                   (format t "~&; Stumpway: keybinding pressed (keysym=~A mod=~A)~%"
                                           keysym modifiers)
                                   (funcall callback))
                                  (:released
                                   nil)
                                  (:stop-repeat
                                   nil)))
                              (wl:wl-proxy-hooks binding))
                        ;; Enable the binding
                        (rwm:river-xkb-binding-v1.enable binding)
                        (push binding *active-bindings*))))))

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
  "Change the prefix key at runtime, e.g. (stumpway:set-prefix-key \"C-z\").
Current grabs are destroyed and the new binding is installed during
the next manage sequence, like set-user-mod."
  (multiple-value-bind (mods keysym) (parse-key-spec spec)
    (unless keysym
      (message "bad key spec: ~A (e.g. \"C-z\", \"s-t\")" spec)
      (return-from set-prefix-key))
    ;; Swap the binding in the table
    (remhash *prefix-key-binding* *keybindings*)
    (setf *prefix-key-binding* (cons keysym mods))
    (define-key keysym mods (lambda () (enter-prefix-mode)))
    ;; Reinstall: destroy grabs, re-create during the next manage pass
    (dolist (binding *active-bindings*)
      (ignore-errors
        (rwm:river-xkb-binding-v1.destroy binding)))
    (setf *active-bindings* nil)
    (request-manage-dirty)
    (message "prefix: ~A" spec)))

(defun setup-default-keybindings ()
  "Set up the default StumpWM-style keybindings."

  ;; MOD+Return - launch terminal
  (define-key +xkb-key-Return+ *mod*
    (lambda ()
      (format t "~&; Stumpway: launching terminal~%")
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
          (format t "~&; Stumpway: closing window ~A~%" (window-title focused))
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
      (format t "~&; Stumpway: launching emacs~%")
      (uiop:launch-program '("emacs"))))

  ;; MOD+D - application launcher (native layer-shell popup)
  (define-key +xkb-key-D+ *mod*
    (lambda ()
      (format t "~&; Stumpway: launching application launcher~%")
      (handler-case
          (show-launcher)
        (error (e)
          (format t "~&; Stumpway: launcher error: ~A~%" e)))))

  ;; MOD+B - toggle the status bar
  (define-key +xkb-key-B+ *mod*
    (lambda ()
      (format t "~&; Stumpway: toggling bar~%")
      (handler-case
          (toggle-bar)
        (error (e)
          (format t "~&; Stumpway: bar error: ~A~%" e)))))

  ;; Ctrl+T - prefix key (StumpWM-style keychains, see prefix.lisp):
  ;;   C-t w windowlist | C-t g workspace picker | C-t x command palette
  ;;   C-t : eval       | C-t ! shell command    | C-t b toggle bar
  ;;   C-t n/p next/prev workspace | C-t q quit stumpway
  ;; The prefix key (changeable at runtime: set-prefix-key)
  (define-key (car *prefix-key-binding*) (cdr *prefix-key-binding*)
    (lambda ()
      (enter-prefix-mode)))
  (setup-default-prefix-bindings)

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
  (define-key 69 (append *mod* *mod-shift*)    ; uppercase E
    (lambda ()
      (logout-confirm)))

  ;; MOD+; - live eval prompt (type a Lisp expression, Enter evaluates
  ;; it in the running WM image; Escape cancels)
  (define-key +xkb-key-Semicolon+ *mod*
    (lambda ()
      (format t "~&; Stumpway: opening eval prompt~%")
      (handler-case
          (show-eval-prompt)
        (error (e)
          (format t "~&; Stumpway: eval prompt error: ~A~%" e)))))

  ;; Print - screenshot of the full screen
  ;; Shift+Print - screenshot of a selected region (slurp)
  (define-key +xkb-key-Print+ *mod-none*
    (lambda ()
      (take-screenshot)))
  (define-key +xkb-key-Print+ *mod-shift*
    (lambda ()
      (take-screenshot :region t))))

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
  (let ((names-file "/tmp/stumpway-launcher-names.txt")
        (cmds-file "/tmp/stumpway-launcher-cmds.txt")
        (result-file "/tmp/stumpway-launcher-result.txt"))
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
                (format t "~&; Stumpway: launching ~A~%" (cdr entry))
                (uiop:launch-program
                  (list "bash" "-c" (cdr entry)))))))))))

;;; --- Integration with manage sequence ---

(defun install-keybindings-if-needed ()
  "Install keybindings during a manage sequence if not yet installed."
  (when (and *xkb-bindings-proxy* (null *active-bindings*))
    (install-keybindings)))

(defun set-user-mod (mod)
  "Change the user modifier — the MOD in MOD+Return, MOD+1..9,
MOD+drag, ... — live. MOD is a friendly name: :alt, :super or :ctrl.
Current grabs are destroyed and everything re-binds with the new
modifier during the next manage sequence (enable requests are
manage-sequence-only in river's protocol).

  (stumpway:set-user-mod :alt)     ; classic StumpWM-on-X11 feel
  (stumpway:set-user-mod :super)   ; the default"
  (let ((resolved (resolve-mod mod)))
    (unless resolved
      (message "unknown modifier: ~A (try :alt, :super, :ctrl)" mod)
      (return-from set-user-mod))
    (setf *mod* resolved)
    ;; Re-register the user bindings with the new modifier
    (clrhash *keybindings*)
    (setup-default-keybindings)
    ;; Destroy the current grabs; install-keybindings-if-needed
    ;; re-creates them during the next manage sequence
    (dolist (binding *active-bindings*)
      (ignore-errors
        (rwm:river-xkb-binding-v1.destroy binding)))
    (setf *active-bindings* nil)
    ;; MOD+drag pointer grabs likewise
    (dolist (binding *pointer-bindings*)
      (ignore-errors
        (rwm:river-pointer-binding-v1.destroy binding)))
    (setf *pointer-bindings* nil
          *pointer-bindings-enabled* nil)
    (clrhash *seats-with-pointer-bindings*)
    (request-manage-dirty)
    (message "mod key: ~(~A~)" mod)))
