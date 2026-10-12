;;; Keyboard bindings for Stumpway
;;;
;;; Uses the river-xkb-bindings-v1 protocol to register key
;;; bindings with the compositor. Bindings are defined with
;;; keysyms and modifiers, and trigger callbacks when pressed.
;;;
;;; Default bindings (StumpWM-style, with Alt as prefix):
;;;   Alt+Return   - launch terminal (foot)
;;;   Alt+Q        - close focused window
;;;   Alt+J        - focus next window
;;;   Alt+K        - focus previous window
;;;   Alt+Space    - cycle layout
;;;   Alt+1..9     - switch to workspace 1..9
;;;   Alt+Shift+1..9 - move focused window to workspace 1..9
;;;   Alt+E        - launch emacs
;;;   Alt+D        - application launcher (native layer-shell popup)
;;;   Alt+B        - toggle the status bar
;;;   Print        - screenshot of full screen (grim)
;;;   Shift+Print  - screenshot of selected region (grim + slurp)
;;;   Alt+;        - live eval prompt (evaluate Lisp in the running WM)

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
  (defconstant +xkb-key-Escape+  65307))   ; #xff1b

;;; --- Modifier bitfield constants ---
;;; Wayflan expects bitfield enums as lists of keywords.

(defparameter *mod-none*  '(:none))
(defparameter *mod-shift* '(:shift))
(defparameter *mod-ctrl*  '(:ctrl))
(defparameter *mod-alt*   '(:mod1))
(defparameter *mod-super* '(:mod4))

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

(defun setup-default-keybindings ()
  "Set up the default StumpWM-style keybindings."

  ;; Alt+Return - launch terminal
  (define-key +xkb-key-Return+ *mod-alt*
    (lambda ()
      (format t "~&; Stumpway: launching terminal~%")
      (uiop:launch-program '("foot"))))

  ;; Alt+Q - close focused window
  (define-key +xkb-key-Q+ *mod-alt*
    (lambda ()
      (let* ((seats (all-seats))
             (focused (and seats (seat-focused-window (first seats)))))
        (when focused
          (format t "~&; Stumpway: closing window ~A~%" (window-title focused))
          (close-window focused)
          (request-manage-dirty)))))

  ;; Alt+J - focus next
  (define-key +xkb-key-J+ *mod-alt*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-next (first outputs))))))

  ;; Alt+K - focus previous
  (define-key +xkb-key-K+ *mod-alt*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-previous (first outputs))))))

  ;; Alt+Space - cycle layout
  (define-key +xkb-key-Space+ *mod-alt*
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

  ;; Alt+1..9 - switch to workspace N
  ;; Alt+Shift+1..9 - move focused window to workspace N
  ;; (Alt+Space cycles layouts; the old Alt+1/2/3 layout bindings
  ;; were replaced by workspaces)
  (loop for i from 1 to 9
        for keysym = (+ 48 i)
        do (let ((ws i))
             (define-key keysym *mod-alt*
               (lambda ()
                 (switch-workspace ws)))
             (define-key keysym (append *mod-alt* *mod-shift*)
               (lambda ()
                 (move-focused-window-to-workspace ws)))))

  ;; Alt+E - launch emacs
  (define-key +xkb-key-E+ *mod-alt*
    (lambda ()
      (format t "~&; Stumpway: launching emacs~%")
      (uiop:launch-program '("emacs"))))

  ;; Alt+D - application launcher (native layer-shell popup)
  (define-key +xkb-key-D+ *mod-alt*
    (lambda ()
      (format t "~&; Stumpway: launching application launcher~%")
      (handler-case
          (show-launcher)
        (error (e)
          (format t "~&; Stumpway: launcher error: ~A~%" e)))))

  ;; Alt+B - toggle the status bar
  (define-key +xkb-key-B+ *mod-alt*
    (lambda ()
      (format t "~&; Stumpway: toggling bar~%")
      (handler-case
          (toggle-bar)
        (error (e)
          (format t "~&; Stumpway: bar error: ~A~%" e)))))

  ;; Alt+; - live eval prompt (type a Lisp expression, Enter evaluates
  ;; it in the running WM image; Escape cancels)
  (define-key +xkb-key-Semicolon+ *mod-alt*
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
