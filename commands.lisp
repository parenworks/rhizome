;;; Command dispatch for Stumpway
;;;
;;; Commands can be triggered by:
;;;   - Keyboard bindings (via river-xkb-bindings-v1, future)
;;;   - Pointer bindings (via river_seat_v1.get_pointer_binding)
;;;   - The REPL (via SLIME/SLY - the StumpWM way)
;;;
;;; From the REPL:
;;;   (stumpway:execute-command output :focus-next nil)
;;;   (stumpway:execute-command output :switch-layout '("even"))

(in-package #:stumpway)

(defvar *command-table* (make-hash-table)
  "Maps command keywords to handler functions.")

(defvar *command-docs* (make-hash-table)
  "Maps command keywords to their docstrings (describe-command).")

(defmacro define-command (name (output args) &body body)
  "Define a user command handler. A leading docstring is captured for
describe-command."
  `(progn
     (setf (gethash ,name *command-table*)
           (lambda (,output ,args)
             ,@body))
     (when (and (first ',body) (stringp (first ',body)))
       (setf (gethash ,name *command-docs*) (first ',body)))))

(defun execute-command (output command args)
  "Execute a user command on OUTPUT."
  (let ((handler (gethash command *command-table*)))
    (if handler
        (funcall handler output args)
        (format t "~&; Stumpway: unknown command ~A~%" command))))

(defun describe-command-prompt ()
  "Prompt for a command name and show its documentation."
  (prompt-open "Describe command"
              :candidates
              (sort (loop for k being the hash-keys of *command-table*
                          collect (string-downcase (string k)))
                    #'string<)
              :on-complete
              (lambda (name)
                (let* ((k (find-symbol (string-upcase name) :keyword))
                       (doc (and k (gethash k *command-docs*))))
                  (if doc
                      (message "~A: ~A" name doc)
                      (message "~A: no documentation" name))))))

;;; --- Built-in commands --

(define-command :switch-layout (output args)
  "Switch to a named layout. Usage: switch-layout <name>"
  (when args
    (let ((layout-name (intern (string-upcase (first args)) :keyword)))
      (switch-layout output layout-name))))

(define-command :split-horizontal (output args)
  "Split the current frame horizontally."
  (declare (ignore args))
  (split-horizontal output))

(define-command :split-vertical (output args)
  "Split the current frame vertically."
  (declare (ignore args))
  (split-vertical output))

(define-command :remove-split (output args)
  "Remove the current frame split."
  (declare (ignore args))
  (remove-split output))

(define-command :resize-split (output args)
  "Move the current split's dividing line by ARG pixels (default +30)."
  (resize-current-split output (current-workspace output)
                        (or (and args
                                 (parse-integer (first args) :junk-allowed t))
                            30)))

(define-command :focus-left (output args)
  "Move frame focus left."
  (declare (ignore args))
  (focus-frame-direction output (current-workspace output) :left))

(define-command :focus-right (output args)
  "Move frame focus right."
  (declare (ignore args))
  (focus-frame-direction output (current-workspace output) :right))

(define-command :focus-up (output args)
  "Move frame focus up."
  (declare (ignore args))
  (focus-frame-direction output (current-workspace output) :up))

(define-command :focus-down (output args)
  "Move frame focus down."
  (declare (ignore args))
  (focus-frame-direction output (current-workspace output) :down))

(define-command :toggle-float (output args)
  "Float/tile the focused window (GIMP-style floats)."
  (declare (ignore output args))
  (toggle-float-focused))

(define-command :focus-next (output args)
  "Focus the next frame."
  (declare (ignore args))
  (focus-next output))

(define-command :focus-previous (output args)
  "Focus the previous frame."
  (declare (ignore args))
  (focus-previous output))

(define-command :list-layouts (output args)
  "List available layouts."
  (declare (ignore output args))
  (format t "~&; Stumpway: available layouts:~%")
  (maphash (lambda (name fn)
             (declare (ignore fn))
             (format t "~&;   ~A~%" name))
           *layouts*))

(define-command :help (output args)
  "Show available commands."
  (declare (ignore output args))
  (format t "~&; Stumpway commands:~%")
  (maphash (lambda (name fn)
             (declare (ignore fn))
             (format t "~&;   ~A~%" name))
           *command-table*))

;;; --- StumpWM quick-win commands ---

(define-command :lastmsg (output args)
  "Redisplay the most recent message (StumpWM: lastmsg)."
  (declare (ignore output args))
  (lastmsg))

(define-command :time (output args)
  "Show the current date and time (StumpWM: time)."
  (declare (ignore output args))
  (time-message))

(define-command :select-window (output args)
  "Focus the window with number N. Usage: select-window <n>"
  (when args
    (select-window-by-number (or (parse-integer (first args) :junk-allowed t)
                                 -1))))

(define-command :pull (output args)
  "Pull the window with number N into the current frame.
Usage: pull <n>"
  (when args
    (pull-window-by-number (or (parse-integer (first args) :junk-allowed t)
                               -1)
                           output
                           (current-workspace output))))

(define-command :pull-next (output args)
  "Pull the next hidden window into the current frame
(StumpWM: pull-hidden-next)."
  (declare (ignore args))
  (pull-hidden-next output (current-workspace output) 1))

(define-command :pull-prev (output args)
  "Pull the previous hidden window into the current frame
(StumpWM: pull-hidden-previous)."
  (declare (ignore args))
  (pull-hidden-next output (current-workspace output) -1))

(define-command :renumber (output args)
  "Renumber the workspace's windows from 1 (StumpWM: renumber)."
  (declare (ignore args))
  (renumber-windows output (current-workspace output))
  "renumbered")

(define-command :repack-window-numbers (output args)
  "Fill gaps in the workspace's window numbers (StumpWM: repack)."
  (declare (ignore args))
  (repack-window-numbers output (current-workspace output))
  "repacked")

(define-command :toggle-fullscreen (output args)
  "Toggle the focused window's fullscreen (StumpWM: fullscreen)."
  (declare (ignore args))
  (let ((win (find-if #'window-focused-p (all-windows))))
    (if win
        (toggle-fullscreen win)
        (message "no focused window"))))

(define-command :command-mode (output args)
  "Keep the prefix armed for successive commands until Escape
(StumpWM: command-mode)."
  (declare (ignore output args))
  (enter-command-mode))

(define-command :iresize (output args)
  "Interactive resize: arrows move the split until Enter/Esc
(StumpWM: iresize)."
  (declare (ignore output args))
  (enter-iresize-mode))

(define-command :mark (output args)
  "Mark/unmark the focused window for mark commands (StumpWM: mark)."
  (declare (ignore output args))
  (toggle-mark-focused))

(define-command :clear-window-marks (output args)
  "Remove every window mark (StumpWM: clear-window-marks)."
  (declare (ignore output args))
  (clear-window-marks))

(define-command :move-marked (output args)
  "Move every marked window to WORKSPACE and clear the marks
(StumpWM: gmove-marked). Usage: move-marked <workspace>"
  (when args
    (let ((ws (or (parse-integer (first args) :junk-allowed t)
                  -1)))
      (move-marked-to-workspace ws))))

(define-command :dump-frame (output args)
  "Save the current workspace's frame layout to a file.
Usage: dump-frame <name> (files live in ~/.local/share/stumpway/frame-dumps/)"
  (when args
    (dump-frame output (current-workspace output)
                (frame-dump-pathname (first args)))))

(define-command :restore-frame (output args)
  "Restore a saved frame layout into the current workspace.
Usage: restore-frame <name>"
  (when args
    (let ((file (frame-dump-pathname (first args))))
      (if (probe-file file)
          (restore-frame output (current-workspace output) file)
          (message "no such dump: ~A" (first args))))))

(define-command :set-prefix-key (output args)
  "Change the prefix key at runtime (StumpWM: set-prefix-key).
Usage: set-prefix-key <spec>  e.g. set-prefix-key C-z"
  (when args
    (set-prefix-key (first args))))

(define-command :enable-minor-mode (output args)
  "Enable a minor mode: its prefix bindings take precedence.
Usage: enable-minor-mode <name>"
  (declare (ignore output))
  (when args
    (enable-minor-mode (intern (string-upcase (first args)) :keyword))))

(define-command :disable-minor-mode (output args)
  "Disable a minor mode. Usage: disable-minor-mode <name>"
  (declare (ignore output))
  (when args
    (disable-minor-mode (intern (string-upcase (first args)) :keyword))))

(define-command :list-minor-modes (output args)
  "List the enabled minor modes."
  (declare (ignore output args))
  (format nil "enabled: ~{~A~^, ~}" (enabled-minor-modes)))

(define-command :wse (output args)
  "List windows matching a selection expression.
Usage: wse <expr>  e.g. wse (:and (:app-id \"firefox\") (:float t))"
  (declare (ignore output))
  (when args
    (let ((wins (wse-windows (read-from-string
                               (format nil "(~A)" (first args))))))
      (format nil "~D: ~{~A~^, ~}"
              (length wins)
              (mapcar (lambda (w) (or (window-title w)
                                      (window-app-id w) "?"))
                      wins)))))

(define-command :only (output args)
  "Collapse the workspace to a single frame (StumpWM: only)."
  (declare (ignore args))
  (frame-only output (current-workspace output)))

(define-command :balance (output args)
  "Equalize every split in the workspace (StumpWM: balance-frames)."
  (declare (ignore args))
  (frame-balance output (current-workspace output)))

(define-command :fclear (output args)
  "Clear the current frame; its windows move to the other frames
(StumpWM: fclear)."
  (declare (ignore args))
  (frame-clear output (current-workspace output)))

(define-command :exchange-left (output args)
  "Swap the current frame's window with the frame to the left
(StumpWM: exchange-direction)."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :left))

(define-command :exchange-right (output args)
  "Swap the current frame's window with the frame to the right."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :right))

(define-command :exchange-up (output args)
  "Swap the current frame's window with the frame above."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :up))

(define-command :exchange-down (output args)
  "Swap the current frame's window with the frame below."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :down))

;;; --- Clipboard (via wl-clipboard) ---

(defun getsel ()
  "The Wayland clipboard/primary selection contents, as a string."
  (string-right-trim '(#\newline #\return #\space)
                     (uiop:run-program '("wl-paste" "--no-newline")
                                       :output :string
                                       :ignore-error-status t)))

(defun putsel (text)
  "Put TEXT on the Wayland clipboard."
  (uiop:run-program '("wl-copy")
                    :input (format nil "~A" text)
                    :ignore-error-status t))

(defun copy-last-message ()
  "Copy the most recent on-screen message to the clipboard
(StumpWM: copy-last-message)."
  (when *last-message*
    (putsel *last-message*))
  "copied")

(define-command :getsel (output args)
  "Show the clipboard contents (StumpWM: getsel)."
  (declare (ignore output args))
  (message "~A" (getsel)))

(define-command :putsel (output args)
  "Put ARGS on the clipboard. Usage: putsel <text>"
  (declare (ignore output))
  (when args
    (putsel (first args))
    "copied"))

(define-command :copy-last-message (output args)
  "Copy the most recent message to the clipboard."
  (declare (ignore output args))
  (copy-last-message))

;;; --- Power management ---

(define-command :suspend (output args)
  "Suspend the machine (systemctl suspend)."
  (declare (ignore output args))
  (message "suspending")
  (uiop:launch-program '("systemctl" "suspend"))
  "suspending")

(define-command :hibernate (output args)
  "Hibernate the machine (systemctl hibernate)."
  (declare (ignore output args))
  (message "hibernating")
  (uiop:launch-program '("systemctl" "hibernate"))
  "hibernating")

(define-command :lock (output args)
  "Lock the session with the configured locker
((:lock :command \"swaylock\" :args ...) — sudo pacman -S swaylock)."
  (declare (ignore output args))
  (lock-screen)
  "locking")

(define-command :logout (output args)
  "End the river session; the display manager's greeter returns."
  (declare (ignore output args))
  (logout-confirm))

;;; --- IPC-facing commands (see ipc.lisp) ---

(define-command :switch-workspace (output args)
  "Switch to workspace N. Usage: switch-workspace <n>"
  (when args
    (switch-workspace (first args) output)
    (format nil "workspace ~A" (first args))))

(define-command :current-workspace (output args)
  "Report the current workspace number."
  (declare (ignore args))
  (format nil "~A" (current-workspace (or output (first (all-outputs))))))

(define-command :toggle-bar (output args)
  "Toggle the status bar."
  (declare (ignore output args))
  (toggle-bar)
  "ok")

(define-command :screenshot (output args)
  "Take a screenshot; use arg REGION for a region capture."
  (declare (ignore output))
  (take-screenshot :region (and args (string-equal (first args) "region")))
  "ok")

(define-command :list-windows (output args)
  "List window titles."
  (declare (ignore output args))
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (win) (or (window-title win) (window-app-id win) "?"))
                  (all-windows))))

(define-command :quit (output args)
  "Stop the window manager (the river session keeps running)."
  (declare (ignore output args))
  (stop)
  "stopping")

;;; --- Power management ---

(defun shutdown-machine ()
  "Power off the machine. Works from an active session (polkit
grants login1 power-off to active local sessions)."
  (format t "~&; Stumpway: powering off~%")
  (uiop:launch-program '("systemctl" "poweroff")))

(defun reboot-machine ()
  "Reboot the machine."
  (format t "~&; Stumpway: rebooting~%")
  (uiop:launch-program '("systemctl" "reboot")))

;;; --- Screenshot ---

(defun take-screenshot (&key (region nil))
  "Capture a screenshot with grim, saving it to
~/Pictures/Screenshots/ and copying it to the Wayland clipboard.
With :region t, select the area with slurp first."
  (let* ((dir (merge-pathnames #P"Pictures/Screenshots/"
                                (user-homedir-pathname)))
         (path (merge-pathnames
                 (multiple-value-bind (sec min hour day month year)
                     (get-decoded-time)
                   (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D.png"
                           year month day hour min sec))
                 dir))
         (path-str (namestring path))
         (cmd (if region
                  (format nil "grim -g \"$(slurp)\" ~S && wl-copy < ~S"
                          path-str path-str)
                  (format nil "grim ~S && wl-copy < ~S" path-str path-str))))
    (ensure-directories-exist dir)
    (uiop:launch-program (list "bash" "-c" cmd))
    (format t "~&; Stumpway: screenshot ~A-> ~A~%"
            (if region "region " "") path-str)))
