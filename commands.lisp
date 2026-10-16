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
