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

(defmacro define-command (name (output args) &body body)
  "Define a user command handler."
  `(setf (gethash ,name *command-table*)
         (lambda (,output ,args)
           ,@body)))

(defun execute-command (output command args)
  "Execute a user command on OUTPUT."
  (let ((handler (gethash command *command-table*)))
    (if handler
        (funcall handler output args)
        (format t "~&; Stumpway: unknown command ~A~%" command))))

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
