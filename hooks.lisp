;;; Hooks for Stumpway
;;;
;;; StumpWM-style hooks: user-defined functions triggered on WM
;;; events. Handlers are added with ADD-HOOK and run (in unspecified
;;; order) with the event's arguments; an error in one handler never
;;; affects others or the WM itself.
;;;
;;;   (stumpway:add-hook :window-created
;;;     (lambda (win) (format t "new window: ~A~%" (window-title win))))
;;;
;;; Available hooks:
;;;   :window-created      (win)
;;;   :window-closed       (win)
;;;   :focus-changed       (win)
;;;   :layout-changed      (output layout-name)
;;;   :workspace-changed  (workspace output)
;;;   :output-created      (output)

(in-package #:stumpway)

(defvar *hooks* (make-hash-table)
  "Maps hook names (keywords) to lists of handler functions.")

(defun add-hook (name function)
  "Add FUNCTION as a handler for the hook NAME (a keyword)."
  (pushnew function (gethash name *hooks*) :test #'eq)
  function)

(defun remove-hook (name function)
  "Remove FUNCTION from the handlers for the hook NAME."
  (setf (gethash name *hooks*)
        (remove function (gethash name *hooks*) :test #'eq))
  function)

(defun clear-hook (name)
  "Remove all handlers for the hook NAME."
  (remhash name *hooks*))

(defun list-hooks ()
  "Return an alist of (name . handlers) for all hooks with handlers."
  (loop for name being the hash-keys of *hooks*
        collect (cons name (gethash name *hooks*))))

(defun run-hook (name &rest args)
  "Run all handlers for the hook NAME with ARGS. Handler errors are
logged and isolated: they never affect other handlers or the WM."
  (dolist (handler (gethash name *hooks*))
    (handler-case
        (apply handler args)
      (error (e)
        (format t "~&; Stumpway: error in ~A hook handler: ~A~%" name e)))))
