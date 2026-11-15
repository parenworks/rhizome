;;; ipc-command-args.lisp: how IPC command lines reach their handlers.
;;; Integer arguments arrive once parsed, wse and putsel take the rest
;;; of the line as one argument, and scratchpad without an argument
;;; keeps toggle-scratchpad's default. Downstream functions are
;;; replaced by recorders, so nothing here needs a compositor.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

;; The scratchpad command is a module: load it from the repo's
;; modules/ directory, the way a user loads it.
(let ((*module-directory*
        (make-pathname
          :directory (append (pathname-directory
                               (asdf:system-source-directory :rhizome))
                              '("modules")))))
  (load-module "rhizome-scratchpads"))

(defvar *recorded-calls* nil
  "Argument lists passed to the recorders, most recent first.")

(defmacro with-recorders ((&rest names) &body body)
  "Run BODY with each function in NAMES replaced by a recorder that
pushes (NAME . ARGS) onto *recorded-calls* and returns NIL."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (mapcar #'fdefinition ',names))
           (*recorded-calls* nil))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (name)
                          `(setf (fdefinition ',name)
                                 (lambda (&rest args)
                                   (push (cons ',name args) *recorded-calls*)
                                   nil)))
                        names)
              ,@body)
         (loop for name in ',names
               for fn in ,saved
               do (setf (fdefinition name) fn))))))

(defun recorded-call (line name)
  "Dispatch LINE and return the arguments NAME was called with, or
:NOT-CALLED."
  (ipc-dispatch-line line)
  (let ((call (assoc name *recorded-calls*)))
    (if call (rest call) :not-called)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- integer arguments over IPC ---
    (with-recorders (select-window-by-number)
      (ok "select-window 3 selects window 3"
          (recorded-call "select-window 3" 'select-window-by-number)
          '(3)))
    (with-recorders (pull-window-by-number)
      (ok "pull 2 pulls window 2"
          (recorded-call "pull 2" 'pull-window-by-number)
          '(2 nil 1)))
    (with-recorders (move-marked-to-workspace)
      (ok "move-marked 5 moves to workspace 5"
          (recorded-call "move-marked 5" 'move-marked-to-workspace)
          '(5)))
    (with-recorders (resize-current-split)
      (ok "resize-split -40 moves the split by -40"
          (recorded-call "resize-split -40" 'resize-current-split)
          '(nil 1 -40))
      (setf *recorded-calls* nil)
      (ok "resize-split with no argument moves the split by 30"
          (recorded-call "resize-split" 'resize-current-split)
          '(nil 1 30)))

    ;; --- string arguments from Lisp still parse ---
    (with-recorders (select-window-by-number move-marked-to-workspace)
      (execute-command nil :select-window '("7"))
      (execute-command nil :move-marked '("4"))
      (ok "execute-command with string arguments"
          (reverse *recorded-calls*)
          '((select-window-by-number 7) (move-marked-to-workspace 4))))
    (with-recorders (select-window-by-number)
      (ok "select-window with a non-number selects nothing"
          (recorded-call "select-window x" 'select-window-by-number)
          '(-1)))

    ;; --- wse and putsel take the rest of the line ---
    (with-recorders (wse-windows)
      (ok "wse reads an expression containing a space"
          (recorded-call "wse :app-id \"foot\"" 'wse-windows)
          '((:app-id "foot")))
      (setf *recorded-calls* nil)
      (ok "wse reads a parenthesized expression"
          (recorded-call "wse (:and (:app-id \"foot\") (:float t))"
                         'wse-windows)
          '((:and (:app-id "foot") (:float t))))
      (setf *recorded-calls* nil)
      (ok "wse reads a one-word expression"
          (recorded-call "wse :any" 'wse-windows)
          '((:any))))
    (with-recorders (putsel)
      (ok "putsel copies the whole text"
          (recorded-call "putsel hello   there world" 'putsel)
          '("hello   there world"))
      (setf *recorded-calls* nil)
      (ok "putsel keeps a number as text"
          (recorded-call "putsel 0042" 'putsel)
          '("0042")))

    ;; --- scratchpad keeps its default name ---
    (with-recorders (toggle-scratchpad)
      (ok "scratchpad with no argument uses the default"
          (recorded-call "scratchpad" 'toggle-scratchpad)
          '())
      (setf *recorded-calls* nil)
      (ok "scratchpad NAME toggles NAME"
          (recorded-call "scratchpad notes" 'toggle-scratchpad)
          '("notes"))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
