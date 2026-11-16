;;; sources.lisp - named sources own what they register
;;;
;;; THE MODEL (one model for rules AND keybindings):
;;;   - (:config ...) in the config file is the :config source
;;;   - while load-module runs a module, *current-source* is the
;;;     module's name as a keyword - its top-level define-key /
;;;     define-prefix-key / configure calls file under it
;;;   - the REPL is :repl
;;;   - reload-config replaces the :config source ONLY: a binding or
;;;     rule the file no longer makes is gone, everything another
;;;     source registered survives untouched (#25)
;;;   - set-window-rules / set-key-bindings replace a source's whole
;;;     set: no duplicates on reload, omitted entries dropped
;;;   - precedence for rules: :config first, then sources in
;;;     first-registration order

(in-package #:rhizome)

(defparameter *current-source* nil
  "The source that owns what the running code registers. NIL (the
REPL outside a load) means :repl.")

(defparameter *window-rule-sources* (make-hash-table)
  "Source key to its complete list of window rules.")

(defparameter *window-rule-source-order* nil
  "Sources in first-registration order, :config excluded (it is
always first).")

(defun current-source ()
  "The source that owns registrations being made right now."
  (or *current-source* :repl))

;;; --- window rules ---

(defun effective-window-rules ()
  "All sources' rules, :config first, then registration order."
  (apply #'append
         (cons (gethash :config *window-rule-sources*)
               (loop for s in *window-rule-source-order*
                     collect (gethash s *window-rule-sources*)))))

(defun set-window-rules (rules &key (source (current-source)))
  "Replace SOURCE's complete set of window rules; NIL empties the
set. The effective rules (:config first, then registration order)
take effect at the next rule match - rules are not re-run over
windows that are already open."
  (unless (or (eq source :config)
              (gethash source *window-rule-sources*))
    (push source *window-rule-source-order*))
  (setf (gethash source *window-rule-sources*) rules)
  (setf (getf *config* :window-rules) (effective-window-rules))
  rules)

;;; --- keybindings ---

(defparameter *source-key-bindings* (make-hash-table)
  "Source key to the (MAP . SPEC) pairs it bound.")

(defun note-source-binding (map spec)
  "Record that the current source bound SPEC in MAP."
  (let ((source (current-source))
        (entry (cons map spec)))
    (setf (gethash source *source-key-bindings*)
          (cons entry
                (remove entry (gethash source *source-key-bindings*)
                        :test #'equal)))))

(defun remove-source-key-bindings (&optional (source (current-source)))
  "Undo every binding SOURCE made (a NIL binding removes the key
from the keymap, per define-key), releasing its grabs."
  (dolist (entry (gethash source *source-key-bindings*))
    (ignore-errors
      (let ((spec (cdr entry)))
        (if (and (consp spec) (integerp (first spec)))
            ;; the direct form: (keysym modifiers) - define-key's
            ;; integer arm, a NIL binding removes it
            (funcall (symbol-function 'define-key)
                     (first spec) (second spec) nil)
            (funcall (symbol-function 'bind-key-sequence)
                     (car entry)
                     (funcall (symbol-function 'ensure-key-sequence)
                              spec)
                     nil)))))
  (setf (gethash source *source-key-bindings*) nil)
  (when (fboundp 'mark-key-grabs-dirty)
    (funcall (symbol-function 'mark-key-grabs-dirty))))

(defun set-key-bindings (bindings &key (source (current-source)))
  "Replace SOURCE's complete set of keybindings. BINDINGS is a list
of (MAP SPEC ACTION) triples, applied in order."
  (remove-source-key-bindings source)
  (dolist (b bindings)
    (destructuring-bind (map spec action) b
      (let ((*current-source* source))
        (note-source-binding map spec)
        (funcall (symbol-function 'bind-key-sequence)
                 map
                 (funcall (symbol-function 'ensure-key-sequence)
                          spec)
                 action))))
  (when (fboundp 'mark-key-grabs-dirty)
    (funcall (symbol-function 'mark-key-grabs-dirty))))

;;; --- reload semantics ---

(defun reset-config-source ()
  "Drop the :config source's registrations (its rules and its
keybindings) so a re-read of the file replaces them wholesale.
Every other source keeps its sets."
  (remove-source-key-bindings :config)
  (remhash :config *window-rule-sources*)
  (setf (getf *config* :window-rules) (effective-window-rules)))
