;;; Keymaps for Rhizome
;;;
;;; A keymap is a named, documented table of key -> binding with an
;;; ordered list of parent keymaps. Lookup consults the keymap's own
;;; table first, then each parent depth-first, so a child binding
;;; shadows a parent's. Keys are compared with EQUAL; rhizome's own
;;; keymaps use keys as keys.lisp defines them. A binding is a
;;; function, a symbol naming one, an event-binding, or a keymap (or a
;;; symbol whose value is one), which a key press enters as a sequence.
;;;
;;; Rhizome's own bindings live in default keymaps that are parents of
;;; the user keymaps, so a user binding wins regardless of the order
;;; in which the two were made.

(in-package #:rhizome)

(defstruct (keymap (:constructor %make-keymap (name doc parents))
                   (:copier nil)
                   (:print-object
                    (lambda (keymap stream)
                      (print-unreadable-object (keymap stream :type t :identity t)
                        (format stream "~S ~D"
                                (keymap-name keymap)
                                (hash-table-count (keymap-bindings keymap)))))))
  (name nil)
  (doc nil)
  (parents nil :type list)
  (bindings (make-hash-table :test 'equal) :type hash-table :read-only t)
  (docs (make-hash-table :test 'equal) :type hash-table :read-only t))

(setf (documentation 'keymap 'type)
      "A table of key -> binding (a function or a keymap) with a name, a
doc string, and ordered parent keymaps consulted after its own table.")

(defvar *defining-default-bindings* nil
  "True while the default keymaps are built: define-key and
define-prefix-key then write the default layer, not the user layer.")

(defun make-keymap (&key name doc parents)
  "Return a new empty keymap named NAME with documentation DOC.
PARENTS is an ordered list of keymaps consulted after this one."
  (%make-keymap name doc parents))

(defun keymap-key (designator)
  "Return the table key for DESIGNATOR: a canonical key when it is a
bare keysym, a key, or a key spec string naming one key; DESIGNATOR
itself otherwise."
  (if (or (integerp designator) (stringp designator) (key-p designator))
      (ensure-key designator)
      designator))

(defun keymap-lookup (keymap key)
  "Return the binding of KEY (see keymap-key) in KEYMAP or, failing
that, in its parents depth-first in order. The second value is the
keymap whose own table holds the binding, or NIL when KEY is unbound."
  (setf key (keymap-key key))
  (multiple-value-bind (binding present)
      (gethash key (keymap-bindings keymap))
    (if present
        (values binding keymap)
        (dolist (parent (keymap-parents keymap) (values nil nil))
          (multiple-value-bind (found where) (keymap-lookup parent key)
            (when where
              (return (values found where))))))))

(defun keymap-bind (keymap key binding &optional doc)
  "Bind KEY (see keymap-key) to BINDING in KEYMAP's own table,
documented by DOC. A NIL BINDING removes KEY from the own table, so a
parent's binding shows through again. Returns BINDING."
  (setf key (keymap-key key))
  (let ((table (keymap-bindings keymap))
        (docs (keymap-docs keymap)))
    (cond ((null binding)
           (remhash key table)
           (remhash key docs))
          (t
           (setf (gethash key table) binding)
           (if doc
               (setf (gethash key docs) doc)
               (remhash key docs))))
    binding))

(defun keymap-binding-doc (keymap key)
  "Return the doc string of KEY's effective binding in KEYMAP, or NIL."
  (multiple-value-bind (binding where) (keymap-lookup keymap key)
    (declare (ignore binding))
    (and where (values (gethash (keymap-key key) (keymap-docs where))))))

(defun map-keymap (function keymap)
  "Call FUNCTION with KEY and BINDING for every effective binding of
KEYMAP, inherited ones included. Each key is visited once, with the
binding keymap-lookup returns for it. Returns NIL."
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((walk (map)
               (maphash (lambda (key binding)
                          (unless (gethash key seen)
                            (setf (gethash key seen) t)
                            (funcall function key binding)))
                        (keymap-bindings map))
               (mapc #'walk (keymap-parents map))))
      (walk keymap)))
  nil)

(defun keymap-keys (keymap)
  "Return the keys with an effective binding in KEYMAP, inherited ones
included."
  (let ((keys '()))
    (map-keymap (lambda (key binding)
                  (declare (ignore binding))
                  (push key keys))
                keymap)
    (nreverse keys)))

(defun clear-keymap (keymap)
  "Remove every binding from KEYMAP's own table. Parents are untouched.
Returns KEYMAP."
  (clrhash (keymap-bindings keymap))
  (clrhash (keymap-docs keymap))
  keymap)

(defun binding-keymap (binding)
  "Return the keymap BINDING denotes: BINDING itself when it is a
keymap, the value of a bound symbol holding a keymap, or NIL."
  (cond ((keymap-p binding) binding)
        ((and binding (symbolp binding) (boundp binding)
              (keymap-p (symbol-value binding)))
         (symbol-value binding))))

(defun bind-key-sequence (keymap keys binding &optional doc)
  "Bind the key list KEYS in KEYMAP (or the keymap a symbol names) to
BINDING, documented by DOC, and return BINDING. Every key but the last
descends a level: into the keymap the key is bound to when KEYMAP's own
table holds it or a symbol names it; into a new child keymap, bound in
KEYMAP, when the keymap is inherited from a parent, which is left
unmodified; into a new keymap when the key is unbound or bound to a
function."
  (let ((map (or (binding-keymap keymap)
                 (error "~S is not a keymap" keymap))))
    (loop for (key . more) on keys
          while more
          do (multiple-value-bind (found holder) (keymap-lookup map key)
               (let ((sub (binding-keymap found)))
                 (setf map
                       (cond ((and sub (or (symbolp found) (eq holder map)))
                              sub)
                             (sub
                              (keymap-bind map key
                                           (make-keymap :name (key-description key)
                                                        :parents (list sub))))
                             (t
                              (keymap-bind map key
                                           (make-keymap :name (key-description key)))))))))
    (keymap-bind map (car (last keys)) binding doc)))

(defstruct (event-binding (:constructor event-binding (function))
                          (:copier nil))
  (function nil :read-only t))

(setf (documentation 'event-binding 'type)
      "A binding whose FUNCTION is called with the key event as its one
argument. A plain function binding is called with no arguments.")
