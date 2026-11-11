;;; Keymaps for Rhizome
;;;
;;; A keymap is a named, documented table of key -> binding with an
;;; ordered list of parent keymaps. Lookup consults the keymap's own
;;; table first, then each parent depth-first, so a child binding
;;; shadows a parent's. Keys are compared with EQUAL: direct bindings
;;; use (keysym . modifiers), prefix chains use the bare keysym. A
;;; binding is a function or a nested keymap.
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

(defun keymap-lookup (keymap key)
  "Return the binding of KEY in KEYMAP or, failing that, in its parents
depth-first in order. The second value is the keymap whose own table
holds the binding, or NIL when KEY is unbound."
  (multiple-value-bind (binding present)
      (gethash key (keymap-bindings keymap))
    (if present
        (values binding keymap)
        (dolist (parent (keymap-parents keymap) (values nil nil))
          (multiple-value-bind (found where) (keymap-lookup parent key)
            (when where
              (return (values found where))))))))

(defun keymap-bind (keymap key binding &optional doc)
  "Bind KEY to BINDING in KEYMAP's own table, documented by DOC.
A NIL BINDING removes KEY from the own table, so a parent's binding
shows through again. Returns BINDING."
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
    (and where (values (gethash key (keymap-docs where))))))

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
