;;; River window management protocol bindings
;;;
;;; This file uses wayflan's scanner to generate Lisp bindings
;;; from the river-window-management-v1.xml protocol definition.

(in-package #:cl-user)

(defpackage #:rhizome.river-wm
  (:use #:cl #:xyz.shunter.wayflan.client))

(in-package #:rhizome.river-wm)

;;; --- Wayflan compat: tolerate zero-length wire strings ---
;;;
;;; wayflan's read-wl-string computes :max-chars (1- nul-length),
;;; which is 0 for a zero-length wayland string — and CFFI asserts
;;; on that. The demarshal error aborts BEFORE the buffer advances,
;;; so the WM re-reads the same poison event forever and everything
;;; queued behind it (keyboard input, layer-surface configures —
;;; the entire event stream) never processes: a wedged launcher
;;; that shows one stale entry and ignores typing. Patched here at
;;; load time rather than in the quicklisp dist: on the failing
;;; path the uint is already consumed and a 0-length string needs
;;; no advance, so returning "" is wire-correct. Upstream bug
;;; (worth reporting to wayflan: empty strings are legal on the
;;; wire and river sends them, e.g. empty app-ids).

(let ((real (symbol-function 'xyz.shunter.wayflan.wire:read-wl-string)))
  (setf (symbol-function 'xyz.shunter.wayflan.wire:read-wl-string)
        (lambda (buffer)
          (handler-case (funcall real buffer)
            (error () "")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/river-window-management-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t)

  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/river-xkb-bindings-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t)

  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/wlr-layer-shell-unstable-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t)

  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/river-layer-shell-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t)

  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/river-input-management-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t)

  (xyz.shunter.wayflan.client.scanner:wl-include
    (merge-pathnames
      #P"protocol/river-libinput-config-v1.xml"
      (asdf:system-source-directory :rhizome))
    :export t))
