;;; River window management protocol bindings
;;;
;;; This file uses wayflan's scanner to generate Lisp bindings
;;; from the river-window-management-v1.xml protocol definition.

(in-package #:cl-user)

(defpackage #:rhizome.river-wm
  (:use #:cl #:xyz.shunter.wayflan.client))

(in-package #:rhizome.river-wm)

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
