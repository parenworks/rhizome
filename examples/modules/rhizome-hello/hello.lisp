;;; rhizome-hello — the one-file example module.
;;; Copy this directory into ~/.config/rhizome/modules/ and add
;;; (rhizome:load-module "rhizome-hello") to your config.lisp.

(in-package #:rhizome)

(define-command :hello (output args)
  "Say hello from the example module."
  (declare (ignore output args))
  (message "hello from rhizome-hello, loaded as a module"))
