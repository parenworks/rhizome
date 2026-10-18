;;; Window Selection Expressions (WSE) for Rhizome
;;;
;;; A tiny Lisp DSL for selecting sets of windows, ported from
;;; StumpWM's wse.lisp. Expressions are evaluated against windows:
;;;
;;;   (:app-id "firefox")            regex against the app-id
;;;   (:title "docs")                regex against the title
;;;   (:number 3)                    window with number 3
;;;   (:workspace 2)                 windows on workspace 2
;;;   (:float t) / (:float nil)      floating / tiled windows
;;;   (:marked t)                     windows carrying a mark
;;;   (:focused t)                    the focused window
;;;   (:and e1 e2 ...) / (:or ...) / (:not e) / (:any)
;;;
;;; Regexes are raw cl-ppcre patterns (case sensitivity is yours to
;;; ask for, e.g. "(?i)firefox"). Use from the REPL, hooks, or window
;;; rules:
;;;
;;;   (rhizome:wse-windows '(:and (:app-id "emacs") (:title "main")))
;;;   (rhizome:wse-map '(:workspace 2 :float nil) #'window-number)
;;;   (rhizome:configure
;;;     '(:window-rules ((:wse (:and (:app-id "gimp") (:float nil))
;;;                            :float t))))

(in-package #:rhizome)

(defun wse-matches-p (win expr)
  "Whether WIN matches the window selection expression EXPR."
  (let ((op (first expr)))
    (ecase op
      (:and (every (lambda (e) (wse-matches-p win e)) (rest expr)))
      (:or (some (lambda (e) (wse-matches-p win e)) (rest expr)))
      (:not (not (wse-matches-p win (second expr))))
      (:any t)
      (:app-id (and (window-app-id win)
                    (cl-ppcre:scan (second expr) (window-app-id win))))
      (:title (and (window-title win)
                   (cl-ppcre:scan (second expr) (window-title win))))
      (:number (eql (window-number win) (second expr)))
      (:workspace (eql (window-workspace win) (second expr)))
      (:float (eq (not (window-floating-p win)) (not (second expr))))
      (:marked (eq (not (window-marked-p win)) (not (second expr))))
      (:focused (eq (window-focused-p win) (second expr))))))

(defun wse-windows (expr)
  "All windows matching the selection expression EXPR."
  (remove-if-not (lambda (win) (wse-matches-p win expr)) (all-windows)))

(defun wse-map (expr fn)
  "Call FN on every window matching EXPR."
  (dolist (win (wse-windows expr))
    (funcall fn win)))

(defun wse-count (expr)
  "How many windows match EXPR."
  (length (wse-windows expr)))
