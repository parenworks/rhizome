;;; output-rules-by-name.lisp - a window rule's :output takes the
;;; output's wl_output NAME (e.g. "eDP-2") alongside the 1-based
;;; index. Names survive output reordering; indices do not.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *orn-pass* 0)
(defparameter *orn-fail* 0)

(defun orn-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *orn-pass*)
             (format t "~&PASS: ~A~%" name))
      (progn (incf *orn-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                     name actual expected))))

(let ((a (make-instance 'rhizome-output :proxy :orn-a))
      (b (make-instance 'rhizome-output :proxy :orn-b)))
  (setf (output-wl-output-name a) "eDP-2"
        (output-wl-output-name b) "DP-1")
  (let ((real-sorted (symbol-function 'all-outputs-sorted))
        (real-rules (gethash :config *window-rule-sources*)))
    (flet ((stub-sorted () (list a b)))
      (setf (symbol-function 'all-outputs-sorted) #'stub-sorted)
      (unwind-protect
           (progn
             ;; by name
             (set-window-rules '((:match "app1" :output "DP-1"))
                                :source :config)
             (let ((win (make-instance 'rhizome-window)))
               (setf (window-app-id win) "app1")
               (apply-window-rules win)
               (orn-ok "rule by output NAME places on the named output"
                       (eq (window-output win) b) t))
             ;; by index (unchanged)
             (set-window-rules '((:match "app2" :output 2))
                                :source :config)
             (let ((win (make-instance 'rhizome-window)))
               (setf (window-app-id win) "app2")
               (apply-window-rules win)
               (orn-ok "rule by output INDEX still works (1-based)"
                       (eq (window-output win) b) t))
             ;; unknown name: rule does not fire (no crash)
             (set-window-rules '((:match "app3" :output "HDMI-9"))
                                :source :config)
             (let ((win (make-instance 'rhizome-window)))
               (setf (window-app-id win) "app3")
               (apply-window-rules win)
               (orn-ok "unknown name places on nothing"
                       (window-output win) nil)))
        (setf (symbol-function 'all-outputs-sorted) real-sorted)
        (set-window-rules real-rules :source :config)))))

(format t "~&=== ~A passed, ~A failed ===~%" *orn-pass* *orn-fail*)
(when (plusp *orn-fail*)
  (uiop:quit 1))
