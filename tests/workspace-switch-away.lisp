;;; Inverse regression test: current workspace is 2.
;;; The visible workspace-1 IDE must be hidden, the workspace-2
;;; browser must be shown (no-op if already visible).
(ql:quickload :rhizome)

(in-package #:rhizome)

(let ((calls '()))
  (dolist (spec '((show-window      :show)
                  (hide-window      :hide)
                  (make-window-node :node)
                  (set-window-position :position)
                  (set-window-borders  :borders)))
    (let* ((fn (first spec))
           (tag (second spec)))
      (setf (symbol-function fn)
            (lambda (win &rest args)
              (declare (ignore args))
              (push (list tag win) calls)
              (when (eq fn 'show-window)
                (setf (window-hidden-p win) nil))
              (when (eq fn 'hide-window)
                (setf (window-hidden-p win) t))))))

  (let ((ide    (make-instance 'rhizome-window :proxy :ide-proxy))
        (browser (make-instance 'rhizome-window :proxy :browser-proxy))
        (out     (make-instance 'rhizome-output :proxy :out-proxy
                                :width 1920 :height 1080)))
    (setf (gethash :ide-proxy *windows*) ide
          (window-hidden-p ide) nil
          (window-output ide) out)
    (assign-window-workspace ide 1)
    (setf (gethash :browser-proxy *windows*) browser
          (window-hidden-p browser) t
          (window-output browser) out)
    (assign-window-workspace browser 2)
    ;; Current workspace: 2 (we just switched here)
    (setf (gethash out *output-workspace*) 2)

    (render-layout-for-output out)

    (let* ((ide-hidden (find (list :hide ide) calls :test #'equal))
           (browser-shown (find (list :show browser) calls :test #'equal))
           (ide-shown-again (find (list :show ide) calls :test #'equal)))
      (format t "~&=== RESULT (switch TO ws 2) ===~%")
      (format t "ide hidden:         ~A~%" (and ide-hidden t))
      (format t "browser shown:      ~A~%" (and browser-shown t))
      (format t "ide wrongly re-shown: ~A~%" (and ide-shown-again t))
      (if (and ide-hidden browser-shown (not ide-shown-again))
          (format t "~&PASS-AWAY: switching to ws 2 hides ws 1, shows ws 2~%")
          (format t "~&FAIL: switch-away behavior wrong~%")))))

(uiop:quit 0)
