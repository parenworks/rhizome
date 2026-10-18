;;; Regression test for workspace window restoration.
;;; Scenario: IDE hidden on workspace 1 (we switched away and back),
;;; browser visible on workspace 2, current workspace is 1.
;;; render-layout-for-output must SHOW the hidden workspace-1 window
;;; and HIDE the workspace-2 one.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((calls '()))
  ;; Stub the river-touching functions so the layout logic can be
  ;; exercised without a live compositor connection.
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
    ;; IDE on workspace 1, hidden (we switched away and are back)
    (setf (gethash :ide-proxy *windows*) ide
          (window-hidden-p ide) t
          (window-output ide) out)
    (assign-window-workspace ide 1)
    ;; Browser on workspace 2, currently visible
    (setf (gethash :browser-proxy *windows*) browser
          (window-hidden-p browser) nil
          (window-output browser) out)
    (assign-window-workspace browser 2)
    ;; Current workspace: 1
    (setf (gethash out *output-workspace*) 1)

    (render-layout-for-output out)

    (let* ((ide-shown (find (list :show ide) calls :test #'equal))
           (browser-hidden (find (list :hide browser) calls :test #'equal))
           (browser-shown (find (list :show browser) calls :test #'equal)))
      (format t "~&=== RESULT (switch BACK to ws 1) ===~%")
      (format t "ide shown:      ~A~%" (and ide-shown t))
      (format t "browser hidden: ~A~%" (and browser-hidden t))
      (format t "browser wrongly re-shown: ~A~%" (and browser-shown t))
      (if (and ide-shown browser-hidden (not browser-shown))
          (format t "~&PASS: switching back to ws 1 restores hidden windows~%")
          (format t "~&FAIL: windows not restored correctly~%")))))

(uiop:quit 0)
