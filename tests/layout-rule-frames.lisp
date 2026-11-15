;;; layout-rule-frames.lisp: a window rule's :layout gives the rule's
;;; workspace that layout. The workspace stores it, its frame tree is
;;; rebuilt from the layout's template with the matching window in it
;;; (a window the rule meets before it has an output included), and
;;; the output's layout mirror follows only when that workspace is
;;; showing. A rule that fires again (every title change) leaves an
;;; already applied layout's frames alone.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0)
      (out (make-instance 'rhizome-output :proxy :lay-out
                                          :width 1920 :height 1080)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (window (proxy app-id workspace)
           "A window on OUT and WORKSPACE; with WORKSPACE NIL, a new
window the manage pass has not placed yet (no output, no workspace)."
           (let ((win (make-instance 'rhizome-window :proxy proxy)))
             (setf (gethash proxy *windows*) win
                   (window-app-id win) app-id)
             (when workspace
               (setf (window-output win) out)
               (assign-window-workspace win workspace))
             win))
         (leaf-count (workspace)
           (length (frame-leaves (frame-tree-for out workspace)))))
    (setf (gethash :lay-out *outputs*) out
          (gethash out *output-workspace*) 1)

    ;; --- a rule naming another workspace, for a new window ---
    (configure '(:window-rules ((:match "^lay-even$" :workspace 2
                                 :layout :even))))
    (window :la1 "plain" 2)
    (window :la2 "plain" 2)
    (let ((win (window :la3 "lay-even" nil)))
      (apply-window-rules win)
      (ok "the rule moves the window to workspace 2"
          (window-workspace win) 2)
      (ok "the window gets the rule's output"
          (eq (window-output win) out) t))
    (ok "workspace 2 stores the rule's layout"
        (workspace-layout out 2) :even)
    (ok "workspace 2's frames follow the layout: a 2x2 grid for three windows"
        (leaf-count 2) 4)
    (ok "every window on workspace 2, the new one included, sits in a frame"
        (loop for p in '(:la1 :la2 :la3)
              always (window-frame (gethash p *windows*)))
        t)
    (ok "workspace 1's layout is untouched"
        (workspace-layout out 1) :master-stack)
    (ok "the output's mirror stays on the showing workspace's layout"
        (output-current-layout out) :master-stack)

    ;; --- the rule fires again: frames are kept ---
    (let ((tree (frame-tree-for out 2)))
      (apply-window-rules (gethash :la3 *windows*))
      (ok "a repeated match keeps the frame tree"
          (eq tree (frame-tree-for out 2)) t))

    ;; --- a rule for the showing workspace, layout named by a string ---
    (configure '(:window-rules ((:match "^lay-mono$" :layout "monocle"))))
    (window :lb1 "plain" 1)
    (let ((win (window :lb2 "lay-mono" nil)))
      (apply-window-rules win)
      (ok "a new window lands on the showing workspace"
          (window-workspace win) 1)
      (ok "the new window shares the single monocle frame"
          (eq (window-frame win)
              (window-frame (gethash :lb1 *windows*)))
          t))
    (ok "workspace 1 stores the string layout as a keyword"
        (workspace-layout out 1) :monocle)
    (ok "workspace 1's frames follow the layout: one frame"
        (leaf-count 1) 1)
    (ok "the output's mirror follows the showing workspace"
        (output-current-layout out) :monocle)

    ;; --- an unknown layout changes nothing ---
    (configure '(:window-rules ((:match "^lay-bogus$" :workspace 3
                                 :layout :no-such-layout))))
    (let ((win (window :lc1 "lay-bogus" 1)))
      (apply-window-rules win)
      (ok "an unknown layout still moves the window"
          (window-workspace win) 3))
    (ok "an unknown layout is not stored"
        (workspace-layout out 3) :master-stack)
    (ok "an unknown layout leaves the mirror alone"
        (output-current-layout out) :monocle))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
