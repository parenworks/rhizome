;;; Tests for per-workspace types and layouts (the StumpWM parity
;;; milestone): :tile / :float workspace modes, per-workspace
;;; layout names surviving switches, and the float-mode sweep.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

;; request-manage-dirty and messages are no-ops without a WM proxy /
;; with the stub below; float/unfloat are the real ones so the sweep
;; logic is exercised for real.

(let ((pass 0) (fail 0) (dirty 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (dirty-calls () dirty))
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args) (declare (ignore args)) (incf dirty)))

    (let ((out (make-instance 'rhizome-output :proxy :modes-out
                              :width 1920 :height 1080))
          (*workspace-layouts* (make-hash-table :test 'equal))
          (*workspace-modes* (make-hash-table :test 'equal)))
      (setf (gethash out *output-workspace*) 1
            (output-current-layout out) :master-stack)

      ;; --- mode accessor: defaults ---

      (ok "workspace mode defaults to :tile"
          (workspace-mode out 3) :tile)
      (ok "workspace layout falls back to the configured default"
          (workspace-layout out 3) :master-stack)

      ;; --- config-declared float modes ---

      (let ((*config* '(:workspaces (:modes (2 :float 5 :float)))))
        (ok "config declares workspace 2 as :float"
            (workspace-mode out 2) :float)
        (ok "config-declared mode for workspace 5"
            (workspace-mode out 5) :float)
        (ok "undeclared workspaces stay :tile"
            (workspace-mode out 3) :tile))
      (ok "hash modes win over config"
          (let ((*config* '(:workspaces (:modes (2 :float)))))
            (setf (workspace-mode out 2) :tile)
            (prog1 (workspace-mode out 2)
              (setf (workspace-mode out 2) :float)))
          :tile)

      ;; --- set-workspace-mode sweeps windows ---

      (let ((win1 (make-instance 'rhizome-window :proxy :mode-w1))
            (win2 (make-instance 'rhizome-window :proxy :mode-w2)))
        (setf (gethash :mode-w1 *windows*) win1
              (gethash :mode-w2 *windows*) win2
              (window-output win1) out
              (window-output win2) out
              (window-x win1) 10 (window-y win1) 10
              (window-width win1) 100 (window-height win1) 100
              (window-x win2) 10 (window-y win2) 10
              (window-width win2) 100 (window-height win2) 100)
        (assign-window-workspace win1 4)
        (assign-window-workspace win2 4)
        (setf dirty 0)
        (set-workspace-mode :float 4 out)
        (ok "float mode floats every window on the workspace"
            (list (window-floating-p win1) (window-floating-p win2))
            '(t t))
        (ok "tile mode returns them to the grid"
            (progn (set-workspace-mode :tile 4 out)
                   (list (window-floating-p win1)
                         (window-floating-p win2)))
            '(nil nil))
        (ok "mode changes request relayout"
            (> (dirty-calls) 0) t)
        (set-workspace-mode :tile 4 out))

      ;; --- per-workspace layouts ---

      ;; switch-layout stores under the CURRENT workspace; a fake
      ;; output with zero dims skips the frame-tree rebuild
      (let ((bogus (make-instance 'rhizome-output :proxy :bogus-out
                                  :width 0 :height 0)))
        (setf (gethash bogus *output-workspace*) 1
              (output-current-layout bogus) :master-stack)
        (switch-layout bogus :even)
        (ok "switch-layout stores per-workspace"
            (workspace-layout bogus 1) :even)
        (ok "switch-layout mirrors to the output reader"
            (output-current-layout bogus) :even)
        ;; Switching to another workspace: its own layout (or the
        ;; fallback) shows; the first workspace keeps :even
        (switch-workspace 2 bogus)
        (ok "new workspace falls back to the output layout"
            (output-current-layout bogus) :master-stack)
        (switch-layout bogus :monocle)
        (switch-workspace 1 bogus)
        (ok "workspace 1 kept its layout across switches"
            (output-current-layout bogus) :even)
        (switch-workspace 2 bogus)
        (ok "workspace 2 kept its layout across switches"
            (output-current-layout bogus) :monocle))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))))

(uiop:quit 0)
