;;; Tests for the niri-style scroll ribbon: enter/exit transitions,
;;; column geometry, viewport clamping, focus-follows-visibility,
;;; close-pruning, arrivals, floats, and the toggle.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; River-touching stubs
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'propose-window-dimensions)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'make-window-node)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'set-window-position)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'set-window-borders)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'show-window)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'focus-window)
          (lambda (win &rest seats)
            (declare (ignore seats))
            (dolist (w (all-windows))
              (unless (eq w win)
                (setf (window-focused-p w) nil)))
            (setf (window-focused-p win) t)))

    ;; Fresh state (and one seat, so focus takes effect)
    (setf *seats* (make-hash-table)
          (gethash :scroll-seat *seats*)
          (make-instance 'rhizome-seat :proxy :scroll-seat))
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *scroll-states* (make-hash-table :test 'equal))

    (let* ((out (make-instance 'rhizome-output :proxy :sc-out
                              :width 1920 :height 1080))
           (w0 (make-instance 'rhizome-window :proxy :sc0))
           (w1 (make-instance 'rhizome-window :proxy :sc1))
           (w2 (make-instance 'rhizome-window :proxy :sc2))
           (w3 (make-instance 'rhizome-window :proxy :sc3)))
      (setf (gethash :sc0 *windows*) w0
            (gethash :sc1 *windows*) w1
            (gethash :sc2 *windows*) w2
            (gethash :sc3 *windows*) w3
            (gethash out *output-workspace*) 5)
      (dolist (win (list w0 w1 w2 w3))
        (setf (window-output win) out
              (window-width win) 800
              (window-height win) 600))
      (dolist (win (list w0 w1 w2))
        (assign-window-workspace win 5))
      ;; w3 starts on another workspace; it arrives later
      (assign-window-workspace w3 6)
      ;; Three columns of tiling: w0 | w1 | w2, focused on w0
      (set-workspace-tree out 5
                          '((0 0 640 1080)
                            (640 0 640 1080)
                            (1280 0 640 1080))
                          (list w0 w1 w2))
      (setf (window-focused-p w0) t)

      ;; --- Enter: frame order becomes column order ---

      (scroll-enter out 5)
      (ok "workspace is scrolled"
          (scroll-workspace-p out 5) t)
      (ok "columns keep frame order"
          (mapcar #'window-proxy (getf (scroll-state out 5) :columns))
          '(:SC0 :SC1 :SC2))
      (ok "windows leave the frame tree"
          (remove-if-not #'window-frame (list w0 w1 w2))
          nil)
      (ok "layout indicator switches to scroll"
          (output-current-layout out) :scroll)
      (ok "column width defaults to half the screen"
          (getf (scroll-state out 5) :column-width) 960)

      ;; --- Column geometry ---

      (let ((rects (scroll-layout-columns out 5)))
        (ok "columns are full height at the viewport origin"
            (mapcar (lambda (r) (list (second r) (third r)
                                      (fourth r) (fifth r)))
                    rects)
            '((0 0 960 1080) (960 0 960 1080) (1920 0 960 1080))))
        (ok "focused column starts visible"
            (getf (scroll-state out 5) :scroll-x) 0)

      ;; --- Focus moves columns; the viewport follows ---

      (scroll-focus out 5 1)
      (ok "focus moves one column right"
          (and (window-focused-p w1) t) t)
      (ok "second column still fits: no scroll yet"
          (getf (scroll-state out 5) :scroll-x) 0)
      (scroll-focus out 5 1)
      (ok "focus reaches the third column"
          (and (window-focused-p w2) t) t)
      (ok "viewport scrolled to keep focus visible"
          (getf (scroll-state out 5) :scroll-x) 960)
      (ok "focused column now on screen"
          (second (third (scroll-layout-columns out 5))) 960)
      (scroll-focus out 5 -1)
      (ok "focus returns left"
          (and (window-focused-p w1) t) t)

      ;; --- Viewport scroll: half-screen steps, clamped ---

      (scroll-viewport out 5 1)
      (ok "viewport scrolls half a screen"
          (getf (scroll-state out 5) :scroll-x) 960)
      (scroll-viewport out 5 1)
      (ok "viewport clamps at the ribbon end"
          (getf (scroll-state out 5) :scroll-x) 960)
      (scroll-viewport out 5 -1)
      (scroll-viewport out 5 -1)
      (ok "viewport clamps at the ribbon start"
          (getf (scroll-state out 5) :scroll-x) 0)

      ;; --- Closes prune columns and refocus ---

      (remhash :sc1 *windows*)
      (scroll-manage-decisions out 5 ())
      (ok "closed window's column is pruned"
          (mapcar #'window-proxy (getf (scroll-state out 5) :columns))
          '(:SC0 :SC2))
      (ok "surviving focus stays"
          (and (window-focused-p w2) t) t)

      ;; --- Arrivals enter next to the focused column ---

      (assign-window-workspace w3 5)
      (scroll-manage-decisions out 5 (list w3))
      (ok "new window becomes a column after the focused one"
          (mapcar #'window-proxy (getf (scroll-state out 5) :columns))
          '(:SC0 :SC2 :SC3))
      (remhash :sc3 *windows*)

      ;; --- Floats leave the ribbon ---

      (float-window w2 :x 10 :y 10 :width 100 :height 100)
      (scroll-manage-decisions out 5 ())
      (ok "floated window leaves the ribbon"
          (mapcar #'window-proxy (getf (scroll-state out 5) :columns))
          '(:SC0))

      ;; --- Exit: back to the dynamic tiler ---

      (scroll-exit out 5)
      (ok "state is gone"
          (scroll-workspace-p out 5) nil)
      (ok "layout indicator restored"
          (output-current-layout out) :master-stack)
      (ok "windows are frameless for re-adoption"
          (list (window-frame w0) (window-frame w2))
          '(nil nil))
      (dolist (win (list w0 w2))
        (setf (window-floating-p win) nil))
      (let ((*config* '(:tiling (:auto-split t))))
        (frames-manage-decisions out 5 (list w0))
        (frames-manage-decisions out 5 (list w2)))
      (ok "dynamic tiler re-adopts with splits"
          (list (length (frame-leaves (frame-tree-for out 5)))
                (and (window-frame w0) t)
                (and (window-frame w2) t))
          '(2 t t))

      ;; --- Toggle both ways ---

      (scroll-toggle out)
      (ok "toggle enters the ribbon"
          (scroll-workspace-p out (current-workspace out)) t)
      (scroll-toggle out)
      (ok "toggle exits the ribbon"
          (scroll-workspace-p out (current-workspace out)) nil)

      ;; --- Dispatch: the pipeline routes scrolled workspaces ---

      (scroll-enter out 5)
      (remhash :sc2 *windows*)
      (apply-layout-for-output out)
      (ok "manage pass routes to the ribbon (pruning ran)"
          (mapcar #'window-proxy (getf (scroll-state out 5) :columns))
          '(:SC0))
      (scroll-exit out 5)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
