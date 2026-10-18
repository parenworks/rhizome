;;; Tests for per-workspace group types: the registry, live type
;;; switches that take over every window (tile/scroll/floating),
;;; floating-workspace arrivals, and the C-t l cycle.
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
          (gethash :groups-seat *seats*)
          (make-instance 'rhizome-seat :proxy :groups-seat))
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *scroll-states* (make-hash-table :test 'equal)
          *workspace-types* (make-hash-table :test 'equal))

    (let* ((out (make-instance 'rhizome-output :proxy :gr-out
                              :width 1920 :height 1080))
           (a (make-instance 'rhizome-window :proxy :ga))
           (b (make-instance 'rhizome-window :proxy :gb))
           (c (make-instance 'rhizome-window :proxy :gc)))
      (setf (gethash :ga *windows*) a
            (gethash :gb *windows*) b
            (gethash :gc *windows*) c
            (gethash out *output-workspace*) 7)
      (dolist (win (list a b c))
        (setf (window-output win) out
              (window-width win) 800
              (window-height win) 600))
      (dolist (win (list a b c))
        (assign-window-workspace win 7))
      ;; two tiled windows, dynamic adoption
      (frames-manage-decisions out 7 (list a))
      (frames-manage-decisions out 7 (list b))
      (setf (window-focused-p a) t)

      ;; --- Registry defaults and the floating switch ---

      (ok "workspaces are tiling by default"
          (workspace-type out 7) :tile)

      (set-workspace-type out 7 :floating)
      (ok "floating switch takes over every window"
          (list (workspace-type out 7)
                (window-floating-p a)
                (window-floating-p b))
          '(:floating t t))
      (ok "floats cascade to distinct positions"
          (/= (window-float-x a) (window-float-x b))
          t)

      ;; --- New arrivals float on a floating workspace ---

      (frames-manage-decisions out 7 (list c))
      (ok "arrival on a floating workspace floats (not tiled)"
          (list (window-floating-p c) (and (window-frame c) t))
          '(t nil))

      ;; --- Switching to scroll unfloats into the ribbon ---

      (set-workspace-type out 7 :scroll)
      (ok "scroll switch unfloats everything into columns"
          (list (workspace-type out 7)
                (window-floating-p a)
                (window-floating-p b)
                (window-floating-p c)
                (mapcar #'window-proxy
                        (getf (scroll-state out 7) :columns)))
          '(:scroll nil nil nil (:GA :GB :GC)))

      ;; --- Switching back to tile: ribbon exits, dynamic re-adoption ---

      (set-workspace-type out 7 :tile)
      (ok "tile switch exits the ribbon"
          (list (workspace-type out 7)
                (scroll-workspace-p out 7))
          '(:tile nil))
      (ok "windows are frameless for re-adoption"
          (list (window-frame a) (window-frame b) (window-frame c))
          '(nil nil nil))
      (dolist (win (list a b c))
        (frames-manage-decisions out 7 (list win)))
      (ok "dynamic tiler re-adopts with splits"
          (length (frame-leaves (frame-tree-for out 7)))
          3)

      ;; --- The C-t l cycle: tile -> scroll -> floating -> tile ---

      (cycle-workspace-type out)
      (ok "cycle 1: tile -> scroll"
          (workspace-type out 7) :scroll)
      (cycle-workspace-type out)
      (ok "cycle 2: scroll -> floating"
          (list (workspace-type out 7)
                (window-floating-p a))
          '(:floating t))
      (cycle-workspace-type out)
      (ok "cycle 3: floating -> tile"
          (list (workspace-type out 7)
                (window-floating-p a))
          '(:tile nil))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
