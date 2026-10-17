;;; Tiling engine for Stumpway
;;;
;;; Integrates the frame trees (frames.lisp) and floating windows
;;; (floating.lisp) with river's manage/render sequence loop in
;;; wm-protocol.lisp. The frame tree is the real layout — the StumpWM
;;; model; layout NAMES are frame-tree templates:
;;;
;;;   - :monocle    one frame filling the screen
;;;   - :master-stack  master frame + stack frame side by side
;;;   - :even       a grid with a cell per window
;;;   - custom      define-layout placement lists become the frames
;;;
;;; Switching a workspace's layout rebuilds its tree from the
;;; template; user splits afterwards are kept until the next switch.

(in-package #:stumpway)

;;; --- Layout registry ---

(defvar *layouts* (make-hash-table)
  "Maps layout names (keywords) to layout functions.")

(defmacro define-layout (name (windows width height) &body body)
  "Define a new layout template.
NAME is a keyword naming the layout.
The function receives WINDOWS (a list of stumpway-window instances),
WIDTH, HEIGHT and must return a list of (window x y w h) tuples.
The placements become the frame tree's leaves."
  `(setf (gethash ,name *layouts*)
         (lambda (,windows ,width ,height)
           ,@body)))

(defun get-layout-function (name)
  "Look up a layout function by name."
  (or (gethash name *layouts*)
      (error "Unknown layout: ~A" name)))

;;; Built-in templates, registered so rules and lookups find them

(define-layout :master-stack (windows width height)
  "Classic master/stack: one master on the left, rest stacked on the right."
  (if (<= (length windows) 1)
      (loop for win in windows
            collect (list win 0 0 width height))
      (let* ((master-width (floor width 2))
             (stack-width (- width master-width))
             (stack-count (1- (length windows)))
             (stack-height (floor height stack-count)))
        (cons (list (first windows) 0 0 master-width height)
              (loop for win in (rest windows)
                    for i below stack-count
                    collect (list win master-width
                                  (* i stack-height)
                                  stack-width
                                  stack-height))))))

(define-layout :even (windows width height)
  "Even grid layout - all windows get equal space."
  (let ((count (length windows)))
    (if (<= count 0)
        nil
        (let* ((cols (ceiling (sqrt count)))
               (rows (ceiling count cols))
               (cell-width (floor width cols))
               (cell-height (floor height rows)))
          (loop for win in windows
                for i below count
                for col = (mod i cols)
                for row = (floor i cols)
                collect (list win (* col cell-width)
                              (* row cell-height)
                              cell-width cell-height))))))

(define-layout :monocle (windows width height)
  "Monocle layout - each window gets the full screen, one visible at a time."
  (when windows
    (list (list (first windows) 0 0 width height))))

(defun layout-template-rects (name width height windows)
  "The frame rectangles for layout NAME over WIDTH x HEIGHT, sized for
WINDOWS. Built-ins get exact template rectangles; custom layouts run
as placement functions and their placements become the rectangles."
  (case name
    (:monocle (list (list 0 0 width height)))
    (:master-stack (let ((mw (floor width 2)))
                     (list (list 0 0 mw height)
                           (list mw 0 (- width mw) height))))
    (:even (let* ((count (max 1 (length windows)))
                  (cols (ceiling (sqrt count)))
                  (rows (ceiling count cols))
                  (cw (floor width cols))
                  (ch (floor height rows)))
             (loop for r below rows
                   nconc (loop for c below cols
                               collect (list (* c cw) (* r ch)
                                             (if (= c (1- cols))
                                                 (- width (* c cw))
                                                 cw)
                                             (if (= r (1- rows))
                                                 (- height (* r ch))
                                                 ch))))))
    (t (mapcar (lambda (placement)
                 (list (second placement) (third placement)
                       (fourth placement) (fifth placement)))
               (funcall (get-layout-function name) windows width height)))))

(defun switch-layout (output layout-name)
  "Switch OUTPUT's current workspace to LAYOUT-NAME by rebuilding its
frame tree from the layout's template (windows distributed one per
frame, extras onto the last)."
  (scroll-exit-if-active output)
  (setf (output-current-layout output) layout-name)
  (let* ((workspace (current-workspace output))
         (width (output-width output))
         (height (max 0 (- (output-height output)
                           (bar-reserved-height))))
         (wins (remove-if #'window-minimized-p
                          (windows-on-workspace output workspace))))
    (when (and (> width 0) (> height 0))
      (set-workspace-tree output workspace
                          (layout-template-rects layout-name width height wins)
                          wins)))
  (format t "~&; Stumpway: switched to ~A~%" layout-name)
  (message "~A" layout-name)
  (run-hook :layout-changed output layout-name)
  (request-manage-dirty))

;;; --- StumpWM frame commands (IPC-facing wrappers) ---

(defun split-horizontal (output &optional (ratio 1/2))
  "Split the current frame side-by-side."
  (declare (ignore ratio))
  (split-current-frame output (current-workspace output) :horizontal))

(defun split-vertical (output &optional (ratio 1/2))
  "Split the current frame stacked (top/bottom)."
  (declare (ignore ratio))
  (split-current-frame output (current-workspace output) :vertical))

(defun remove-split (output)
  "Remove the current frame's split, merging into its sibling."
  (remove-current-split output (current-workspace output)))

(defun focus-next (output)
  "Cycle to the next window in the current frame."
  (cycle-frame-window output (current-workspace output) 1))

(defun focus-previous (output)
  "Cycle to the previous window in the current frame."
  (cycle-frame-window output (current-workspace output) -1))

;;; --- Manage/render sequence integration ---

(defun apply-layout-for-output (output)
  "Manage-sequence pass for OUTPUT: stamp new windows onto this
output/workspace, let the frame tree propose dimensions, and let
floats manage themselves."
  (let* ((workspace (current-workspace output))
         (assigned (remove-if
                     (lambda (win)
                       (or (window-minimized-p win)
                           (window-floating-p win)
                           (not (eq (window-output win) output))
                           (and (window-workspace win)
                                (not (eql (window-workspace win)
                                          workspace)))))
                     (all-windows)))
         (unassigned (remove-if
                       (lambda (win)
                         (or (window-minimized-p win)
                             (window-floating-p win)
                             (window-output win)))
                       (all-windows)))
         (wins (append assigned unassigned)))
    ;; Stamp windows onto this output and workspace (idempotent)
    (dolist (win wins)
      (setf (window-output win) output)
      (assign-window-workspace win workspace))
    (if (scroll-workspace-p output workspace)
        (scroll-manage-decisions output workspace wins)
        (frames-manage-decisions output workspace wins))
    (float-manage-decisions output)))

;;; --- Scroll-layout stubs (redefined by scroll.lisp, which loads later) ---

(defun scroll-workspace-p (output workspace)
  "T while the workspace shows the niri-style scroll ribbon.
Placeholder: redefined by scroll.lisp."
  (declare (ignore output workspace))
  nil)

(defun scroll-manage-decisions (output workspace wins)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output workspace wins))
  nil)

(defun scroll-render-decisions (output workspace wins)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output workspace wins))
  nil)

(defun scroll-exit-if-active (output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

;; Same guard as frames.lisp's group stubs: a stub's derived NULL
;; return type must not poison the compiled dispatch sites that the
;; real definitions (returning T) will execute.
(declaim (ftype (function (t t) t) scroll-workspace-p)
         (ftype (function (t t t) t) scroll-manage-decisions
                                 scroll-render-decisions)
         (ftype (function (t) t) scroll-exit-if-active scroll-toggle))

(defun scroll-toggle (output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

(defun scroll-focus-left (&optional output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

(defun scroll-focus-right (&optional output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

(defun scroll-viewport-left (&optional output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

(defun scroll-viewport-right (&optional output)
  "Placeholder: redefined by scroll.lisp."
  (declare (ignore output))
  nil)

(defun render-layout-for-output (output)
  "Render-sequence pass for OUTPUT: the frame tree places and shows
its frames' visible windows, floats stack above them, and everything
else on this output (other workspaces, minimized, non-visible windows
inside frames) hides."
  (let* ((workspace (current-workspace output))
         (wins (remove-if #'window-minimized-p
                          (windows-on-workspace output workspace)))
         (tiled (remove-if #'window-floating-p wins))
         (rendered (if (scroll-workspace-p output workspace)
                       (scroll-render-decisions output workspace tiled)
                       (frames-render-decisions output workspace tiled)))
         (floats (float-render-decisions output rendered)))
    ;; Only send show/hide on actual state transitions: river treats
    ;; every show/hide request as a state change and starts a new
    ;; manage/render sequence for it, so redundant requests each
    ;; render cause an infinite loop.
    (let ((laid-out (append rendered floats)))
      (loop for win in (windows-on-output output)
            unless (member win laid-out)
            do (unless (window-hidden-p win)
                 (hide-window win))))))
