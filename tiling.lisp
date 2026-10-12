;;; Tiling engine for Stumpway
;;;
;;; This module implements StumpWM-style tiling logic. It operates on
;;; stumpway-window objects and integrates with the manage/render sequence
;;; loop in wm-protocol.lisp.
;;;
;;; Layouts are functions that take a list of windows and the output
;;; dimensions, and return a list of (window x y width height) tuples.

(in-package #:stumpway)

;;; --- Frame structure --

(defstruct frame
  "A rectangular region of an output that can hold windows."
  (x 0 :type integer)
  (y 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  (parent nil :type (or null frame))
  (first-child nil :type (or null frame))
  (second-child nil :type (or null frame))
  (split-direction nil :type (or null (member :horizontal :vertical)))
  (windows nil :type list))

;;; --- Layout registry --

(defvar *layouts* (make-hash-table)
  "Maps layout names (keywords) to layout functions.")

(defmacro define-layout (name (windows width height) &body body)
  "Define a new layout function.
NAME is a keyword naming the layout.
The function receives WINDOWS (a list of stumpway-window instances),
WIDTH, HEIGHT and must return a list of (window x y w h) tuples."
  `(setf (gethash ,name *layouts*)
         (lambda (,windows ,width ,height)
           ,@body)))

(defun get-layout-function (name)
  "Look up a layout function by name."
  (or (gethash name *layouts*)
      (error "Unknown layout: ~A" name)))

(defun switch-layout (output layout-name)
  "Switch OUTPUT to use LAYOUT-NAME."
  (setf (output-current-layout output) layout-name)
  (format t "~&; Stumpway: switched to ~A~%" layout-name)
  (run-hook :layout-changed output layout-name)
  (request-manage-dirty))

;;; --- Built-in layouts --

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
  "Monocle layout - each window gets the full screen, only one visible at a time."
  (when windows
    (list (list (first windows) 0 0 width height))))

;;; --- Manage/render sequence integration --

(defun apply-layout-for-output (output)
  "Called during a manage sequence. Propose dimensions for each window
on OUTPUT's current workspace based on the current layout."
  (let* ((all-wins (all-windows))
         (workspace (current-workspace output))
         (assigned (remove-if
                     (lambda (win)
                       (or (window-minimized-p win)
                           (not (eq (window-output win) output))
                           (and (window-workspace win)
                                (not (eql (window-workspace win)
                                          workspace)))))
                     all-wins))
         (unassigned (remove-if
                       (lambda (win)
                         (or (window-minimized-p win)
                             (window-output win)))
                       all-wins))
         (wins (append assigned unassigned))
         (layout-fn (get-layout-function (output-current-layout output)))
         (placements
           (if (and (> (output-width output) 0)
                    (> (output-height output) 0))
               (funcall layout-fn wins (output-width output)
                        (max 0 (- (output-height output)
                                  (bar-reserved-height))))
               nil)))
    (format t "~&; Stumpway [debug]: apply-layout - ws ~D, ~A assigned, ~A unassigned, ~A placements~%"
            workspace (length assigned) (length unassigned) (length placements))
    (loop for (win x y w h) in placements
          do (progn
               (propose-window-dimensions win w h)
               ;; Stamp newly placed windows with this output's workspace
               (assign-window-workspace win workspace)
               (setf (window-x win) (+ x (output-x output)))
               (setf (window-y win) (+ y (output-y output)
                                         (bar-reserved-height)))
               (setf (window-output win) output)))))

(defun render-layout-for-output (output)
  "Called during a render sequence. Set positions, borders, and z-order
for each window on OUTPUT's current workspace based on the computed
layout. Windows on other workspaces are hidden."
  (let* ((wins (remove-if #'window-minimized-p
                          (windows-on-workspace output
                                                (current-workspace output))))
         (layout-fn (get-layout-function (output-current-layout output)))
         (placements
           (if (and (> (output-width output) 0)
                    (> (output-height output) 0))
               (funcall layout-fn wins (output-width output)
                        (max 0 (- (output-height output)
                                  (bar-reserved-height))))
               nil)))
    (format t "~&; Stumpway [debug]: render-layout - ~A windows, ~A placements~%"
            (length wins) (length placements))
    (loop for (win x y w h) in placements
          do (progn
               (make-window-node win)
               (set-window-position win
                                    (+ x (output-x output))
                                    (+ y (output-y output)
                                        (bar-reserved-height)))
               (if (window-focused-p win)
                   (set-window-borders win '(:top :bottom :left :right)
                                       2 #x00 #x55 #xff #xff)
                   (set-window-borders win '(:top :bottom :left :right)
                                       1 #x40 #x40 #x40 #xff))
               ;; Only send show/hide on actual state transitions:
               ;; river treats every show/hide request as a state change
               ;; and starts a new manage/render sequence for it, so
               ;; redundant requests each render cause an infinite loop.
               ;; Note: show only when hidden (e.g. switching back to
               ;; this window's workspace) — this is the visible window
               ;; restoration path.
               (when (window-hidden-p win)
                 (show-window win))))
    ;; Hide windows not in the layout: windows on other workspaces,
    ;; minimized windows, and overflow hidden by the layout (monocle).
    (let ((laid-out (mapcar #'first placements)))
      (loop for win in (windows-on-output output)
            unless (member win laid-out)
            do (unless (window-hidden-p win)
                 (hide-window win))))))
;;; --- Frame tree operations (StumpWM-style) --

(defun make-root-frame (width height)
  "Create a root frame covering the entire output."
  (make-frame :width width :height height))

(defun split-frame (frame direction ratio)
  "Split FRAME in DIRECTION (:horizontal or :vertical) at RATIO (0.0-1.0).
Returns the new (second) child frame."
  (let ((first (make-frame :parent frame))
        (second (make-frame :parent frame)))
    (setf (frame-split-direction frame) direction
          (frame-first-child frame) first
          (frame-second-child frame) second)
    (ecase direction
      (:horizontal
       (let ((split-x (floor (* (frame-width frame) ratio))))
         (setf (frame-x first) (frame-x frame)
               (frame-y first) (frame-y frame)
               (frame-width first) split-x
               (frame-height first) (frame-height frame)
               (frame-x second) (+ (frame-x frame) split-x)
               (frame-y second) (frame-y frame)
               (frame-width second) (- (frame-width frame) split-x)
               (frame-height second) (frame-height frame))))
      (:vertical
       (let ((split-y (floor (* (frame-height frame) ratio))))
         (setf (frame-x first) (frame-x frame)
               (frame-y first) (frame-y frame)
               (frame-width first) (frame-width frame)
               (frame-height first) split-y
               (frame-x second) (frame-x frame)
               (frame-y second) (+ (frame-y frame) split-y)
               (frame-width second) (frame-width frame)
               (frame-height second) (- (frame-height frame) split-y)))))
    second))

(defun split-horizontal (output &optional (ratio 1/2))
  "Split the current frame horizontally."
  (declare (ignore output ratio))
  ;; Frame tree operations require per-output frame tracking
  ;; This is a stub for future implementation
  (request-manage-dirty))

(defun split-vertical (output &optional (ratio 1/2))
  "Split the current frame vertically."
  (declare (ignore output ratio))
  (request-manage-dirty))

(defun remove-split (output)
  "Remove the current frame's split."
  (declare (ignore output))
  (request-manage-dirty))

(defun focus-next (output)
  "Focus the next window on OUTPUT."
  (let* ((wins (sort (windows-on-output output)
                     (lambda (a b)
                       (or (< (window-x a) (window-x b))
                           (and (= (window-x a) (window-x b))
                                (< (window-y a) (window-y b)))))))
         (seats (all-seats)))
    (format t "~&; Stumpway [debug]: focus-next - ~D wins, ~D seats~%" (length wins) (length seats))
    (when (and wins seats)
      (let* ((current (seat-focused-window (first seats)))
             (pos (position current wins :test #'eq))
             (next (if pos
                       (or (nth (1+ pos) wins) (first wins))
                       (first wins))))
        (format t "~&; Stumpway [debug]: focus-next - current=~A pos=~A next=~A~%"
                (and current (window-title current)) pos (and next (window-title next)))
        (when next
          (dolist (seat seats)
            (focus-window next seat))
          (request-manage-dirty))))))

(defun focus-previous (output)
  "Focus the previous window on OUTPUT."
  (let* ((wins (sort (windows-on-output output)
                     (lambda (a b)
                       (or (< (window-x a) (window-x b))
                           (and (= (window-x a) (window-x b))
                                (< (window-y a) (window-y b)))))))
         (seats (all-seats)))
    (format t "~&; Stumpway [debug]: focus-prev - ~D wins, ~D seats~%" (length wins) (length seats))
    (when (and wins seats)
      (let* ((current (seat-focused-window (first seats)))
             (pos (position current wins :test #'eq))
             (prev (if pos
                       (let ((prev-pos (mod (1- pos) (length wins))))
                         (nth prev-pos wins))
                       (first (last wins)))))
        (format t "~&; Stumpway [debug]: focus-prev - current=~A pos=~A prev=~A~%"
                (and current (window-title current)) pos (and prev (window-title prev)))
        (when prev
          (dolist (seat seats)
            (focus-window prev seat))
          (request-manage-dirty))))))
