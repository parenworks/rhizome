;;; Expose for Rhizome — the StumpWM (C-t Return) / macOS overview
;;;
;;; Press C-t Return: every window on the workspace — including the
;;; ones hidden in monocle frames — floats into an aspect-fitted
;;; grid. Pick one with a digit key (1..9, 0), arrows + Return,
;;; or by clicking it; Escape (or the 60s timeout) restores the
;;; previous layout unchanged. Everything snaps back exactly: float
;;; geometry, frame membership, per-frame window order, and focus.
;;;
;;; The pick keyboard rides the prefix capture surface (like iresize);
;;; clicks ride river's window-interaction events, which fire on any
;;; button press over a window — focus moves and ends the expose.

(in-package #:rhizome)

(defparameter *expose-order* nil
  "The exposed windows in grid order (row-major) while expose runs.")
(defparameter *expose-cols* 1
  "Columns in the active expose grid.")
(defparameter *expose-saved* nil
  "Layout snapshot while expose runs: (window-saves frame-saves).")
(defparameter *expose-prev-focus* nil
  "The focused window before expose started (Escape restores it).")

(defun expose-grid (output workspace)
  "Aspect-fit placements (WIN x y w h) for the workspace's windows,
row-major grid over the output's content area."
  (let* ((wins (remove-if
                 (lambda (w)
                   (or (window-minimized-p w)
                       (window-fullscreen-p w)
                       (not (eql (window-workspace w) workspace))
                       (not (eq (window-output w) output))))
                 (all-windows)))
         (n (length wins))
         (cols (max 1 (ceiling (sqrt n))))
         (rows (max 1 (ceiling n cols)))
         (cw (floor (output-width output) cols))
         (ch (floor (max 0 (- (output-height output)
                               (bar-reserved-height)))
                    rows))
         (gx (output-x output))
         (gy (+ (output-y output) (bar-reserved-height))))
    (values
      (loop for win in wins
            for i from 0
            for col = (mod i cols)
            for row = (floor i cols)
            for cell-x = (+ gx (* col cw))
            for cell-y = (+ gy (* row ch))
            for w = (max 1 (window-width win))
            for h = (max 1 (window-height win))
            ;; 4px breathing room per cell edge
            for scale = (min (/ (- cw 8) w) (/ (- ch 8) h))
            for fw = (max 1 (floor (* w scale)))
            for fh = (max 1 (floor (* h scale)))
            collect (list win
                         (+ cell-x (floor (- cw fw) 2))
                         (+ cell-y (floor (- ch fh) 2))
                         fw
                         fh))
      cols)))

(defun expose-save-state (wins frames)
  "Snapshot for exact restoration: per-window float state and each
frame's full window order."
  (list
    (loop for win in wins
          collect (list win
                        (window-frame win)
                        (window-floating-p win)
                        (window-float-x win)
                        (window-float-y win)
                        (window-float-width win)
                        (window-float-height win)))
    (loop for frame in frames
          collect (cons frame (frame-windows frame)))))

(defun expose-begin (output workspace)
  "Float the workspace's windows into the grid and snapshot the
layout. Returns T if there was anything to expose."
  (multiple-value-bind (placements cols) (expose-grid output workspace)
    (let ((wins (mapcar #'first placements)))
      (when (< (length wins) 2)
        (message "nothing to expose")
        (return-from expose-begin nil))
      (let ((frames (remove-duplicates
                      (remove nil (mapcar #'window-frame wins))
                      :test #'eq)))
        (setf *expose-saved* (expose-save-state wins frames)
              *expose-order* wins
              *expose-cols* cols
              *expose-prev-focus* (find-if #'window-focused-p (all-windows)))
        (dolist (p placements)
          (destructuring-bind (win x y w h) p
            (frame-remove-window win)
            ;; Glide from the window's current rendered position
            ;; into its grid slot (expose-begin knows both)
            (when (animations-enabled-p)
              (animation-start win :from-x (window-x win)
                                      :from-y (window-y win)))
            (setf (window-floating-p win) t
                  (window-float-x win) x
                  (window-float-y win) y
                  (window-float-width win) w
                  (window-float-height win) h)))
        (request-manage-dirty)
        t))))

(defun expose-restore (&optional (picked *expose-prev-focus*))
  "Put every window back exactly where it was before expose, focusing
PICKED (the pre-expose focus by default)."
  (when *expose-saved*
    (destructuring-bind (win-saves frame-saves) *expose-saved*
      (dolist (save win-saves)
        (destructuring-bind (win frame was-float
                              fx fy fw fh)
                            save
          ;; Glide back out of the grid slot to the restored position
          (when (animations-enabled-p)
            (animation-start win :from-x (window-x win)
                                    :from-y (window-y win)))
          (setf (window-floating-p win) was-float
                (window-float-x win) fx
                (window-float-y win) fy
                (window-float-width win) fw
                (window-float-height win) fh)
          (unless was-float
            (setf (window-frame win) frame))))
      (dolist (save frame-saves)
        (setf (frame-windows (first save)) (rest save))))
    (setf *expose-active* nil
          *expose-saved* nil
          *expose-order* nil
          *expose-prev-focus* nil)
    (when picked
      (dolist (seat (all-seats))
        (focus-window picked seat)))
    (request-manage-dirty)))

(defun enter-expose ()
  "C-t Return: the window overview (StumpWM's expose)."
  (let* ((out (first (all-outputs)))
         (ws (current-workspace out)))
    (unless (and out (not *expose-active*))
      (return-from enter-expose))
    (when (expose-begin out ws)
      (enter-prefix-mode)
      (when *prefix-mode-active*
        (setf *expose-active* t
              *prefix-expiry* (+ (get-universal-time) 60))
        (show-message "expose: 1-9/0, arrows+Return, or click; Esc cancels"
                      60)))))

(defun expose-restore-if-active ()
  "Restore the pre-expose layout when a prefix exit ends expose
(Escape, unbound key, timeout)."
  (when *expose-active*
    (expose-restore)))

(defun expose-pick (win)
  "End expose, keeping WIN (and focusing it)."
  (expose-restore win)
  (exit-prefix-mode))

(defun expose-focus-index (delta)
  "Move grid focus by DELTA positions (row-major, clamped)."
  (let* ((order *expose-order*)
         (n (length order))
         (focused (find-if #'window-focused-p order))
         (pos (or (position focused order :test #'eq) 0))
         (next (min (max 0 (+ pos delta)) (1- n))))
    (when (and (/= pos next) (nth next order))
      (dolist (seat (all-seats))
        (focus-window (nth next order) seat))
      (setf *prefix-expiry* (+ (get-universal-time) 60)))))

(defun expose-handle-key (keysym)
  "Handle one keysym during expose: digits pick, arrows move focus,
Return picks the focused window."
  (let ((n (length *expose-order*)))
    (cond
      ;; Digits 1-9 and 0 (the 10th slot); '1' is keysym 49, so the
      ;; grid index is one less than the digit
      ((<= 49 keysym 57)
       (let ((idx (- keysym 49)))
         (if (< idx n)
             (expose-pick (nth idx *expose-order*))
             (message "no window ~D" (1+ idx)))))
      ((= keysym 48)                           ; 0
       (if (>= n 10)
           (expose-pick (nth 9 *expose-order*))
           (message "no window 10")))
      ;; Arrows: move focus through the grid
      ((= keysym 65361) (expose-focus-index -1))          ; Left
      ((= keysym 65363) (expose-focus-index 1))           ; Right
      ((= keysym 65362) (expose-focus-index (- *expose-cols*)))  ; Up
      ((= keysym 65364) (expose-focus-index *expose-cols*))     ; Down
      ;; Return picks the focused window
      ((= keysym 65293)
       (expose-pick (or (find-if #'window-focused-p *expose-order*)
                        (first *expose-order*))))
      (t (message "1-9/0, arrows, Return, Esc"))))
  t)

(defun expose-clicked (win)
  "A window was clicked during expose: that's the pick. (Redefines
the seat.lisp placeholder; called after focus already moved.)"
  (when (and *expose-active* win (member win *expose-order* :test #'eq))
    (expose-pick win)))
