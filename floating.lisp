;;; Floating windows for Rhizome — the StumpWM float experience
;;;
;;; Not everything belongs in a tiling grid: GIMP toolboxes, palette
;;; dialogs, and picture-in-picture video want to keep their own size
;;; and be moved around freely. StumpWM handles these as floats and
;;; so does rhizome:
;;;
;;;   - A floating window keeps its own position and dimensions; the
;;;     tiling layouts never see it, and it stacks above the tiling.
;;;   - On first float we tell the window it is NOT part of the tiling
;;;     (river_window_v1.set_tiled none) so it draws its own shadows
;;;     and decorations, and propose 0x0 so it picks its own size —
;;;     a GIMP toolbox comes up exactly as GIMP intends.
;;;   - Interactive move/resize rides river's pointer op protocol:
;;;     clients request a move (titlebar drag), op_delta events carry
;;;     the cumulative pointer motion, and each manage sequence applies
;;;     it. Sizes are clamped to the window's own min/max hints.
;;;   - Window rules can float apps by regex:
;;;     (rhizome:configure '(:window-rules ((:match "gimp" :float t))))
;;;   - C-t f toggles the focused window between float and tiling.

(in-package #:rhizome)

(defparameter *float-min-size* 50
  "Smallest float dimension a user resize may produce.")

;;; --- Float state ---

(defun float-window (win &key x y width height (animate t))
  "Make WIN float, keeping its current geometry by default (a window
floated from its tiling position must not jump). Explicit :x :y
:width :height overrides place it precisely. Placing at an
explicit position glides when the animation engine is on (expose
and maximize ride this)."
  (if (window-floating-p win)
      ;; Already floating: an explicit reposition glides from the
      ;; current rendered position to the target
      (when (or x y)
        (when (and animate (animations-enabled-p))
          (animation-start win :from-x (window-x win)
                                  :from-y (window-y win)))
        (when x (setf (window-float-x win) x))
        (when y (setf (window-float-y win) y))
        (message "moved: ~A" (or (window-title win)
                                 (window-app-id win) "window"))
        (request-manage-dirty))
      (progn
        (setf (window-floating-p win) t
              (window-float-x win) (or x (window-x win) 0)
              (window-float-y win) (or y (window-y win) 0)
              (window-float-width win) (or width
                                         (when (plusp (window-width win))
                                           (window-width win)))
              (window-float-height win) (or height
                                           (when (plusp (window-height win))
                                             (window-height win))))
        ;; Leave the frame tree: a float must not be laid out as tiled
        (let ((frame (window-frame win)))
          (when frame
            (setf (frame-windows frame) (remove win (frame-windows frame))
                  (window-frame win) nil)))
        ;; First float at an explicit position glides from the
        ;; tiling position; keeping geometry is instant
        (when (and animate (or x y) (animations-enabled-p))
          (animation-start win :from-x (window-x win)
                                  :from-y (window-y win)))
        (message "floated: ~A" (or (window-title win)
                                   (window-app-id win) "window"))
        (request-manage-dirty))))

(defun unfloat-window (win)
  "Return WIN to the tiling."
  (unless (window-floating-p win)
    (return-from unfloat-window))
  (setf (window-floating-p win) nil
        ;; Manual unfloat supersedes any maximize state
        (window-maximized-p win) nil
        (window-maximize-prev win) nil)
  ;; Glide back to wherever the tiling places it
  (when (animations-enabled-p)
    (animation-start win :from-x (window-x win)
                            :from-y (window-y win)))
  (message "tiled: ~A" (or (window-title win) (window-app-id win) "window"))
  (request-manage-dirty))

;;; --- Maximize ---
;;;
;;; river_window_v1.maximize_requested / unmaximize_requested arrive
;;; whenever a client wants to be maximized (xdg_toplevel.set_maximized
;;; under the hood). We honor them as floats covering the output's
;;; usable area — the same geometry the tiling uses, bar strip
;;; included — and restore the exact prior state on unmaximize.

(defun output-usable-area (out)
  "The usable area of OUT as four values (x y width height): output
geometry minus the bar's reserved strip at the top. Matches the
tiling's convention everywhere bar-reserved-height appears."
  (values (output-x out)
          (+ (output-y out) (bar-reserved-height))
          (output-width out)
          (max 0 (- (output-height out) (bar-reserved-height)))))

(defun maximize-window (win)
  "Float WIN to the usable area of its output, saving the
pre-maximize state: tiled windows return to the grid on unmaximize,
floats restore their own geometry. Size hints still clamp (a
fixed-size dialog won't stretch), since they express a preference
the WM is free to exceed but usually shouldn't."
  (when (window-maximized-p win)
    (return-from maximize-window))
  (setf (window-maximize-prev win)
        (if (window-floating-p win)
            (list :floating
                  (window-float-x win) (window-float-y win)
                  (window-float-width win) (window-float-height win))
            :tiled)
        (window-maximized-p win) t)
  (let ((out (window-output win)))
    (when out
      (multiple-value-bind (ux uy uw uh) (output-usable-area out)
        (let ((w (float-clamp-dim win uw :width))
              (h (float-clamp-dim win uh :height)))
          (if (window-floating-p win)
              (setf (window-float-x win) ux
                    (window-float-y win) uy
                    (window-float-width win) w
                    (window-float-height win) h)
              (float-window win :x ux :y uy :width w :height h))))))
  (message "maximized: ~A" (or (window-title win) (window-app-id win) "window"))
  (request-manage-dirty))

(defun unmaximize-window (win)
  "Restore WIN's pre-maximize state."
  (unless (window-maximized-p win)
    (return-from unmaximize-window))
  (let ((prev (window-maximize-prev win)))
    (setf (window-maximized-p win) nil
          (window-maximize-prev win) nil)
    (if (eq prev :tiled)
        (unfloat-window win)
        (progn
          (destructuring-bind (x y w h) (cdr prev)
            (setf (window-float-x win) x
                  (window-float-y win) y
                  (window-float-width win) w
                  (window-float-height win) h))
          (message "unmaximized: ~A"
                   (or (window-title win) (window-app-id win) "window"))
          (request-manage-dirty)))))

(defun toggle-maximize (win)
  "Toggle WIN's maximize (IPC: rhizome-cmd maximize)."
  (when win
    (if (window-maximized-p win)
        (unmaximize-window win)
        (maximize-window win))))

(defun toggle-float (win)
  "Toggle WIN between floating and tiled."
  (if (window-floating-p win)
      (unfloat-window win)
      (float-window win)))

(defun focused-window ()
  "The window with keyboard focus, if any."
  (let ((seats (all-seats)))
    (and seats (seat-focused-window (first seats)))))

(defun toggle-float-focused ()
  "Float/tile the focused window."
  (let ((win (focused-window)))
    (when win
      (toggle-float win))))

;;; --- Geometry helpers ---

(defun float-clamp-position (win)
  "Keep WIN's float position reachable: at least a ~30px strip of the
window stays on its output, so a float can never be lost off-screen."
  (let* ((out (window-output win))
         (w (or (window-float-width win) (window-width win) 100))
         (h (or (window-float-height win) (window-height win) 100)))
    (when out
      (let ((ox (output-x out)) (oy (output-y out))
            (ow (output-width out)) (oh (output-height out)))
        (setf (window-float-x win)
              (min (max (window-float-x win) (- ox (- w 30)))
                   (+ ox ow -30))
              (window-float-y win)
              (min (max (window-float-y win) (- oy (- h 30)))
                   (+ oy oh -30)))))))

(defun float-clamp-dim (win size dimension)
  "Clamp SIZE (a :width or :height) to WIN's min/max size hints and
the *float-min-size* floor. A hint of 0 means no preference."
  (let ((min (ecase dimension
               (:width (window-hint-min-width win))
               (:height (window-hint-min-height win))))
        (max (ecase dimension
               (:width (window-hint-max-width win))
               (:height (window-hint-max-height win)))))
    (setf size (max size (max *float-min-size* (or min 0))))
    (when (and max (plusp max))
      (setf size (min size max)))
    size))

(defun float-apply-resize (win op)
  "Apply the cumulative resize delta of OP (a seat op-state plist) to
WIN, honoring which EDGES the drag started from."
  (let* ((dx (getf op :dx 0)) (dy (getf op :dy 0))
         (edges (getf op :edges)))
    (let ((x (getf op :origin-x)) (y (getf op :origin-y))
          (w (getf op :origin-w)) (h (getf op :origin-h)))
      (when (member :left edges)
        (incf x dx) (decf w dx))
      (when (member :right edges)
        (incf w dx))
      (when (member :top edges)
        (incf y dy) (decf h dy))
      (when (member :bottom edges)
        (incf h dy))
      (setf (window-float-x win) x
            (window-float-y win) y
            (window-float-width win) (float-clamp-dim win w :width)
            (window-float-height win) (float-clamp-dim win h :height)))))

;;; --- Manage/render sequence integration ---
;;;
;;; Called from the tiling engine's passes (tiling.lisp), which own
;;; the manage/render sequence split.

(defun float-propose (win)
  "Manage-sequence requests for one floating window: inform it that it
is not part of the tiling, and propose its float dimensions — 0x0
(= the window picks its own size) until a size is known."
  (set-window-tiled-state win '(:none))
  (if (and (window-float-width win) (window-float-height win))
      (propose-window-dimensions win
                                 (window-float-width win)
                                 (window-float-height win))
      (propose-window-dimensions win 0 0)))

(defun float-manage-decisions (output)
  "Manage-sequence decisions for floating windows on OUTPUT: adopt
new/unassigned floats onto the current workspace, and re-propose the
dimensions of floats on the current workspace."
  (let ((workspace (current-workspace output)))
    (dolist (win (all-windows))
      (unless (or (window-minimized-p win)
                   (not (window-floating-p win)))
        (cond ((null (window-output win))
               ;; Adopt: same as the tiling path stamping placements
               (setf (window-output win) output)
               (assign-window-workspace win workspace)
               ;; configure_bounds hint for fresh floats too
               (recommend-dimension-bounds win)
               (float-propose win))
              ((and (eq (window-output win) output)
                    (eql (window-workspace win) workspace))
               (float-propose win)))))))

(defun float-render-decisions (output tiled-wins)
  "Render-sequence decisions for floating windows on OUTPUT's current
workspace: position each at its float coordinates, border it, show it
if hidden, and stack the floats above the tiling (focused float on
top). Returns the floats actually rendered, so the render pass can
exclude them from its hide logic."
  (let* ((workspace (current-workspace output))
         (floats (remove-if-not
                   (lambda (win)
                     (and (window-floating-p win)
                          (not (window-minimized-p win))
                          (eql (window-workspace win) workspace)))
                   (windows-on-output output)))
         (focused (find-if #'window-focused-p floats)))
    ;; Focused float renders last: it lands on top of the stack.
    (when focused
      (setf floats (append (remove focused floats) (list focused))))
    (let ((below (let ((top (first (last tiled-wins))))
                   (and top (window-node top)))))
      (dolist (win floats)
        (float-clamp-position win)
        (make-window-node win)
        (multiple-value-bind (ax ay)
            (animation-position win (window-float-x win) (window-float-y win))
          (set-window-position win ax ay))
        (multiple-value-bind (r g b) (theme-border-rgb (window-focused-p win))
          (set-window-borders win '(:top :bottom :left :right)
                              (if (window-focused-p win) 2 1)
                              r g b #xff))
        ;; Show only on transition: river treats every show/hide as a
        ;; state change and starts a new manage/render sequence.
        (when (window-hidden-p win)
          (show-window win))
        ;; Stack above the previous node (or the top tiled node)
        (when (window-node win)
          (when below
            (place-node-above (window-node win) below))
          (setf below (window-node win)))))
    floats))

;;; --- Interactive move/resize (river pointer ops) ---

(defun float-op-request (win seat type &optional edges)
  "A client asked to move/resize WIN interactively (titlebar drag, or
an MOD+drag binding). op-start-pointer is a manage-sequence request,
so record the request here and start the op in the next manage pass.
Floating windows only: dragging the tiling is a no-op."
  (when (and (window-floating-p win) (gethash (window-proxy win) *windows*))
    (setf (seat-op-pending seat)
          (list :win win :type type :edges edges))))

(defun window-at-pointer (seat)
  "The window under the pointer: the focused window if the pointer is
inside it, else the topmost float, else any window containing the
point."
  (let ((px (seat-pointer-x seat)) (py (seat-pointer-y seat)))
    (flet ((inside-p (win)
             (and (gethash (window-proxy win) *windows*)
                  (not (window-minimized-p win))
                  (>= px (window-x win))
                  (< px (+ (window-x win) (window-width win)))
                  (>= py (window-y win))
                  (< py (+ (window-y win) (window-height win))))))
      (let ((focused (find-if #'window-focused-p (all-windows))))
        (or (and focused (inside-p focused) focused)
            (find-if (lambda (w) (and (window-floating-p w) (inside-p w)))
                     (all-windows))
            (find-if #'inside-p (all-windows)))))))

(defun pointer-drag-start (seat type)
  "A MOD+drag began on SEAT: StumpWM behavior — dragging a tiled
window floats it first — then an interactive move/resize op starts
(the river pointer-op protocol; see float-op-manage-pass). Right-drag
resizes from the classic bottom-right corner. Middle-click opens
the window context menu (menus.lisp) instead of an op."
  (if (eq type :menu)
      (menu-open-pointer-trigger seat)
      (let ((win (window-at-pointer seat)))
        (when win
          (dolist (s (all-seats))
            (focus-window win s))
          (unless (window-floating-p win)
            (float-window win))
          (float-op-request win seat type
                            (if (eq type :resize) '(:bottom :right) nil))
          (request-manage-dirty)))))

(defun float-op-manage-pass ()
  "Advance interactive float ops during a manage sequence: start
pending ops, apply op deltas (positions are applied by the render
pass; resizes propose dimensions here), and end released ops.
Cumulative deltas make this idempotent: origin + total motion."
  (dolist (seat (all-seats))
    (let ((pending (seat-op-pending seat)))
      (when pending
        (setf (seat-op-pending seat) nil)
        (let ((win (getf pending :win)))
          (when (and win (gethash (window-proxy win) *windows*))
            (setf (seat-op-state seat)
                  (list :win win
                        :type (getf pending :type)
                        :edges (getf pending :edges)
                        :origin-x (window-float-x win)
                        :origin-y (window-float-y win)
                        :origin-w (or (window-float-width win)
                                      (window-width win) 100)
                        :origin-h (or (window-float-height win)
                                      (window-height win) 100)
                        :dx 0 :dy 0 :released nil))
            (rwm:river-seat-v1.op-start-pointer (seat-proxy seat))
            (format t "~&; Rhizome: float ~A op started~%"
                    (getf pending :type))))))
    (let ((op (seat-op-state seat)))
      (when op
        (let ((win (getf op :win)))
          (cond ((or (null win)
                     (not (gethash (window-proxy win) *windows*)))
                 ;; The op's window vanished: drop the op
                 (setf (seat-op-state seat) nil))
                ((getf op :released)
                 (rwm:river-seat-v1.op-end (seat-proxy seat))
                 (setf (seat-op-state seat) nil))
                (t
                 (when (or (not (zerop (getf op :dx 0)))
                           (not (zerop (getf op :dy 0))))
                   (case (getf op :type)
                     (:move
                      (setf (window-float-x win)
                            (+ (getf op :origin-x) (getf op :dx))
                            (window-float-y win)
                            (+ (getf op :origin-y) (getf op :dy)))
                      (float-clamp-position win)
                      ;; No dimensions change to propose; trigger the
                      ;; manage/render cycle that applies positions
                      (request-manage-dirty))
                     (:resize
                      (float-apply-resize win op)
                      (propose-window-dimensions win
                                                 (window-float-width win)
                                                 (window-float-height win))))))))))))
