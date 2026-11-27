;;; Multiple outputs for Rhizome
;;;
;;; StumpWM multi-head semantics: one output is current at a time.
;;; New windows open there, output-relative commands act on it, and
;;; commands move focus or the focused window between outputs.
;;;
;;; The current output is, in order:
;;;   1. the output of the focused window;
;;;   2. the output last selected by focus-output or
;;;      move-window-to-output (an empty output clears keyboard focus,
;;;      so 1 does not shadow it);
;;;   3. the output under the seat's pointer;
;;;   4. the first output.

(in-package #:rhizome)

(defvar *selected-output* nil
  "Output most recently selected by focus-output or
move-window-to-output, or NIL.")

(defun live-output-p (output)
  "True when OUTPUT is a registered output."
  (and output (member output (all-outputs) :test #'eq) t))

(defun output-at-point (x y)
  "The output whose rectangle contains the global point X, Y, or NIL."
  (find-if (lambda (out)
             (and (<= (output-x out) x)
                  (< x (+ (output-x out) (output-width out)))
                  (<= (output-y out) y)
                  (< y (+ (output-y out) (output-height out)))))
           (all-outputs)))

(defun pointer-output ()
  "The output under the first seat's pointer, or NIL."
  (let ((seat (first (all-seats))))
    (and seat
         (output-at-point (seat-pointer-x seat) (seat-pointer-y seat)))))

(defun current-output ()
  "The output the operator is working on, or NIL when none exists.
See the file header for the order of precedence."
  (let ((outputs (all-outputs)))
    (when outputs
      (let ((focused (focused-window)))
        (or (and focused
                 (gethash (window-proxy focused) *windows*)
                 (live-output-p (window-output focused))
                 (window-output focused))
            (and (live-output-p *selected-output*) *selected-output*)
            (pointer-output)
            (first outputs))))))

(defun adjacent-output (output delta)
  "The output DELTA steps from OUTPUT in all-outputs-sorted order (top
to bottom, then left to right), wrapping at either end. OUTPUT itself
when it is the only one."
  (let* ((sorted (all-outputs-sorted))
         (pos (position output sorted :test #'eq)))
    (when sorted
      (nth (mod (+ (or pos 0) delta) (length sorted)) sorted))))

(defun output-in-direction (output direction)
  "The nearest output beside OUTPUT in DIRECTION (:left, :right, :up
or :down), measured between centres, or NIL at the edge."
  (flet ((cx (o) (+ (output-x o) (floor (output-width o) 2)))
         (cy (o) (+ (output-y o) (floor (output-height o) 2))))
    (let ((best nil) (best-distance nil))
      (dolist (o (all-outputs) best)
        (unless (eq o output)
          (let* ((dx (- (cx o) (cx output)))
                 (dy (- (cy o) (cy output)))
                 (ahead (ecase direction
                          (:left (minusp dx))
                          (:right (plusp dx))
                          (:up (minusp dy))
                          (:down (plusp dy))))
                 (distance (+ (abs dx) (abs dy))))
            (when (and ahead
                       (or (null best-distance) (< distance best-distance)))
              (setf best o best-distance distance))))))))

(defun output-focus-target (output)
  "The window focus-output gives focus to on OUTPUT: the current
frame's window, or, on a workspace whose windows sit in no frame
(floating or scroll), its first visible window. NIL otherwise."
  (let* ((ws (current-workspace output))
         (wins (remove-if #'window-minimized-p
                          (windows-on-workspace output ws))))
    (or (frame-current-window (current-frame-for output ws))
        (and (notany #'window-frame wins)
             (first wins)))))

(defun focus-output (output &key (warp t))
  "Make OUTPUT current. Focus its current frame's window; with none,
clear keyboard focus so the next new window opens on OUTPUT. With
WARP (the default) and (:pointer :warp-on-switch), warp the pointer
to OUTPUT's centre. Returns OUTPUT, or NIL when it is not a
registered output."
  (when (live-output-p output)
    (setf *selected-output* output)
    (let ((win (output-focus-target output)))
      (dolist (seat (all-seats))
        (if win
            (focus-window win seat)
            (queue-clear-focus seat))))
    (when (and warp (config-getf '(:pointer) :warp-on-switch :default t))
      (push (list :warp
                  (+ (output-x output) (floor (output-width output) 2))
                  (+ (output-y output) (floor (output-height output) 2)))
            *pending-pointer-warps*))
    (setf *pending-autofocus* nil)
    (request-manage-dirty)
    output))

(defun move-window-to-output (win output)
  "Move WIN to OUTPUT's current workspace and focus it; return WIN, or
NIL when nothing moves. A float keeps its offset from the output's
origin. A tiled WIN joins OUTPUT's current frame at once, as a new
window does, so a render pass before the next manage pass cannot
stack it on that frame's window."
  (let ((source (and win (window-output win)))
        (workspace (and output (current-workspace output))))
    (when (and win (live-output-p output) (not (eq source output)))
      (frame-remove-window win)
      (when (and (window-floating-p win) source
                 (window-float-x win) (window-float-y win))
        (incf (window-float-x win) (- (output-x output) (output-x source)))
        (incf (window-float-y win) (- (output-y output) (output-y source))))
      (setf (window-output win) output
            (window-minimized-p win) nil
            *selected-output* output)
      (assign-window-workspace win workspace)
      (unless (or (window-floating-p win)
                  (floating-workspace-p output workspace)
                  (scroll-workspace-p output workspace))
        (frame-adopt-window win output workspace))
      (dolist (seat (all-seats))
        (focus-window win seat))
      (setf *pending-autofocus* nil)
      (request-manage-dirty)
      win)))

;;; --- Focusing an output from its empty desktop ---
;;;
;;; :stumpwm  MOD+Left on empty desktop focuses the output under the
;;;           pointer; pointer motion never changes the current output.
;;; :niri     the same, and the pointer moving onto another output's
;;;           empty desktop makes that output current.
;;;
;;; The mode is (:workspaces :output-focus) for the workspace the
;;; target output shows, else (:pointer :output-focus), else :stumpwm.

(defvar *pointer-last-output* (make-hash-table :test 'eq)
  "Seat -> the output under its pointer at the last pointer event.")

(defvar *pointer-entered-output* (make-hash-table :test 'eq)
  "Seat -> (OUTPUT . GENERATION) for its pointer's latest crossing
onto OUTPUT that has neither acted nor been dropped, or NIL.
GENERATION is *focus-generation* at the crossing; an entry whose
GENERATION differs from the current count never acts. Set on each
crossing to another output and cleared when the crossing acts or is
dropped, so one crossing focuses an output at most once.")

(defun output-focus-mode (output)
  "The empty-desktop focus mode for OUTPUT's current workspace:
:STUMPWM or :NIRI. Any other configured value reads as :STUMPWM."
  (let* ((overrides (config-getf '(:workspaces) :output-focus))
         (mode (or (and output (listp overrides)
                        (getf overrides (current-workspace output)))
                   (config-getf '(:pointer) :output-focus))))
    (if (eq mode :niri) :niri :stumpwm)))

(defun focus-output-at-pointer (seat)
  "Focus the output under SEAT's pointer without warping the pointer.
Returns that output, or NIL when the pointer is on none."
  (let ((output (output-at-point (seat-pointer-x seat)
                                 (seat-pointer-y seat))))
    (and output (focus-output output :warp nil))))

(defun pointer-motion-output-focus (seat)
  "Handle SEAT's pointer moving to its recorded position. A crossing
onto another output stays pending until it acts or is dropped. Under
:NIRI it acts once, when the pointer first rests on empty desktop on
that output: it focuses the output without warping the pointer. It
is dropped with no effect in any of these cases:
  - the output is already current;
  - the output's mode is :STUMPWM;
  - keyboard focus or the current output changed after the crossing.
Motion within one output never takes back an output chosen from the
keyboard."
  (let ((output (output-at-point (seat-pointer-x seat)
                                 (seat-pointer-y seat))))
    (unless (eq output (gethash seat *pointer-last-output*))
      (setf (gethash seat *pointer-last-output*) output
            (gethash seat *pointer-entered-output*)
            (and output (cons output *focus-generation*))))
    (let* ((entry (gethash seat *pointer-entered-output*))
           (entered (car entry)))
      (when entry
        (cond ((or (/= (cdr entry) *focus-generation*)
                   (not (eq (output-focus-mode entered) :niri))
                   (eq entered (current-output)))
               (setf (gethash seat *pointer-entered-output*) nil))
              ((window-at-pointer seat)
               ;; Over a window: wait for empty desktop on this output
               nil)
              (t
               (setf (gethash seat *pointer-entered-output*) nil)
               (focus-output entered :warp nil)))))))

;;; --- Command functions ---

(defun focus-next-output ()
  "Focus the next output, wrapping (StumpWM's snext)."
  (let ((out (current-output)))
    (when out (focus-output (adjacent-output out 1)))))

(defun focus-previous-output ()
  "Focus the previous output, wrapping (StumpWM's sprev)."
  (let ((out (current-output)))
    (when out (focus-output (adjacent-output out -1)))))

(defun focus-output-direction (direction)
  "Focus the output beside the current one in DIRECTION. No wrap."
  (let ((out (current-output)))
    (when out (focus-output (output-in-direction out direction)))))

(defun focus-output-left ()
  "Focus the output to the left of the current one."
  (focus-output-direction :left))

(defun focus-output-right ()
  "Focus the output to the right of the current one."
  (focus-output-direction :right))

(defun move-focused-window-to-output (output)
  "Move the focused window to OUTPUT; focus follows it."
  (let ((win (focused-window)))
    (if win
        (move-window-to-output win output)
        (message "no focused window"))))

(defun move-window-next-output ()
  "Move the focused window to the next output, wrapping."
  (let ((win (focused-window)))
    (when (and win (window-output win))
      (move-focused-window-to-output
       (adjacent-output (window-output win) 1)))))

(defun move-window-previous-output ()
  "Move the focused window to the previous output, wrapping."
  (let ((win (focused-window)))
    (when (and win (window-output win))
      (move-focused-window-to-output
       (adjacent-output (window-output win) -1)))))

(defun move-window-output-direction (direction)
  "Move the focused window to the output beside it in DIRECTION."
  (let ((win (focused-window)))
    (when (and win (window-output win))
      (let ((target (output-in-direction (window-output win) direction)))
        (when target
          (move-focused-window-to-output target))))))

(defun move-window-output-left ()
  "Move the focused window to the output on its left."
  (move-window-output-direction :left))

(defun move-window-output-right ()
  "Move the focused window to the output on its right."
  (move-window-output-direction :right))
