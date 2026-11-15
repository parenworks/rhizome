;;; rhizome-display — the display configurator module.
;;;
;;; The display menu: brightness (brightnessctl steps and levels),
;;; night light (hyprsunset 4000K), and per-output modes/scale
;;; through the core's zwlr_output_management tracking
;;; (display.lisp — heads, modes, display-apply). Loaded with
;;; (rhizome:load-module "rhizome-display"); the bar's :display
;;; module's :on-click "rhizome-cmd display-menu" drives it.

(in-package #:rhizome)

(defun display-brightness ()
  "Current brightness percentage as a string, or NIL."
  (handler-case
      (let* ((out (uiop:run-program '("brightnessctl" "info")
                                    :output :string :ignore-error-status t))
             (pc (position #\( out))
             (pe (position #\% out)))
        (when (and pc pe (> pe pc))
          (subseq out (1+ pc) pe)))
    (error () nil)))

(defun brightness-step-argument (delta)
  "Return the brightnessctl relative value for a signed DELTA percent:
\"N%+\" to raise, \"N%-\" to lower. A leading dash is never emitted,
since brightnessctl parses one as an option."
  (format nil "~D%~:[+~;-~]" (abs delta) (minusp delta)))

(defun brightness-absolute-argument (percent)
  "Return the brightnessctl absolute value \"N%\" for PERCENT (0-100)."
  (format nil "~D%" percent))

(defun brightness-step-command (delta)
  "Return the argv that steps brightness by signed DELTA percent."
  (list "brightnessctl" "set" (brightness-step-argument delta)))

(defun brightness-absolute-command (percent)
  "Return the argv that sets brightness to PERCENT."
  (list "brightnessctl" "set" (brightness-absolute-argument percent)))

(defun display-step-brightness (delta)
  "Raise or lower brightness by signed DELTA percent and show a message.
The brightnessctl exit status is ignored."
  (uiop:run-program (brightness-step-command delta) :ignore-error-status t)
  (message "brightness ~:[+~;-~]~D%" (minusp delta) (abs delta)))

(defun display-set-brightness (percent)
  "Set brightness to PERCENT and show a message.
The brightnessctl exit status is ignored."
  (uiop:run-program (brightness-absolute-command percent)
                    :ignore-error-status t)
  (message "brightness ~D%" percent))

(defun display-night-light-on-p ()
  "Whether hyprsunset (night light) is running."
  (plusp (length (uiop:run-program '("pgrep" "-x" "hyprsunset")
                                   :output :string
                                   :ignore-error-status t))))

(defun display-toggle-night-light ()
  "Toggle the night light gamma filter."
  (if (display-night-light-on-p)
      (progn (uiop:run-program '("pkill" "-x" "hyprsunset")
                               :ignore-error-status t)
             (message "night light off"))
      (progn (uiop:launch-program '("hyprsunset" "-t" "4000"))
             (message "night light on (4000K)"))))

(defun display-output-menu (state)
  "Per-output submenu: modes and scale."
  (let ((head (getf state :head)))
    (select-from-menu
      (format nil "~A — ~A"
              (getf state :name) (or (getf state :description) ""))
      (append
        ;; modes (current one marked)
        (mapcar (lambda (m)
                  (let* ((label (display-mode-label m))
                         (current (eq m (getf state :current-mode))))
                    (cons (if current
                              (concatenate 'string label " ✓")
                              label)
                          (lambda () (display-apply head :mode m)))))
                (getf state :modes))
        ;; scale
        (mapcar (lambda (s)
                  (let ((current (= s (getf state :scale))))
                    (cons (if current
                              (format nil "Scale ~,2f ✓" s)
                              (format nil "Scale ~,2f" s))
                          (lambda () (display-apply head :scale s)))))
                '(1 1.25 1.5 2))))))

(define-command :display-menu (output args)
  "The display configurator: brightness, night light, outputs."
  (declare (ignore output args))
  (select-from-menu
    (format nil "Display (~A% bright)" (or (display-brightness) "?"))
    (append
      (list (cons "Brightness +10%" (lambda () (display-step-brightness 10)))
            (cons "Brightness -10%" (lambda () (display-step-brightness -10)))
            (cons "Brightness 100%" (lambda () (display-set-brightness 100)))
            (cons "Brightness 50%" (lambda () (display-set-brightness 50)))
            (cons "Toggle night light" (lambda () (display-toggle-night-light))))
      ;; one entry per output head → mode/scale submenu
      (mapcar (lambda (state)
                (cons (format nil "Output ~A (~A)"
                              (getf state :name)
                              (display-mode-label (getf state :current-mode)))
                      (lambda () (display-output-menu state))))
              (display-heads)))))

(define-command :group-type (output args)
  "Report the current workspace's group type."
  (declare (ignore args))
  (workspace-type output (current-workspace output)))

(define-command :set-group-type (output args)
  "Set the current workspace's group type: tile, scroll or floating."
  (let ((type (and args (first args))))
    (set-workspace-type output (current-workspace output)
                        (cond ((string= type "tile") :tile)
                              ((string= type "scroll") :scroll)
                              ((string= type "floating") :floating)
                              (t type)))))

(define-command :cycle-group-type (output args)
  "Cycle the group type: tile -> scroll -> floating -> tile (C-t l)."
  (declare (ignore args))
  (cycle-workspace-type output))

(define-command :balance (output args)
  "Equalize every split in the workspace (StumpWM: balance-frames)."
  (declare (ignore args))
  (frame-balance output (current-workspace output)))

(define-command :fclear (output args)
  "Clear the current frame; its windows move to the other frames
(StumpWM: fclear)."
  (declare (ignore args))
  (frame-clear output (current-workspace output)))

(define-command :exchange-left (output args)
  "Swap the current frame's window with the frame to the left
(StumpWM: exchange-direction)."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :left))

(define-command :exchange-right (output args)
  "Swap the current frame's window with the frame to the right."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :right))

(define-command :exchange-up (output args)
  "Swap the current frame's window with the frame above."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :up))

(define-command :exchange-down (output args)
  "Swap the current frame's window with the frame below."
  (declare (ignore args))
  (exchange-window-direction output (current-workspace output) :down))
