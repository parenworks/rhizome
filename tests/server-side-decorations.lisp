;;; Tests for decoration negotiation (window.lisp): decoration_hint
;;; handling, the use_ssd / use_csd decision, flushing inside the
;;; manage sequence only, the (:windows :decorations) config key, and
;;; set_tiled edges for tiled windows. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (ssd '()) (csd '()) (tiled '()))
  (labels ((record (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (clear-calls ()
             (setf ssd '() csd '() tiled '()))
           (make-win (proxy &optional hint)
             (let ((win (make-instance 'rhizome-window :proxy proxy)))
               (setf (gethash proxy *windows*) win)
               (when hint
                 (handle-window-event win :decoration-hint hint))
               win))
           (manage-pass ()
             (on-manage-sequence)))
    (macrolet ((ok (name form expected)
                 `(record ,name
                          (handler-case ,form
                            (error (e) (list :error (princ-to-string e))))
                          ,expected)))

      ;; Stub river: record decoration and tiled-state requests
      (setf (symbol-function 'rwm:river-window-v1.use-ssd)
            (lambda (proxy) (push proxy ssd) nil))
      (setf (symbol-function 'rwm:river-window-v1.use-csd)
            (lambda (proxy) (push proxy csd) nil))
      (setf (symbol-function 'rwm:river-window-v1.set-tiled)
            (lambda (proxy edges) (push (list proxy edges) tiled) nil))
      ;; Manage-pass steps unrelated to decorations
      (dolist (fn '(setup-default-keybindings install-keybindings-if-needed
                    enable-pointer-bindings-if-needed float-op-manage-pass
                    flush-fullscreen-requests request-manage-dirty
                    message))
        (setf (symbol-function fn)
              (lambda (&rest args) (declare (ignore args)) nil)))
      (setf *outputs* (make-hash-table)
            *windows* (make-hash-table)
            *pending-autofocus* nil
            *takeover-state* nil)

      ;; --- wayflan delivers the hint as a dashed keyword ---
      (ok "decoration_hint values decode to dashed keywords"
          (mapcar (lambda (v)
                    (xyz.shunter.wayflan.client:wl-enum-keyword
                     'rwm:river-window-v1.decoration-hint v))
                  '(0 1 2 3))
          '(:only-supports-csd :prefers-csd :prefers-ssd :no-preference))

      ;; --- the hint is recorded, nothing is sent outside a sequence ---
      (clear-calls)
      (let ((win (make-win :w-ssd :prefers-ssd)))
        (ok "decoration hint is recorded on the window"
            (window-decoration-hint win) :prefers-ssd)
        (ok "hint event sends no request outside the manage sequence"
            (list ssd csd) '(nil nil))
        (manage-pass)
        (ok "manage pass tells a prefers-ssd window to use SSD"
            ssd '(:w-ssd))
        (clear-calls)
        (manage-pass)
        (ok "unchanged window is not told again"
            ssd '()))

      ;; --- which hints get use_ssd ---
      (setf *windows* (make-hash-table))
      (clear-calls)
      (make-win :w-csd-pref :prefers-csd)
      (make-win :w-nopref :no-preference)
      (make-win :w-csd-only :only-supports-csd)
      (make-win :w-no-hint)
      (manage-pass)
      (ok "every SSD-capable window is told to use SSD"
          (sort (copy-list ssd) #'string<)
          '(:w-csd-pref :w-nopref))
      (ok "no use_csd under the server default"
          csd '())

      ;; --- a later hint re-sends ---
      (clear-calls)
      (let ((win (gethash :w-csd-only *windows*)))
        (handle-window-event win :decoration-hint :prefers-ssd)
        (ok "changed hint is not sent before the manage pass"
            ssd '())
        (manage-pass)
        (ok "manage pass sends use_ssd after the hint changes"
            ssd '(:w-csd-only)))
      (clear-calls)
      (handle-window-event (gethash :w-nopref *windows*)
                           :decoration-hint :prefers-ssd)
      (manage-pass)
      (ok "a repeated hint on an SSD window re-sends use_ssd"
          ssd '(:w-nopref))

      ;; --- the config key ---
      (ok "default decorations mode is :server"
          (config-getf '(:windows) :decorations) :server)
      (clear-calls)
      (let ((*config* (merge-plist *config*
                                   '(:windows (:decorations :client)))))
        (make-win :w-client :prefers-ssd)
        (manage-pass)
        (ok ":client sends no use_ssd"
            ssd '())
        (ok ":client restores CSD only on windows told to use SSD"
            (sort (copy-list csd) #'string<)
            '(:w-csd-only :w-csd-pref :w-nopref)))

      ;; --- set_tiled for tiled windows ---
      (setf *windows* (make-hash-table)
            *output-workspace* (make-hash-table)
            *window-workspace* (make-hash-table)
            *minimized-windows* (make-hash-table)
            *frame-trees* (make-hash-table :test 'equal)
            *current-frames* (make-hash-table :test 'equal))
      (clear-calls)
      (let ((out (make-instance 'rhizome-output :proxy :ssd-out
                                :width 1920 :height 1080))
            (win (make-win :w-tiled)))
        (setf (gethash out *output-workspace*) 1
              (window-output win) out)
        (assign-window-workspace win 1)
        (setf (symbol-function 'rwm:river-window-v1.propose-dimensions)
              (lambda (&rest args) (declare (ignore args)) nil))
        (frames-manage-decisions out 1 (list win))
        (ok "tiled window is told it is tiled on all four edges"
            tiled '((:w-tiled (:top :bottom :left :right))))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (sb-ext:exit :code 1))))
