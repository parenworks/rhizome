;;; Tests for decoration surfaces (decorations.lisp): the
;;; pseudo-shadow lifecycle, protocol sequencing, config gating,
;;; and the shadow-draw math. River-touching functions are
;;; stubbed; the memfd/mmap buffer path runs for real.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (offset-calls nil) (sync-calls 0)
      (commit-calls 0) (destroy-calls 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (reset ()
           (clrhash *decorations*)
           (setf offset-calls nil sync-calls 0 commit-calls 0)
           (setf (symbol-function 'wl:wl-compositor.create-surface)
                 (lambda (&rest a) (declare (ignore a)) :fake-surface))
           (setf (symbol-function 'rwm:river-window-v1.get-decoration-below)
                 (lambda (&rest a) (declare (ignore a)) :fake-deco))
           (setf (symbol-function 'rwm:river-decoration-v1.set-offset)
                 (lambda (p x y)
                   (declare (ignore p))
                   (push (list x y) offset-calls)))
           (setf (symbol-function 'rwm:river-decoration-v1.sync-next-commit)
                 (lambda (p) (declare (ignore p)) (incf sync-calls)))
           (setf (symbol-function 'wl:wl-surface.attach)
                 (lambda (&rest a) (declare (ignore a)) nil))
           (setf (symbol-function 'wl:wl-surface.damage)
                 (lambda (&rest a) (declare (ignore a)) nil))
           (setf (symbol-function 'wl:wl-surface.commit)
                 (lambda (&rest a) (declare (ignore a)) (incf commit-calls)))
           (setf (symbol-function 'wl:wl-surface.destroy)
                 (lambda (&rest a) (declare (ignore a)) nil))
           (setf (symbol-function 'wl:wl-buffer.destroy)
                 (lambda (&rest a) (declare (ignore a)) nil))
           (setf (symbol-function 'wl:wl-shm-pool.destroy)
                 (lambda (&rest a) (declare (ignore a)) nil))
           (setf (symbol-function 'wl:wl-shm.create-pool)
                 (lambda (&rest a) (declare (ignore a)) :fake-pool))
           (setf (symbol-function 'wl:wl-shm-pool.create-buffer)
                 (lambda (&rest a) (declare (ignore a)) :fake-buffer))
           (setf *compositor* :fake-compositor)
           (setf *shm* :fake-shm)))

    ;; --- config gating ---
    (reset)
    (configure '(:decorations (:enabled nil :radius 12)))
    (ok "disabled config → radius 0"
        (decoration-radius) 0)
    (configure '(:decorations (:enabled t :radius 12)))
    (ok "enabled config → radius 12"
        (decoration-radius) 12)
    (configure '(:decorations (:enabled t :radius 10 :alpha 0.4)))
    (ok "alpha config flows through"
        (getf (decoration-config) :alpha) 0.4)

    ;; --- lifecycle: floats get decorations, tiled don't ---
    (reset)
    (let ((win (make-instance 'rhizome-window :proxy :deco-w1)))
      (setf (gethash :deco-w1 *windows*) win)
      (setf (window-floating-p win) t)
      (setf (window-float-width win) 200
            (window-float-height win) 100)
      (decorations-render-pass)
      (let ((state (gethash win *decorations*)))
        (ok "floating window gets a decoration" (and state t) t)
        (ok "decoration proxy recorded"
            (getf state :proxy) :fake-deco)
        ;; surface 200x100 + 2*10 band each side
        (ok "surface sized window + 2*radius"
            (list (getf state :w) (getf state :h)) '(220 120))
        (ok "offset sent once, (-r -r)"
            offset-calls '((-10 -10)))
        ;; sync_next_commit precedes the commit (protocol order)
        (ok "sync_next_commit called" (plusp sync-calls) t)
        (ok "surface committed" (plusp commit-calls) t)
        (ok "offset sent only once after a second pass"
            (progn (decorations-render-pass) offset-calls)
            '((-10 -10)))
        ;; resize redraws
        (setf (window-float-width win) 300)
        (let ((before commit-calls))
          (decorations-render-pass)
          (ok "resize triggers a redraw"
              (> commit-calls before) t))
        ;; un-float removes the decoration
        (setf (window-floating-p win) nil)
        (decorations-render-pass)
        (ok "un-float removes the decoration"
            (gethash win *decorations*) nil)))

    ;; --- the shadow math (real buffer) ---
    (reset)
    (configure '(:decorations (:enabled t :radius 8 :alpha 0.5
                               :focused-alpha 0.8)))
    (let ((state (list :ptr nil :focused nil)))
      ;; 30x20 window + 8px band → 46x36 surface
      (setf (getf state :ptr)
            (cffi:foreign-alloc :unsigned-int :count (* 46 36)))
      (decoration-draw state 46 36 8 0.5 0.8 122 162 247)
      (flet ((px (x y)
               (cffi:mem-aref (getf state :ptr) :unsigned-int (+ (* 46 y) x))))
        ;; outside the band: fully transparent
        (ok "outer rim transparent" (px 0 0) 0)
        ;; inside the band near the window edge: visible alpha
        (let ((edge (px 9 15)))
          (ok "band pixel has alpha"
              (> (ash edge -24) 20) t))
        ;; premultiplied: every channel ≤ its alpha
        (let ((violations 0))
          (loop for x below 46
                do (loop for y below 36
                         do (let ((p (px x y)))
                              (let ((a (ash p -24))
                                    (r (logand 255 (ash p -16)))
                                    (g (logand 255 (ash p -8)))
                                    (b (logand 255 p)))
                                (when (or (> r a) (> g a) (> b a))
                                  (incf violations))))))
          (ok "premultiplied c* ≤ a everywhere" violations 0)))
      (cffi:foreign-free (getf state :ptr)))

    ;; --- cleanup safety ---
    (reset)
    (ok "cleanup on absent window is a no-op"
        (progn (cleanup-window-decoration
                 (make-instance 'rhizome-window :proxy :nope))
               t)
        t)

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1)))) 
