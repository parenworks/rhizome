;;; Tests for the shared software-surface primitives
;;; (surface.lisp): the ONE definition of the row-major offset
;;; formula, with bounds checking and premultiplied semantics.
;;; The transposed form (y + W·x) struck this codebase five times;
;;; these tests pin the correct one.
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

    ;; --- the formula itself ---
    ;; 100-wide buffer: (99,10) must be 10*100+99 = 1099.
    ;; The transposed form gives 99+100*10 = ... wait, both give
    ;; 1099 here; the DISCRIMINATING coordinate pair:
    (ok "index (2,3) in 100-wide = 302"
        (surface-index 100 50 2 3) 302)
    (ok "index (3,2) in 100-wide = 203"
        (surface-index 100 50 3 2) 203)
    ;; THE bug case: (80,2) in a 100-wide, 10-tall buffer.
    ;; Correct: 2*100+80 = 280. Transposed: 2+8000 = 8002 (out of
    ;; the 1000-pixel buffer entirely).
    (ok "index (80,2) in 100x10 = 280 (transposed would be 8002)"
        (surface-index 100 10 80 2) 280)

    ;; --- bounds checking ---
    (ok "x out of range → nil" (surface-index 100 50 100 10) nil)
    (ok "negative x → nil" (surface-index 100 50 -1 10) nil)
    (ok "y out of range → nil" (surface-index 100 50 10 50) nil)
    (ok "negative y → nil" (surface-index 100 50 10 -1) nil)

    ;; --- put/get round-trip on a real buffer ---
    (let* ((w 50) (h 30)
           (ptr (cffi:foreign-alloc :unsigned-int :count (* w h))))
      (put-pixel-argb ptr w h 49 29 #xdeadbeef)
      (ok "put/get round-trip at the far corner"
          (get-pixel-argb ptr w h 49 29) #xdeadbeef)
      ;; out-of-range write is a silent no-op — no fault
      (put-pixel-argb ptr w h 50 30 #x12345678)
      (put-pixel-argb ptr w h -1 0 #x12345678)
      (put-pixel-argb ptr w h 0 -1 #x12345678)
      (ok "out-of-range writes are no-ops (no fault)"
          (get-pixel-argb ptr w h 49 29) #xdeadbeef)
      (cffi:foreign-free ptr))

    ;; --- premultiplied make-argb ---
    ;; full alpha: channels pass through (r=10 g=20 b=30)
    (ok "make-argb full alpha"
        (make-argb 10 20 30 :alpha 255) #xFF0A141E)
    ;; half alpha: channels halve
    (ok "make-argb half alpha halves channels"
        (list (logand 255 (ash (make-argb 100 100 100 :alpha 127) -16)))
        '(50))
    ;; c* <= a always holds (the premultiply contract)
    (let ((violations 0))
      (loop for r in '(0 50 128 255)
            do (loop for a in '(0 1 64 128 200 255)
                     do (let ((p (make-argb r r r :alpha a)))
                          (when (> (logand 255 (ash p -16)) (logand 255 (ash p -24)))
                            (incf violations)))))
      (ok "make-argb premultiply c* <= a" violations 0))
    ;; clamping
    (ok "make-argb clamps overrange channels"
        (make-argb 999 999 999 :alpha 999)
        #xFFFFFFFF)

    ;; --- blend math ---
    (let* ((w 4) (h 1)
           (ptr (cffi:foreign-alloc :unsigned-int :count 4)))
      ;; start transparent, blend a solid red over it
      (setf (cffi:mem-aref ptr :unsigned-int 0) 0)
      (blend-pixel ptr w h 0 0 255 0 0 :alpha 255)
      (ok "opaque blend replaces"
          (get-pixel-argb ptr w h 0 0) #xFFFF0000)
      ;; blend half-green over full red (premultiplied over):
      ;; half source-alpha means the dst contributes half too —
      ;; r drops to ~127 (not 255), g rises
      (blend-pixel ptr w h 0 0 0 255 0 :alpha 128)
      (let ((p (get-pixel-argb ptr w h 0 0)))
        (ok "blend halves the destination at half source-alpha"
            (logand 255 (ash p -16)) 127)
        (ok "blend accumulates in premultiplied space (g rises)"
            (> (logand 255 (ash p -8)) 0) t))
      ;; alpha-0 blend leaves the destination intact
      (let ((before (get-pixel-argb ptr w h 0 0)))
        (blend-pixel ptr w h 0 0 255 255 255 :alpha 0)
        (ok "alpha-0 blend is a no-op"
            (get-pixel-argb ptr w h 0 0) before))
      (cffi:foreign-free ptr))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
