;;; Shared software-surface pixel primitives
;;;
;;; ONE place for the row-major offset formula. The transposed
;;; form (y + W·x instead of y·W + x) has struck this codebase five
;;; times — bar, launcher (twice), decorations, and once in a test
;;; reader — because both forms look plausible, compile clean, and
;;; pass small-coordinate tests; only wide/tall draws fault past
;;; the buffer. Every surface now draws through these helpers, so
;;; the formula cannot be transposed per-surface and out-of-range
;;; writes are caught instead of corrupting memory.
;;;
;;; Buffers are premultiplied ARGB u32 (wl_shm argb8888):
;;; little-endian layout byte0=B byte1=G byte2=R byte3=A.

(in-package #:rhizome)

(declaim (inline surface-index))

(defun surface-index (width height x y)
  "Row-major byte-4 pixel index for (X Y) in a WIDTHxHEIGHT buffer,
or NIL when out of range. The ONLY definition of the offset
formula in the codebase."
  (declare (type integer width height x y))
  (if (and (>= x 0) (< x width)
           (>= y 0) (< y height))
      (+ (* width y) x)
      nil))

(declaim (inline put-pixel-argb))

(defun put-pixel-argb (ptr width height x y argb)
  "Store a premultiplied ARGB u32 pixel at (X Y); out-of-range
writes are a NO-OP (logged once per surface, not per pixel, by
the caller if it cares). PTR is the mmap'd shm base."
  (let ((idx (surface-index width height x y)))
    (unless (null idx)
      (setf (cffi:mem-aref ptr :unsigned-int idx) argb))))

(declaim (inline get-pixel-argb))

(defun get-pixel-argb (ptr width height x y)
  "Read a premultiplied ARGB u32 pixel at (X Y), or 0 out of
range."
  (let ((idx (surface-index width height x y)))
    (if (null idx)
        0
        (cffi:mem-aref ptr :unsigned-int idx))))

(defun make-argb (r g b &key (alpha 255))
  "A premultiplied pixel: channels are straight (0-255), alpha
0-255; channels are scaled by alpha here so the stored pixel
always satisfies c* <= a."
  (let ((a (min 255 (max 0 (round alpha))))
        (r (min 255 (max 0 (round r))))
        (g (min 255 (max 0 (round g))))
        (b (min 255 (max 0 (round b)))))
    (logior (ash a 24)
            (ash (round (* r a) 255) 16)
            (ash (round (* g a) 255) 8)
            (round (* b a) 255))))

(defun put-pixel (ptr width height x y r g b &key (alpha 255))
  "Store a straight-color pixel with the given alpha (premultiplied
on the way in). Out-of-range is a no-op."
  (put-pixel-argb ptr width height x y (make-argb r g b :alpha alpha)))

(defun blend-pixel (ptr width height x y r g b &key (alpha 255))
  "Alpha-BLEND a straight color over the existing pixel (premultiplied
over, per Wayland): result = src + dst·(1-src_a). For text and
icon antialiasing edges. ALPHA 0 leaves the destination intact."
  (let ((idx (surface-index width height x y)))
    (unless (null idx)
      (let* ((a (min 255 (max 0 (round alpha))))
              (r (min 255 (max 0 (round r))))
              (g (min 255 (max 0 (round g))))
              (b (min 255 (max 0 (round b))))
              (sa a)
             (old (cffi:mem-aref ptr :unsigned-int idx))
             (da (logand 255 (ash old -24)))
             (dr (logand 255 (ash old -16)))
             (dg (logand 255 (ash old -8)))
             (db (logand 255 old))
             (inv (- 255 sa))
             (out-a (min 255 (+ sa (round (* da inv) 255))))
             (out-r (min 255 (+ (round (* r sa) 255)
                                (round (* dr inv) 255))))
             (out-g (min 255 (+ (round (* g sa) 255)
                                (round (* dg inv) 255))))
             (out-b (min 255 (+ (round (* b sa) 255)
                                (round (* db inv) 255)))))
        (setf (cffi:mem-aref ptr :unsigned-int idx)
              (logior (ash out-a 24)
                      (ash out-r 16)
                      (ash out-g 8)
                      out-b))))))

;;; --- Rounded-rect primitives (the launcher treatment, shared) ---

(declaim (inline rounded-rect-test))

(defun rounded-rect-test (x y w h radius)
  "For pixel (X Y) in a WxH rounded rect with corner RADIUS:
(VALUES INSIDE-P BORDER-P). Corners are quarter circles; the
border is the 1px ring at the rect's edge. The ONE definition —
the launcher, menus, message window, prompts, and selection
pills all route through it."
  (let* ((rr (max 0 radius))
         (cx (cond ((< x rr) rr)
                   ((> x (- w rr 1)) (- w rr 1))
                   (t x)))
         (cy (cond ((< y rr) rr)
                   ((> y (- h rr 1)) (- h rr 1))
                   (t y)))
         (edge (or (= x 0) (= x (1- w))
                   (= y 0) (= y (1- h)))))
    (let* ((in-corner (or (and (< x rr) (< y rr))
                          (and (< x rr) (> y (- h rr 1)))
                          (and (> x (- w rr 1)) (< y rr))
                          (and (> x (- w rr 1)) (> y (- h rr 1)))))
           (d (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2)))))
      (cond
        ((and in-corner (> d rr)) (values nil nil))
        ((or edge (and in-corner (>= d (- rr 1.5)))) (values t t))
        (t (values t nil))))))

(defun fill-rounded-rect (ptr width height x y w h r g b
                              &key (alpha 255) (radius 0))
  "Fill a (rounded when RADIUS > 0) rect at (X Y W H) — the
selection-pill primitive. Bounds-checked; premultiplied."
  (loop for dy below h
        for py = (+ y dy)
        while (< py height) do
    (loop for dx below w
          for px = (+ x dx)
          while (and (< px width) (>= px 0) (>= py 0)) do
      (when (and (>= px 0) (>= dy 0))
        (multiple-value-bind (inside border-p)
            (rounded-rect-test dx dy w h radius)
          (declare (ignore border-p))
          (when inside
            (put-pixel-argb ptr width height px py
                            (make-argb r g b :alpha alpha))))))))

(defun paint-rounded-surface (ptr width height r g b
                              &key (alpha 255) (radius 0)
                                (border-r 255) (border-g 255)
                                (border-b 255))
  "Full-surface rounded fill with a 1px hairline border — the
launcher/menu/message/prompt background. Alpha-cut corners;
premultiplied."
  (loop for y below height
        do (loop for x below width
                 do (multiple-value-bind (inside border-p)
                        (rounded-rect-test x y width height radius)
                      (when inside
                        (if border-p
                            (put-pixel-argb ptr width height x y
                                            (make-argb border-r border-g border-b
                                                       :alpha alpha))
                            (put-pixel-argb ptr width height x y
                                            (make-argb r g b :alpha alpha))))))))
