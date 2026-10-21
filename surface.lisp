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
