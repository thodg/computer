
(in-package :computer)

;; stop
(g2 #x00 ()
  (setf *stop* t))

;; stop @
(g2 #x01 ()
  (setf *stop* t)
  (setf (second *t*) (t--)))

;; 16 bits stack jump
(g2 #x10 ()
  (setf (tp) (t--)))

;; 16 bits explicit jump
(g2 #x12 (p1 p0)
  (let ((p (ub16 p1 p0)))
    (dbg "tp <- ~X" p)
    (setf (tp) p)))

;; read tp
(g2 #x13 ()
  (tw (tp)))

;; 8 bits decrement stack jump
(g2 #x14 ()
  (decf (tp) (t-)))

;; 8 bits decrement explicit jump
(g2 #x15 (p)
  (decf (tp) p))

;; 8 bits increment stack jump
(g2 #x16 ()
  (incf (tp) (t-)))

;; 8 bits increment explicit jump
(g2 #x17 (p)
  (incf (tp) p))

;; conditional
(g2 #x20 ()
  (unless (not (zerop (t-)))
    (setf *skip* t)))

;; 16 bits stack read
(g2 #x30 ()
  (tw (tv (t--))))

;; 16 bits explicit read
(g2 #x31 (p1 p0)
  (tw (tv (ub16 p1 p0))))

;; 8 bits decrement stack read
(g2 #x32 ()
  (tw (tv (- (tp) (t-)))))

;; 8 bits decrement explicit read
(g2 #x33 (p)
  (tw (tv (- (tp) p))))

;; 8 bits increment stack read
(g2 #x34 ()
  (tw (tv (+ (tp) (t-)))))

;; 8 bits increment explicit read
(g2 #x35 (p)
  (tw (tv (+ (tp) p))))

;; copy
(g2 #x3F ()
  (tw (tv (1- (te)))))

;; write
(g2 #x40 (b)
  (tw b))

;; 16 bits stack write
(g2 #x41 (b)
  (tw b (t--)))

;; 16 bits explicit write
(g2 #x42 (b e1 e0)
  (tw b (ub16 e1 e0)))

;; 16 bits stack write
(g2 #x43 ()
  (let* ((e (t--))
	 (b (t-)))
    (tw b e)))

;; 16 bits explicit write
(g2 #x44 (e1 e0)
  (tw (t-) (ub16 e1 e0)))

;; 8 bits decrement stack write
(g2 #x45 ()
  (let* ((p (t-))
	 (b (t-)))
    (tw b (- (tp) p))))

;; 8 bits decrement explicit write
(g2 #x46 (p)
  (tw (t-) (- (tp) p)))

;; 8 bits increment stack write
(g2 #x47 ()  (let* ((b (t-))
	 (p (t-)))
    (tw b (+ (tp) p))))

;; 8 bits increment explicit write
(g2 #x48 (p)
  (tw (t-) (+ (tp) p)))

;; 16 bits stack set ti
(g2 #x50 ()
  (setf (ti) (t--)))

;; 16 bits set ti
(g2 #x51 (i1 i0)
  (setf (ti) (ub16 i1 i0)))

;; 16 bits stack set te
(g2 #x60 ()
  (setf (te) (t--)))

;; 16 bits set te
(g2 #x61 (e1 e0)
  (setf (te) (ub16 e1 e0)))

;; 16 bits lt
(g2 #x70 ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (< b1 b0)
	    255
	    0))))

;; 8 bits lt
(g2 #x71 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (< b1 b0)
	    255
	    0))))

;; 16 bits lte
(g2 #x72 ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (<= b1 b0)
	    255
	    0))))

;; 8 bits lte
(g2 #x73 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (<= b1 b0)
	    255
	    0))))

;; 16 bits eq
(g2 #x74 ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (= b1 b0)
	    255
	    0))))

;; 8 bits eq
(g2 #x75 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (= b1 b0)
	    255
	    0))))

;; not
(g2 #x80 ()
  (let ((b (t-)))
    (declare (type (unsigned-byte 8) b))
    (tw (if (zerop b)
	    255
	    0))))

;; and
(g2 #x81 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logand b1 b0))))

;; or
(g2 #x82 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logior b1 b0))))

;; xor
(g2 #x83 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logxor b1 b0))))

;; left shift
(g2 #x84 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (ash b1 b0))))

;; right shift
(g2 #x85 ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (ash b1 (- b0)))))

;; decrement
(g2 #x90 ()
  (tw (1- (t-))))

;; increment
(g2 #x91 ()
  (tw (1+ (t-))))
