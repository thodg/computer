
(in-package :common-lisp-user)

(defpackage :simple-ops
  (:use :common-lisp
        :computer)
  (:export #:stop
           #:stop]]
           #:jump]]
           #:jump
           #:[tp
           #:dec-tp]
           #:dec-tp
           #:inc-tp]
           #:inc-tp
           #:test]
           #:[r]]
           #:[r
           #:[dec-r]
           #:[dec-r
           #:[inc-r]
           #:[inc-r
           #:[copy
           #:[
           #:w]]
           #:w
           #:w]]]
           #:w@]
           #:[dec-w]]
           #:[dec-w]
           #:[inc-w]]
           #:[inc-w]
           #:ti]]
           #:ti
           #:te]]
           #:te
           #:[lt]]]]
           #:[lt]]
           #:[lte]]]]
           #:[lte]]
           #:[=]]]]
           #:[=]]
           #:[not]
           #:[and]]
           #:[or]]
           #:[xor]]
           #:[<<]]
           #:[>>]]
           #:[decrement]
           #:[increment]))

(op stop ()
  (setf *stop* t))

(op stop]] ()
  (setf *stop* t)
  (setf (second *t*) (t--)))

;; 16 bits stack jump
(op jump]] ()
  (setf (tp) (t--)))

;; 16 bits explicit jump
(op jump (p1 p0)
  (let ((p (ub16 p1 p0)))
    (dbg "tp <- ~X" p)
    (setf (tp) p)))

;; read tp
(op [tp ()
  (tw (tp)))

;; 8 bits decrement stack jump
(op dec-tp] ()
  (decf (tp) (t-)))

;; 8 bits decrement explicit jump
(op dec-tp (p)
  (decf (tp) p))

;; 8 bits increment stack jump
(op inc-tp] ()
  (incf (tp) (t-)))

;; 8 bits increment explicit jump
(op inc-tp (p)
  (incf (tp) p))

;; conditional
(op test] ()
  (unless (not (zerop (t-)))
    (setf *skip* t)))

;; 16 bits stack read
(op [r]] ()
  (tw (tv (t--))))

;; 16 bits explicit read
(op [r (p1 p0)
  (tw (tv (ub16 p1 p0))))

;; 8 bits decrement stack read
(op [dec-r] ()
  (tw (tv (- (tp) (t-)))))

;; 8 bits decrement explicit read
(op [dec-r (p)
  (tw (tv (- (tp) p))))

;; 8 bits increment stack read
(op [inc-r] ()
  (tw (tv (+ (tp) (t-)))))

;; 8 bits increment explicit read
(op [inc-r (p)
  (tw (tv (+ (tp) p))))

;; copy
(op [copy ()
  (tw (tv (1- (te)))))

;; write
(op [ (b)
  (tw b))

;; 16 bits stack write
(op w]] (b)
  (tw b (t--)))

;; 16 bits explicit write
(op w (b e1 e0)
  (tw b (ub16 e1 e0)))

;; 16 bits stack write
(op w]]] ()
  (let* ((e (t--))
	 (b (t-)))
    (tw b e)))

;; 16 bits explicit write
(op w@] (e1 e0)
  (tw (t-) (ub16 e1 e0)))

;; 8 bits decrement stack write
(op [dec-w]] ()
  (let* ((p (t-))
	 (b (t-)))
    (tw b (- (tp) p))))

;; 8 bits decrement explicit write
(op [dec-w] (p)
  (tw (t-) (- (tp) p)))

;; 8 bits increment stack write
(op [inc-w]] ()
  (let* ((b (t-))
	 (p (t-)))
    (tw b (+ (tp) p))))

;; 8 bits increment explicit write
(op [inc-w] (p)
  (tw (t-) (+ (tp) p)))

;; 16 bits stack set ti
(op ti]] ()
  (setf (ti) (t--)))

;; 16 bits set ti
(op ti (i1 i0)
  (setf (ti) (ub16 i1 i0)))

;; 16 bits stack set te
(op te]] ()
  (setf (te) (t--)))

;; 16 bits set te
(op te (e1 e0)
  (setf (te) (ub16 e1 e0)))

;; 16 bits lt
(op [lt]]]] ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (< b1 b0)
	    255
	    0))))

;; 8 bits lt
(op [lt]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (< b1 b0)
	    255
	    0))))

;; 16 bits lte
(op [lte]]]] ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (<= b1 b0)
	    255
	    0))))

;; 8 bits lte
(op [lte]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (<= b1 b0)
	    255
	    0))))

;; 16 bits eq
(op [=]]]] ()
  (let* ((b0 (t--))
	 (b1 (t--)))
    (declare (type (unsigned-byte 16) b0 b1))
    (tw (if (= b1 b0)
	    255
	    0))))

;; 8 bits eq
(op [=]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (tw (if (= b1 b0)
	    255
	    0))))

;; not
(op [not] ()
  (let ((b (t-)))
    (declare (type (unsigned-byte 8) b))
    (tw (if (zerop b)
	    255
	    0))))

;; and
(op [and]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logand b1 b0))))

;; or
(op [or]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logior b1 b0))))

;; xor
(op [xor]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (logxor b1 b0))))

;; left shift
(op [<<]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (ash b1 b0))))

;; right shift
(op [>>]] ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (declare (type (unsigned-byte 8) b0 b1))
    (tw (ash b1 (- b0)))))

;; decrement
(op [decrement] ()
  (tw (1- (t-))))

;; increment
(op [increment] ()
  (tw (1+ (t-))))
