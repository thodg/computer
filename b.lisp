
(in-package :common-lisp-user)

(defpackage :gr
  (:use :common-lisp)
  (:export #:gr
           #:gr-label
           #:gr-in
           #:gr-out
           #:gr-variables
           #:gr-check-out-variables
           #:gr-bindings
           #:gr-in-bound
           #:gr-out-bound
           #:define-gr))

(defclass gr ()
  ((label :initarg :label
          :reader gr-label
          :type symbol)
   (in :initarg :in
       :reader gr-in
       :type list)
   (out :initarg :out
        :initform ()
        :reader gr-out
        :type list)
   (variables :type list)
   (bindings :type list)
   (in-bound :type list)
   (out-bound :type list)))

(defgeneric gr-variables (gr))
(defgeneric gr-check-out-variables (gr))
(defgeneric gr-bindings (gr))
(defgeneric gr-in-bound (gr))
(defgeneric gr-out-bound (gr))

(defun variables (list)
  (let ((v ()))
    (dolist (l list)
      (unless (not (symbolp l))
        (pushnew l v)))
    (nreverse v)))

(defmethod gr-variables ((gr gr))
  (if (slot-boundp gr 'variables)
      (slot-value gr 'variables)
      (setf (slot-value gr 'variables)
            (variables (gr-in gr)))))

(defmethod gr-check-out-variables ((gr gr))
  (let ((variables (gr-variables gr)))
    (dolist (o (gr-out gr))
      (unless (or (not (symbolp o))
                  (find o variables))
        (error "undefined variable ~S" o)))))

(defmethod gr-bindings ((gr gr))
  (if (slot-boundp gr 'bindings)
      (slot-value gr 'bindings)
      (setf (slot-value gr 'bindings)
            (let ((in (gr-in gr))
                  (out (gr-out gr))
                  a)
              (gr-check-out-variables gr)
              (dolist (var (gr-variables gr))
                (dotimes (n 256)
                  (unless (or (find n in)
                              (find n out)
                              (rassoc n a))
                    (push `(,var . ,n) a)
                    (return))))
              (nreverse a)))))

(defmethod gr-in-bound ((gr gr))
  (if (slot-boundp gr 'in-bound)
      (slot-value gr 'in-bound)
      (setf (slot-value gr 'in-bound)
            `(,@(unless (not (gr2? gr))
                  `(,(gr-opcode gr)))
              ,@(sublis (gr-bindings gr) (gr-in gr))))))

(defmethod gr-out-bound ((gr gr))
  (if (slot-boundp gr 'out-bound)
      (slot-value gr 'out-bound)
      (setf (slot-value gr 'out-bound)
            (sublis (gr-bindings gr) (gr-out gr)))))

(defvar *rules*
  ())

(defun define-gr (label in out &rest body)
  (let ((n (make-instance 'gr :label label :in in :out out)))
    (unless (not (find label *rules* :key #'gr-label))
      (warn "Redefining GR ~S" label)
      (setf *rules* (remove label *rules* :key #'gr-label)))
    (dolist (r *rules*)
      (unless (not (equalp (gr-in-bound n) (gr-in-bound r)))
        (error "Duplicate input for rules ~S and ~S"
               (gr-label r) (gr-label n)))
      (unless (not (equalp (gr-out-bound n) (gr-out-bound r)))
        (warn "Duplicate output for rules ~S and ~S"
              (gr-label r) (gr-label n))))
    (push n *rules*)))

(defmacro gr (label in out)
  `(define-gr ',label ',in ',out))

(defun write-gr (gr stream)
  (flet ((b (&rest bytes)
           (dolist (b bytes)
             (etypecase b
               (list (apply #'b b))
               ((unsigned-byte 8) (write-byte b stream))))))
    (let* ((bindings (gr-bindings gr))
           (bindings-length (length bindings))
           (in (gr-in-bound gr))
           (in-length (length in))
           (out (gr-out-bound gr))
           (out-length (length out)))
      (b (+ 1
            1 bindings-length
            1 in-length
            1 out-length)
         bindings-length
         bindings
         in-length
         in
         out-length
         out))))

(defun write-g (&key (path ".g") (rules *rules*) stream)
  (if stream
      #1=(dolist (gr rules)
           (write-gr gr stream))
      (with-open-file (stream path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        #1#)))

(gr stop () (2) ()
  (setf *stop* t))

(gr stop@ () (2) ()
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
