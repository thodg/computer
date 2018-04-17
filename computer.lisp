
(in-package :common-lisp)

(defpackage :computer
  (:use :common-lisp)
  (:export #:*skip*
           #:*stop*
           #:ti
           #:te
           #:tp
           #:t_
           #:tv
           #:tw
           #:t-
           #:ub16
           #:t--
           #:boot
           #:it
           #:computer-op))

(in-package :computer)

(defun dbg (fmt &rest args)
  (apply #'format t fmt args)
  (fresh-line)
  (force-output)
  nil)

(defparameter *default-t-size* 1024)

(defun *t (&optional (size *default-t-size*))
  (list
   0
   0
   0
   (make-array `(,size)
	       :element-type '(unsigned-byte 8)
	       :initial-element 10)))

(defvar *t*)

(defun ti (&optional (a-t *t*))
  (the unsigned-byte (first a-t)))

(defsetf ti (&optional (a-t '*t*)) (i)
  (let ((tp (gensym "TP-")))
    `(let ((,tp (tp)))
       (if (<= ,i ,tp)
	   (setf (first ,a-t) ,i)
	   (error "ti <- ~X~%tp ~X" ,i ,tp)))))

(defun te (&optional (a-t *t*))
  (the unsigned-byte (second a-t)))

(defsetf te (&optional (a-t '*t*)) (e)
  (let ((tp (gensym "TP-")))
    `(let ((,tp (tp)))
       (if (<= ,tp ,e)
	   (setf (second ,a-t) ,e)
	   (error "tp ~X~%te <- ~X" ,tp ,e)))))

(defun tp (&optional (a-t *t*))
  (the (integer 0) (third a-t)))

(defsetf tp (&optional (a-t '*t*)) (p)
  (let ((ti (gensym "TI-"))
	(te (gensym "TE-")))
    `(let ((,ti (ti))
	   (,te (te)))
       (if (and (<= ,ti ,p)
		(< ,p ,te))
	   (setf (third ,a-t) ,p)
	   (error "ti ~X~%tp <- ~X~%te ~X" ,ti ,p ,te)))))

(defun t_ (&optional (a-t *t*))
  (the (array (unsigned-byte 8) (*))
       (fourth a-t)))

(defun tv (p &optional (a-t *t*))
  (declare (type (unsigned-byte 16) p))
  (aref (t_ a-t) p))

(defun tw (b &optional e (a-t *t*))
  (declare (type (unsigned-byte 8) b))
  (setf (aref (t_ a-t) (or e (te a-t))) b)
  (unless e
    (incf (te a-t)))
  b)

(defsetf tv (p &optional (a-t '*t*)) (value)
  `(tw ,value ,p ,a-t))

(defun t- ()
  (tv (decf (te))))

(defun ub16 (b1 b0)
  (declare (type (unsigned-byte 8) b1 b0))
  (+ (* 256 b1) b0))

(defun t-- ()
  (let* ((b0 (t-))
	 (b1 (t-)))
    (ub16 b1 b0)))

(defparameter *default-g-size* 1024)

(defun *g (&optional (size *default-g-size*))
  (list
   0
   0
   (make-array `(,size)
	       :element-type '(unsigned-byte 8)
	       :initial-element 1)))

(defvar *g*)

(defun gi (&optional (g *g*))
  (the (unsigned-byte 16) (first g)))

(defsetf gi (&optional (g '*g*)) (i)
  `(setf (first ,g) ,i))

(defun ga (&optional (g *g*))
  (the (unsigned-byte 16) (second g)))

(defsetf ga (&optional (g '*g*)) (a)
  `(setf (second ,g) ,a))

(defun g_ (&optional (g *g*))
  (the (array (unsigned-byte 8) (*))
       (third g)))

(defun gr (&optional i (*g* *g*))
  (let ((gi (or i (gi))))
    (unless (< gi (length (g_)))
      (error "off g"))
    (prog1 (aref (g_) gi)
      (unless i
	(setf (gi) (+ gi 1))))))

(defun gw (x &optional a (*g* *g*))
  (let ((ga (or a (ga))))
    (unless (< ga (length (g_)))
      (error "off g"))
    (setf (aref (g_) ga) x)
    (unless a
      (setf (ga) (1+ ga)))
    x))

(defun g/stream (stream)
  (let ((*g* (*g (file-length stream))))
    (setf (gi) 0
	  (ga) 0)
    (loop
       (let ((byte (read-byte stream nil :eof)))
	 (if (eq :eof byte)
	     (return)
	     (gw byte))))
    *g*))

(defun g/path (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (g/stream in)))

(defvar *w*
  (make-array '(256)
	      :element-type '(unsigned-byte 8)))

(defun is-v? (b vl vp)
  (dotimes (v vl)
    (if (= (gr (+ vp v)) b)
	(return t))))

(defun yri (vl vp il ip)
  (dotimes (i il)
    (let ((gb (gr (+ ip i)))
	  (tb (tv (+ (tp) i))))
      (if (is-v? gb vl vp)
	  (setf (aref *w* gb) tb)
	  (unless (= gb tb)
	    (return-from yri)))))
  (incf (tp) il)
  t)

(defun yro-1 (vl vp ol of op)
  (declare (ignore of))
  (unless (< (te) (length (t_)))
    (return-from yro-1))
  (dotimes (o (1- ol))
    (let ((gb (gr (+ op o))))
      (if (is-v? gb vl vp)
	  (tw (aref *w* gb))
	  (tw gb))))
  t)

(defvar *stop*)
(defvar *skip*)

(defun noop (&rest args)
  (declare (ignore args))
  ())

(defparameter *op-arity*
  (make-array '(256)
	      :element-type '(unsigned-byte 8)
	      :initial-element 255))

(defparameter *op-fun*
  (make-array '(256)
	      :element-type 'symbol
	      :initial-element 'noop))

(defun computer-op (n arity label)
  (setf (aref *op-arity* n) arity
        (aref *op-fun* n) label))

(defun yro-2 (vl vp ol of op)
  (declare (ignore of))
  (let (aa)
    (dotimes (o (1- ol))
      (let ((gb (gr (+ op (- ol 2 o)))))
	(push (if (is-v? gb vl vp)
		  (aref *w* gb)
		  gb)
	      aa)))
    (let* ((n (first aa))
	   (a (aref *op-arity* n))
           (f (aref *op-fun* n)))
      (unless (< a 255)
	(error "unknown op #x~X" n))
      (dbg "op ~S ~S" f aa)
      (values t (apply f (rest aa))))))

#+nil
(trace yro yro-1 yro-2)

(defun yro (vl vp ol of op)
  (and (or (zerop (logand of 1))
	   (yro-1 vl vp ol of op))
       (or (zerop (logand of 2))
	   (yro-2 vl vp ol of op))))

(defun dbg-yri (rp rl il)
  (format t "~2,'0X ~2,'0D :" rp rl)
  (dotimes (i il)
    (format t " ~2,'0X" (tv (+ (- (tp) il) i))))
  (fresh-line)
  (force-output)
  nil)

(defun yr (rp rl)
  (let* ((vl (gr))
	 (vp (gi))
	 (il (gr (+ vp vl)))
	 (ip (+ vp vl 1))
	 (ol (gr (+ ip il)))
	 (of (gr (+ ip il 1)))
	 (op (+ ip il 2)))
    (setf (gi) ip)
    (and (yri vl vp il ip)
	 (or (dbg-yri rp rl il)
	     (unless (null *skip*)
	       (setf *skip* nil)
	       (dbg "skip")
	       t)
	     (yro vl vp ol of op)))))

(defun y ()
  (loop
     (unless (and (< (tp) (te) (length (t_)))
		  (null *stop*))
       (return))
     (setf (gi) 0)
     (loop
	(let ((gi (gi)))
	  (unless (< gi (ga))
	    (error "bad op at ~2,'0X" (tp)))
	  (let ((rl (gr)))
	    (if (yr gi rl)
		(return)
		(setf (gi) (+ gi rl))))))))

(defun z ()
  (with-open-file (stream ".t_"
			  :direction :output
			  :element-type '(unsigned-byte 8)
			  :if-exists :supersede)
    (format t "te ~S~%" (te))
    (write-sequence (t_) stream :end (te))))
  
(defun t/stream (stream &optional (*t* *t*))
  (loop
     (let ((byte (read-byte stream nil :eof)))
       (when (eq :eof byte)
	 (y)
	 (z)
	 (return))
       (tw byte))))

;(untrace y yr yri yro gr gi ga t/stream tv is-v? it tw)
;(untrace gw tw te)

(defun t/path (path &optional (a-t *t*))
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (t/stream in a-t)))

(defun boot ()
  (setf (first *t*) 0
	(second *t*) 0
	(third *t*) 0)
  (dotimes (i (length (t_)))
    (tw 0 i)))

(defun it (&rest sources)
  (let ((*g* (g/path ".g"))
	(*t* (*t))
	(*stop* nil)
	(*skip* nil))
    ;(format t "G ~S~%" *g*)
    (boot)
    (dolist (src sources)
      (if (eq src '-)
	  (t/stream *standard-input*)
	  (t/path src)))))

#+nil (load "arch/simple-ops")
#+nil (computer:it "com")
