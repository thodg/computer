
(in-package :common-lisp-user)

(load "binary")

(defpackage :program
  (:use :common-lisp
        :binary)
  (:shadow #:write)
  (:export
   #:*b*
   #:program
   #:b
   #:b0
   #:b1
   #:ub8
   #:code@=
   #:code@
   #:data@
   #:pad
   #:data))

(in-package :program)

(defclass reference ()
  ((label :initarg :label
          :reader reference-label
          :type symbol)
   (offset :initarg :offset
           :initform -1
           :accessor reference-offset
           :type fixnum)
   (usage :initform ()
          :accessor reference-usage
          :type list)))

(defclass code-reference (reference)
  ())

(defvar *code-references*)

(defun code-reference (label)
  (or (find label *code-references*
            :key #'reference-label)
      (let ((r (make-instance 'code-reference :label label)))
        (push r *code-references*)
        r)))

(defclass data-reference (reference)
  ((binary :initarg :binary
           :initform 'ub8
           :reader reference-binary
           :type symbol)
   (size :initarg :size
         :initform -1
         :reader reference-size
         :type fixnum)))

(defvar *data-references*)

(defun data-reference (label)
  (or (find label *data-references*
            :key #'reference-label)
      (let ((r (make-instance 'data-reference :label label)))
        (push r *data-references*)
        r)))

(defvar *stream*)
(defvar *b*)

(defmacro program (name &body body)
  `(with-open-file (*stream* ,name
			     :direction :output
			     :element-type '(unsigned-byte 8)
			     :if-exists :supersede)
     (let (*code-references*
           *data-references*)
       (let ((*b* 0))
         ,@body)
       (write-references *code-references*)
       (write-references *data-references*))))

(defun b (&rest bytes)
  (dolist (b bytes)
    (incf *b*)
    (write-byte b *stream*)))

(defun b0 (n)
  (mod n 256))

(defun b1 (n)
  (mod (ash n -8) 256))

(defun code@= (label)
  (let ((r (code-reference label)))
    (unless (= -1 (reference-offset r))
      (error "duplicate @= ~A" label))
    (setf (reference-offset r) *b*)))

(defun code@ (label)
  (let* ((r (code-reference label))
         (offset (reference-offset r)))
    (unless (<= 0 offset)
      (push *b* (reference-usage r)))
    (b (b1 offset) (b0 offset))))

(defun data@= (label offset type &optional (size 1))
  (let ((r (data-reference label)))
    (if (= -1 (reference-offset r))
        (setf (reference-offset r) offset
              (reference-type r) type
              (reference-size r) size)
        (error "duplicate data ~A" label))
    ))

(defun b-reference (r nth)
  (let ((o+ (+ (reference-offset r)
               (* nth (binary-size (reference-binary r))))))
    (b (b1 o+) (b0 o+))))

(defun data@ (label &optional (nth 0))
  (let ((r (data-reference label)))
    (unless (< nth (reference-size r))
      (error "out of bounds"))
    (let ((o (reference-offset r)))
      (unless (<= 0 o)
        (push `(,*b* ,nth) (reference-usage r)))
      (b-reference r nth))))

(defun write-references (references)
  (dolist (r references)
    (let ((offset (reference-offset r)))
      (unless (<= 0 offset)
        (error "undefined reference ~A" (reference-label r)))
      (dolist (u (reference-usage r))
        (file-position *stream* u)
        (write-byte (b1 offset) *stream*)
        (write-byte (b0 offset) *stream*)))))

(defun data-size (defs)
  (let ((size 0))
    (dolist (d defs)
      (incf size (second d)))))

(defmacro finc (x &optional (inc 1))
  `(prog1 ,x (incf ,x ,inc)))

(defun data-value (label size value)
  (unless (<= (length value) size)
    (error "data value larger than size"))
  `((code@= ',label)
    ,@(mapcan (lambda (v)
                (etypecase v
                  ((unsigned-byte 8) `((w ,v ',label)))
                  ((unsigned-byte 16) `((w ,(b1 v) ',label)
                                        (w ,(b0 v) '(,label 1))))))
              value)))

(defun data-def (def)
  (destructuring-bind (label size &rest value) def
    `((data@ ',label *b* ,size)
      ,@(data-value label size value))))

(defun data-defs (defs)
  (mapcan #'data-def defs))

(defvar *data-init*)

(defun data-init (def)
  (destructuring-bind (label size &rest value) def
    (declare (ignore size))
    (mapcan (lambda (v)
              (etypecase v
                ((unsigned-byte 8) `((w ,v ',label)))
                ((unsigned-byte 16) `((w ,(b1 v) '(,label 0))
                                      (w ,(b0 v) '(,label 1))))))
            value)))

(defun data-inits (defs data)
  (let ((*data-init* `(0 . ,data)))
    (mapcan #'data-init defs)))

(defmacro data (&body defs)
  (let ((init (gensym "INIT-"))
        (data (gensym "DATA-")))
    `(progn
       (jump ',init)
       (code@= ',data)
       ,@(data-defs defs)
       (code@= ',init)
       ,@(data-inits defs data))))

(defvar *r*)

(defun r ()
  (incf *r*)
  (read-byte *stream* nil -1))

(defun rr ()
  (+ (* 256 (r)) (r)))

(defparameter *decode*
  '((-1 (return))
    (#x00 '(stop))
    (#x01 '(stop@))
    (#x12 `(jump ,(rr)))
    (#x20 '(test))
    (#x31 `(r ,(rr)))
    (#x3F '(copy))
    (#x40 `(w ,(r)))
    (#x43 '(w@))
    (#x44 `(w@ ,(rr)))
    (#x51 `(ti= ,(rr)))
    (#x73 '(lte))
    (#x91 '(increment))))

(defun commit (path)
  (with-open-file (*stream* path
                            :element-type '(unsigned-byte 8))
    (fresh-line)
    (let ((*r* 0)
          code)
      (loop
         (let* ((p *r*)
                (r (r))
                (i #.`(case r ,@*decode*))
                (*print-base* 16))
           (format t "~&~X ~S~%" p i)
           (setf code `(,@code ,i))))
      (force-output)
      code)))
