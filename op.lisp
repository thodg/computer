
(in-package :common-lisp-user)

(load "computer")
(load "gr")
(load "binary")
(load "program")

(defpackage :op
  (:use :common-lisp
        :computer
        :gr
        :binary
        :program)
  (:export #:op
           #:op-label
           #:op-lambda-list
           #:op-body
           #:define-op
           #:op-gr
           #:ops-gr
           #:write-ops-arch
           #:write-ops-include
           #:write-ops))

(in-package :op)

(defclass op ()
  ((label :initarg :label
          :reader op-label
          :type symbol)
   (lambda-list :initarg :lambda-list
                :reader op-lambda-list
                :type list)
   (lambda-list-ub8 :type list)
   (gr :type list)
   (body :initarg :body
         :reader op-body
         :type list)
   (arch-defun :type list)
   (include-defun :type list)))

(defgeneric op-lambda-list-ub8 (op))
(defgeneric op-gr (op))
(defgeneric op-arch-defun (op))
(defgeneric op-include-defun (op))

(defmethod print-object ((o op) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A ~A"
            (op-label o)
            (op-lambda-list o))))

(defmacro memoize-slot ((instance slot) &body body)
  (let ((ginstance (gensym "INSTANCE-"))
        (gslot (gensym "SLOT-")))
    `(let ((,ginstance ,instance)
           (,gslot ,slot))
       (if (slot-boundp ,ginstance ,gslot)
           (slot-value ,ginstance ,gslot)
           (setf (slot-value ,ginstance ,gslot)
                 (progn ,@body))))))

(defmacro do-lambda-terms ((var &optional binary) op &body body)
  (let ((term (gensym "TERM-")))
    `(dolist (,term (op-lambda-list ,op))
       (etypecase ,term
         (list (let ((,var (first ,term))
                     ,@(unless (null binary)
                         `((,binary (second ,term)))))
                 ,@body))
         (symbol (let ((,var ,term)
                       ,@(unless (null binary)
                           `((,binary 'ub8))))
                   ,@body))))))

(defmethod op-lambda-list-ub8 ((op op))
  (memoize-slot (op 'lambda-list-ub8)
    (let (args)
      (do-lambda-terms (term binary) op
        (dotimes (i (ceiling (binary-size binary) 8))
          (push (term-n term i) args)))
      (nreverse args))))

(defmethod op-gr ((op op))
  (memoize-slot (op 'gr)
    (let* ((op-code (op-code))
           (args (op-lambda-list-ub8 op))
           (in `(,op-code ,@args))
           (out `(2 ,op-code ,@args)))
      (define-gr (op-label op) in out))))

(defmethod op-arch-defun ((op op))
  (memoize-slot (op 'arch-defun)
    `(defun ,(op-label op) ,(op-lambda-list-ub8 op)
       (let (,@(op-arch-vars op))
         ,@(op-body op)))))

(defmethod op-include-defun ((op op))
  (memoize-slot (op 'include-defun)
    (let (args body)
      (do-lambda-terms (term type) op
        (push term args)
        (push (op-defun-b term type) body))
      (setf args (nreverse args)
            body (nreverse body))
      `(defun ,(op-label op) ,args
         ,body))))

(defvar *ops*
  ())

(defun define-op (label lambda-list body)
  (unless (not (find label *ops* :key #'op-label))
    (warn "Redefining op ~S" label)
    (setf *ops* (remove label *ops* :key #'op-label)))
  (let ((op (make-instance 'op
                           :label label
                           :lambda-list lambda-list
                           :body body)))
    (push op *ops*)
    op))

(defmacro op (label lambda-list &body body)
  `(define-op ',label ',lambda-list ',body))

(defun op-code ()
  (dotimes (i 256)
    (unless (find i *grs* :key (lambda (gr)
                                 (first (gr-in-bound gr))))
      (return i))))

(defun term-n (term n)
  (if (= 0 n)
      term
      (intern (format nil "~A~D" term n))))

(defun ops-gr (&optional (ops *ops*))
  (dolist (o ops)
    (op-gr o)))

(defgeneric ub8-to (binary bytes))

(defmethod ub8-to ((binary (eql 'ub8)) (bytes list))
  (assert (endp (rest bytes)))
  (first bytes))

(defun op-arch-vars (op)
  (let (vars)
    (do-lambda-terms (term binary) op
      (let (terms)
        (dotimes (i (ceiling (binary-size binary) 8))
          (push (term-n term i) terms))
        (push `(term ,(ub8-to binary (nreverse terms))) vars)))
    (nreverse vars)))


(defvar *stream*)

(defun output (&rest things)
  (dolist (x things)
    (typecase x
      (character (write-char x *stream*))
      (t (format *stream* "~S~%" x)))))

(defun output-op-arch (op)
  (let ((out (gr-out-bound (op-gr op))))
    (output (op-arch-defun op)
            #\Newline
            `(computer-op ,(second out)
                          ,(length (rest (rest out)))
                          ',(op-label op)))))

(defun ops-exports (ops)
  (let (exports)
    (dolist (op ops)
      (push (string-upcase (op-label op)) exports))
    (nreverse exports)))

(defun write-ops-arch-to-stream (ops stream
                                 &optional (package *package*))
  (let ((package-name (package-name package))
        (*stream* stream))
    (output `(in-package :common-lisp-user)
            #\Newline
            `(defpackage ,package-name
               (:use :computer)
               (:export ,@(ops-exports ops)))
            #\Newline
            `(in-package ,package-name))
    (dolist (op (reverse ops))
      (output #\Newline)
      (output-op-arch op))))

(defun write-ops-arch (&key name path (ops *ops*)
                         (package *package*) stream)
  (if stream
      (write-ops-arch-to-stream ops stream package)
      (progn
        (unless path
          (unless name
            (error "Missing name or path argument."))
          (setq path (format nil "arch/~A.lisp" name)))
        (with-open-file (s path :direction :output
                           :element-type 'character
                           :external-format :utf-8
                           :if-exists :supersede)
          (write-ops-arch-to-stream ops s package)))))

(defgeneric op-lambda-term-include (term binary))

(defmethod op-lambda-term-include ((term symbol) (binary (eql 'ub8)))
  `(b ,term))

(defmethod op-lambda-term-include ((term symbol) (binary (eql 'ub16)))
  `(b (b1 ,term) (b0 ,term)))

(defmethod op-lambda-term-include ((term symbol) (binary (eql 'code@)))
  `(etypecase ,term
     (symbol (code@ ,term))
     ((unsigned-byte 16) (b (b1 ,term) (b0 ,term)))))

(defmethod op-lambda-term-include ((term symbol) (binary (eql 'data@)))
  (let ((label (gensym "LABEL-"))
        (offset (gensym "OFFSET-")))
    `(etypecase ,term
       (symbol (data@ ,term))
       (list (apply #'data@ ,term))
       ((unsigned-byte 16) (b (b1 ,term) (b0 ,term))))))

(defun output-op-include (op)
  (output (op-include-defun op)))

(defun write-ops-include-to-stream (ops stream
                                    &optional (package *package*))
  (let ((package-name (package-name package))
        (*stream* stream))
    (output `(in-package :common-lisp-user)
            #\Newline
            `(defpackage ,package-name
               (:use :program)
               (:export ,@(ops-exports ops)))
            #\Newline
            `(in-package ,package-name))
    (dolist (op (reverse ops))
      (output #\Newline)
      (output-op-include op))))

(defun write-ops-include (&key name path (ops *ops*)
                            (package *package*) stream)
  (if stream
      (write-ops-include-to-stream ops stream package)
      (progn
        (unless path
          (unless name
            (error "Missing name or path argument."))
          (setq path (format nil "include/~A.lisp" name)))
        (with-open-file (s path
                           :direction :output
                           :element-type 'character
                           :external-format :utf-8
                           :if-exists :supersede)
          (write-ops-out-to-stream ops s package)))))

(defun write-ops (name)
  (write-ops-arch :name name)
  (write-ops-include :name name))
