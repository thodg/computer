
(in-package :common-lisp-user)

(load "computer")
(load "gr")
(load "program")

(defpackage :op
  (:use :common-lisp
        :computer
        :gr
        :program)
  (:export #:op
           #:op-label
           #:op-lambda-list
           #:op-body
           #:define-op
           #:op-gr
           #:ops-gr
           #:write-ops
           #:write-ops-out))

(in-package :op)

(defclass op ()
  ((label :initarg :label
          :reader op-label
          :type symbol)
   (lambda-list :initarg :lambda-list
                :reader op-lambda-list
                :type list)
   (gr :type list)
   (body :initarg :body
         :reader op-body
         :type list)
   (defun :type list)
   (out-defun :type list)))

(defgeneric op-gr (op))
(defgeneric op-defun (op))
(defgeneric op-out-defun (op))

(defvar *ops*
  ())

(defun define-op (label lambda-list body)
  (unless (not (find label *ops* :key #'op-label))
    (warn "Redefining op ~S" label)
    (setf *ops* (remove label *ops* :key #'op-label)))
  (let ((op (make-instance 'op :label label :lambda-list lambda-list :body body)))
    (push op *ops*)
    op))

(defmacro op (label lambda-list &body body)
  `(define-op ',label ',lambda-list ',body))

(defun op-code ()
  (dotimes (i 256)
    (unless (find i *grs* :key (lambda (gr)
                                 (first (gr-in-bound gr))))
      (return i))))

(defmethod op-gr ((op op))
  (if (slot-boundp op 'gr)
      (slot-value op 'gr)
      (setf (slot-value op 'gr)
            (let* ((op-code (op-code))
                   (op-lambda-list (op-lambda-list op))
                   (in `(,op-code ,@op-lambda-list))
                   (out `(2 ,op-code ,@op-lambda-list)))
              (define-gr (op-label op) in out)))))

(defun ops-gr (&optional (ops *ops*))
  (dolist (o ops)
    (op-gr o)))

(defmethod op-defun ((op op))
  (if (slot-boundp op 'defun)
      (slot-value op 'defun)
      (setf (slot-value op 'defun)
            `(defun ,(op-label op) ,(op-lambda-list op)
               ,@(op-body op)))))

(defvar *stream*)

(defun output (&rest things)
  (dolist (x things)
    (typecase x
      (character (write-char x *stream*))
      (t (format *stream* "~S~%" x)))))

(defun write-op (op)
  (let ((out (gr-out-bound (op-gr op))))
    (output (op-defun op)
            #\Newline
            `(computer-op ,(second out)
                          ,(length (rest (rest out)))
                          ',(op-label op)))))

(defun ops-exports (ops)
  (let (exports)
    (dolist (op ops)
      (push (string-upcase (op-label op)) exports))
    (nreverse exports)))

(defun write-ops-to-stream (ops stream &optional (package *package*))
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
      (write-op op))))

(defun write-ops (&key (path "computer-ops.lisp") (ops *ops*)
                    (package *package*) stream)
  (if stream
      (write-ops-to-stream ops stream package)
      (with-open-file (s path :direction :output
                         :element-type 'character
                         :external-format :utf-8
                         :if-exists :supersede)
        (write-ops-to-stream ops s package))))

(defmethod op-out-defun ((op op))
  (if (slot-boundp op 'out-defun)
      (slot-value op 'out-defun)
      (setf (slot-value op 'out-defun)
            `(defun ,(op-label op) ,(op-lambda-list op)
               (b ,@(gr-in (op-gr op)))))))

(defun write-op-out (op)
  (output (op-out-defun op)))

(defun write-ops-out-to-stream (ops stream &optional (package *package*))
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
      (write-op-out op))))

(defun write-ops-out (&key (path "program-ops.lisp") (ops *ops*)
                        (package *package*) stream)
  (if stream
      (write-ops-out-to-stream ops stream package)
      (with-open-file (s path :direction :output
                         :element-type 'character
                         :external-format :utf-8
                         :if-exists :supersede)
        (write-ops-out-to-stream ops s package))))
