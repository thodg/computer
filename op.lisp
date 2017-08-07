
(in-package :common-lisp-user)

(defpackage :op
  (:use :common-lisp
        :gr)
  (:export #:op
           #:op-label
           #:op-lambda-list
           #:op-body
           #:define-op
           #:op-gr
           #:ops-gr))

(in-package :op)

(defclass op ()
  ((label :initarg :label
          :reader op-label
          :type symbol)
   (lambda-list :initarg :lambda-list
                :reader op-lambda-list
                :type list)
   (body :initarg :body
         :reader op-body
         :type list)))

(defgeneric op-gr (op))

(defvar *ops*
  ())

(defun define-op (label lambda-list body)
  (unless (not (find label *ops* :key #'op-label))
    (warn "Redefining op ~S" label)
    (setf *ops* (remove label *ops* :key #'op-label)))
  (let ((op (make-instance 'op :label label :lambda-list lambda-list :body body)))
    (push op *ops*)
    label))

(defmacro op (label lambda-list &body body)
  `(define-op ',label ',lambda-list ',body))

(defun op-code ()
  (dotimes (i 256)
    (unless (find i *grs* :key (lambda (gr)
                                 (first (gr-in-bound gr))))
      (return i))))

(defmethod op-gr ((op op))
  (let* ((op-code (op-code))
         (op-lambda-list (op-lambda-list op))
         (in `(,op-code ,@op-lambda-list))
         (out `(2 ,op-code ,@op-lambda-list)))
    (define-gr (op-label op) in out)))

(defun ops-gr (&optional (ops *ops*))
  (dolist (o ops)
    (op-gr o)))
