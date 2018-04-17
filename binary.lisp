
(in-package :common-lisp-user)

(defpackage :binary
  (:use :common-lisp)
  (:export #:binary-size
           #:binary-to-lisp
           #:lisp-to-binary
           #:binary?
           #:check-binary
           #:define-binary
           #:binary))

(in-package :binary)

(defclass binary ()
  ((label :initarg :label
          :reader binary-label
          :type symbol)
   (size :initarg :size
         :reader binary-size
         :type (unsigned-byte 8))))

(defmethod print-object ((o binary) stream)
  (prin1 `(binary ,(binary-label o) ,(binary-size o)) stream))

(defparameter *binaries*
  ())

(defun find-binary (label)
  (find label *binaries* :key #'binary-label))

(defun find-binary! (label)
  (or (find-binary label)
      (error "No such binary : ~A" label)))

(defmethod binary-size ((label symbol))
  (binary-size (find-binary! label)))

(defun register-binary (binary)
  (let* ((label (binary-label binary))
         (registered (find-binary label)))
    (unless (not registered)
      (warn "Redefining binary ~A" label)
      (setf *binaries* (remove registered *binaries*)))
    (push binary *binaries*)
    binary))

(defun define-binary (label size)
  (register-binary (make-instance 'binary
                                  :label label
                                  :size size)))

(defmacro binary (label size)
  `(define-binary ',label ,size))
