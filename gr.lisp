
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
           #:*grs*
           #:define-gr))

(in-package :gr)

(defclass gr ()
  ((label :initarg :label
          :reader gr-label
          :type symbol)
   (in :initarg :in
       :reader gr-in
       :type list)
   (out :initarg :out
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
            (sublis (gr-bindings gr) (gr-in gr)))))

(defmethod gr-out-bound ((gr gr))
  (if (slot-boundp gr 'out-bound)
      (slot-value gr 'out-bound)
      (setf (slot-value gr 'out-bound)
            (sublis (gr-bindings gr) (gr-out gr)))))

(defvar *grs*
  ())

(defun define-gr (label in out)
  (let ((n (make-instance 'gr :label label :in in :out out)))
    (unless (not (find label *grs* :key #'gr-label))
      (warn "Redefining GR ~S" label)
      (setf *grs* (remove label *grs* :key #'gr-label)))
    (dolist (r *grs*)
      (unless (not (equalp (gr-in-bound n) (gr-in-bound r)))
        (error "Duplicate input for rules ~S and ~S"
               (gr-label r) (gr-label n)))
      (unless (not (equalp (gr-out-bound n) (gr-out-bound r)))
        (warn "Duplicate output for rules ~S and ~S"
              (gr-label r) (gr-label n))))
    (push n *grs*)))

(defmacro gr (label in &body out)
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

(defun write-g (&key (path ".g") (grs *grs*) stream)
  (if stream
      #1=(dolist (gr grs)
           (write-gr gr stream))
      (with-open-file (stream path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        #1#)))
