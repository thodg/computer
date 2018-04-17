
(in-package :common-lisp-user)

(load "program")
(load "include/simple-ops")

(defpackage :com
  (:use :common-lisp
        :program
        :simple-ops))

(in-package :com)

(program "com"
  (data (result 10)
        (counter 1 0))
  (ti :loop)
  (code@= :loop)
  (lte 10 'counter)
  (test)
  (jump :exit)
  (r 'counter)
   (copy)
    (w 0)
     (r 'counter)
      (w@)
   (increment)
   (w@ 'counter)
  (jump :loop)

  (code@= :exit)
  (w #x00)
  (r 'counter)
   (stop@))
