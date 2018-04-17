(IN-PACKAGE :COMMON-LISP-USER)

(DEFPACKAGE "SIMPLE-OPS"
  (:USE :PROGRAM)
  (:EXPORT "[INCREMENT]"
           "[DECREMENT]"
           "[>>]]"
           "[<<]]"
           "[XOR]]"
           "[OR]]"
           "[AND]]"
           "[NOT]"
           "[=]]"
           "[=]]]]"
           "[LTE]]"
           "[LTE]]]]"
           "[LT]]"
           "[LT]]]]"
           "TE"
           "TE]]"
           "TI"
           "TI]]"
           "[INC-W]"
           "[INC-W]]"
           "[DEC-W]"
           "[DEC-W]]"
           "W@]"
           "W]]]"
           "W"
           "W]]"
           "["
           "[COPY"
           "[INC-R"
           "[INC-R]"
           "[DEC-R"
           "[DEC-R]"
           "[R"
           "[R]]"
           "TEST]"
           "INC-TP"
           "INC-TP]"
           "DEC-TP"
           "DEC-TP]"
           "[TP"
           "JUMP"
           "JUMP]]"
           "STOP]]"
           "STOP"))

(IN-PACKAGE "SIMPLE-OPS")

(DEFUN STOP () (B 33))

(DEFUN STOP]] () (B 32))

(DEFUN JUMP]] () (B 31))

(DEFUN JUMP (P1 P0) (B 30 P1 P0))

(DEFUN [TP () (B 29))

(DEFUN DEC-TP] () (B 28))

(DEFUN DEC-TP (P) (B 27 P))

(DEFUN INC-TP] () (B 26))

(DEFUN INC-TP (P) (B 25 P))

(DEFUN TEST] () (B 24))

(DEFUN [R]] () (B 23))

(DEFUN [R (P1 P0) (B 22 P1 P0))

(DEFUN [DEC-R] () (B 21))

(DEFUN [DEC-R (P) (B 20 P))

(DEFUN [INC-R] () (B 19))

(DEFUN [INC-R (P) (B 18 P))

(DEFUN [COPY () (B 17))

(DEFUN [ (B) (B 16 B))

(DEFUN W]] (B) (B 15 B))

(DEFUN W (B E1 E0) (B 14 B E1 E0))

(DEFUN W]]] () (B 13))

(DEFUN W@] (E1 E0) (B 12 E1 E0))

(DEFUN [DEC-W]] () (B 11))

(DEFUN [DEC-W] (P) (B 10 P))

(DEFUN [INC-W]] () (B 9))

(DEFUN [INC-W] (P) (B 8 P))

(DEFUN TI]] () (B 7))

(DEFUN TI (I1 I0) (B 6 I1 I0))

(DEFUN TE]] () (B 5))

(DEFUN TE (E1 E0) (B 4 E1 E0))

(DEFUN [LT]]]] () (B 3))

(DEFUN [LT]] () (B 2))

(DEFUN [LTE]]]] () (B 1))

(DEFUN [LTE]] () (B 0))

(DEFUN [=]]]] () (B 44))

(DEFUN [=]] () (B 43))

(DEFUN [NOT] () (B 41))

(DEFUN [AND]] () (B 42))

(DEFUN [OR]] () (B 40))

(DEFUN [XOR]] () (B 39))

(DEFUN [<<]] () (B 38))

(DEFUN [>>]] () (B 37))

(DEFUN [DECREMENT] () (B 36))

(DEFUN [INCREMENT] () (B 35))
