
(in-package :common-lisp)

(defpackage :g
  (:use :common-lisp)
  (:export #:g))

(in-package :g)

(defvar *s*)

(defun wb (b)
  (write-byte b *s*))

(defun gr (i o)
  (let (v
	b)
    (dolist (ie i)
      (if (symbolp ie)
	  (push ie v)
	  (pushnew ie b)))
    (dolist (oe o)
      (if (symbolp oe)
	  (or (find oe v)
	      (error "unbound variable in rule output"))
	  (pushnew oe b)))
    (let (vn
	  (vl (length v))
	  (il (length i))
	  (ol (length o)))
      (dolist (ve v)
	(push (cons ve (or (dotimes (n 256)
			     (unless (or (find n vn :key 'cdr)
					 (find n b))
			       (return n)))
			   (error "what ? how ?")))
	      vn))
      (wb (+ 1 1 vl 1 il 1 ol))
      (wb vl)
      (dolist (vne vn)
	(wb (cdr vne)))
      (wb il)
      (dolist (ie i)
	(etypecase ie
	  (symbol (wb (cdr (assoc ie vn))))
	  ((unsigned-byte 8) (wb ie))))
      (wb ol)
      (dolist (oe o)
	(etypecase oe
	  (symbol (wb (cdr (assoc oe vn))))
	  ((unsigned-byte 8) (wb oe)))))))

(defmacro g (&body body)
  (let (r)
    (loop
       (when (endp body)
	 (return))
       (let* ((i (pop body))
	      (o (pop body)))
	 (push `(gr ',i ',o) r)))
    (setf r (nreverse r))
    `(with-open-file (*s* ".g"
			  :direction :output
			  :element-type '(unsigned-byte 8)
			  :if-exists :supersede)
       ,@r
       (force-output *s*))))

(g
 (#x00)         (2 #x00)         ; stop
 (#x01)         (2 #x01)         ; stop @
 (#x10)         (2 #x10)         ; 16 bits stack jump
 (#x12 p1 p0)   (2 #x12 p1 p0)   ; 16 bits explicit jump
 (#x13)         (2 #x13)         ; read tp
 (#x14)         (2 #x14)         ; 8 bits decrement stack jump
 (#x15 p)       (2 #x15 p)       ; 8 bits decrement jump
 (#x16)         (2 #x16)         ; 8 bits increment stack jump
 (#x17 p)       (2 #x17 p)       ; 8 bits increment jump
 (#x20)         (2 #x20)         ; conditional
 (#x30)         (2 #x30)         ; 16 bits stack read
 (#x31 p1 p0)   (2 #x31 p1 p0)   ; 16 bits explicit read
 (#x3F)         (2 #x3F)         ; copy
 (#x40 b)       (2 #x40 b)       ; write
 (#x41 b)       (2 #x41 b)       ; 16 bits stack write
 (#x42 b p1 p0) (2 #x42 b p1 p0) ; 16 bits explicit write
 (#x43)         (2 #x43)         ; 16 bits stack write
 (#x44 p1 p0)   (2 #x44 p1 p0)   ; 16 bits explicit write
 (#x50)         (2 #x50)         ; stack set ti
 (#x51 i1 i0)   (2 #x51 i1 i0)   ; set ti
 (#x60)         (2 #x60)         ; stack set te
 (#x61 e1 e0)   (2 #x61 e1 e0)   ; set te
 (#x70)         (2 #x70)         ; 16 bits lt
 (#x71)         (2 #x71)         ; 8 bits lt
 (#x72)         (2 #x72)         ; 16 bits lte
 (#x73)         (2 #x73)         ; 8 bits lte
 (#x74)         (2 #x74)         ; 16 bits eq
 (#x75)         (2 #x75)         ; 8 bits eq
 (#x80)         (2 #x80)         ; not
 (#x81)         (2 #x81)         ; and
 (#x82)         (2 #x82)         ; or
 (#x83)         (2 #x83)         ; xor
 (#x84)         (2 #x84)         ; left shift
 (#x85)         (2 #x85)         ; right shift
 (#x90)         (2 #x90)         ; decrement
 (#x91)         (2 #x91)         ; increment

 (#xAA)
 (1 #x7F #x45 #x4C #x46 ;   0-3. -ELF
    1                   ;     4. 32 bits
    1                   ;     5. 2's complement, little endian
    1                   ;     6. File version
    0                   ;     7. UNIX System V ABI
    0                   ;     8. ABI version
    0 0 0 0 0 0 0 0     ;  9-15. Padding
    0 2                 ; 16-17. Executable file
    0 3                 ; 18-19. 386
    0 0 0 1             ; 20-23. Current version
    0 0 0 52            ; 24-27. entry point
    0 0 0 0             ; 28-31. program header
    0 0 0 0             ; 32-35. section header
    0 0 0 0             ; 36-39. processor flags
    0 52                ; 40-41. elf header size
    0 0                 ; 42-43. program header size
    0 0                 ; 44-45. program header count
    0 0                 ; 46-47. section header size
    0 0                 ; 48-49. section header count
    0 0                 ; 50-51. wat?
    )

  (x)
  (1 x))
