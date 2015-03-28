;(import (miruKanren))
(import (scheme base)
        (test-check)

        (miruKanren mk-watch))

(include "prelude.scm")

;; This file is a minikanren version of
;; https://www.student.cs.uwaterloo.ca/~isg/res/mips/opcodes

(define (register r-name r-binary) (binaryo 5 r-name r-binary))
(define (shift a-numeral a-binary) (binaryo 5 a-numeral a-binary))
(define (immediate/16 i-number i-binary) (binaryo 16 i-number i-binary))
(define (immediate/26 i-number i-binary) (binaryo 26 i-number i-binary))

(define (instruction inst word)
  ;; Each MIPS instruction is encoded in exactly one word (32 bits).
  ;; There are three encoding formats.
  (conde
   ((register-encoding inst word))
   ((immediate-encoding inst word))
   ((jump-encoding inst word))))

(define (register-encoding inst word)
  ;; This encoding is used for instructions which do not require any immediate data.
  ;; These instructions receive all their operands in registers.
  ;; Additionally, certain of the bit shift instructions use this encoding; their operands are two registers and a 5-bit shift amount.
  ;; ooooooss sssttttt dddddaaa aaffffff
  (fresh (s o $d $s $t a i label)
    (fresh (o1 o2 o3 o4 o5 o6
               s1 s2 s3 s4 s5
               t1 t2 t3 t4 t5
               d1 d2 d3 d4 d5
               a1 a2 a3 a4 a5
               f1 f2 f3 f4 f5 f6)
      (== word `((,o1 ,o2 ,o3 ,o4 ,o5 ,o6 ,s1 ,s2)
                 (,s3 ,s4 ,s5 ,t1 ,t2 ,t3 ,t4 ,t5)
                 (,d1 ,d2 ,d3 ,d4 ,d5 ,a1 ,a2 ,a3)
                 (,a4 ,a5 ,f1 ,f2 ,f3 ,f4 ,f5 ,f6)))
      ;;(binaryo 32 inst word)
      (instruction-syntax 'register s inst
                          o $d $s $t a i label)
      (== '(0 0 0 0 0 0) `(,o1 ,o2 ,o3 ,o4 ,o5 ,o6))
      (register $s `(,s1 ,s2 ,s3 ,s4 ,s5))
      (register $t `(,t1 ,t2 ,t3 ,t4 ,t5))
      (register $d `(,d1 ,d2 ,d3 ,d4 ,d5))
      (shift a `(,a1 ,a2 ,a3 ,a4 ,a5))
      (instruction/opcode-function/syntax o `(,f1 ,f2 ,f3 ,f4 ,f5 ,f6) s))))

(define (immediate-encoding inst word)
  ;; This encoding is used for instructions which require a 16-bit immediate operand.
  ;; These instructions typically receive one operand in a register, another as an immediate value coded into the instruction itself, and place their results in a register.
  ;; This encoding is also used for load, store, branch, and other instructions so the use of the fields is different in some cases.
  ;; ooooooss sssttttt iiiiiiii iiiiiiii
  (fresh (s o $d $s $t a i label)
    (fresh (o1 o2 o3 o4 o5 o6
               s1 s2 s3 s4 s5
               t1 t2 t3 t4 t5
               i1 i2 i3 i4 i5 i6 i7 i8
               i9 i10 i11 i12 i13 i14 i15 i16)
      (== word `((,o1  ,o2  ,o3  ,o4  ,o5  ,o6  ,s1  ,s2)
                 (,s3  ,s4  ,s5  ,t1  ,t2  ,t3  ,t4  ,t5)
                 (,i1  ,i2  ,i3  ,i4  ,i5  ,i6  ,i7  ,i8)
                 (,i9  ,i10 ,i11 ,i12 ,i13 ,i14 ,i15 ,i16)))
      ;;(binaryo 32 inst word)
      (instruction-syntax 'immediate s inst
                          o $d $s $t a i label)
      (instruction/opcode-function/syntax o `(,o1 ,o2 ,o3 ,o4 ,o5 ,o6) s)
      (register $s `(,s1 ,s2 ,s3 ,s4 ,s5))
      (register $t `(,t1 ,t2 ,t3 ,t4 ,t5))
      (immediate/16 i `(,i1 ,i2 ,i3 ,i4 ,i5 ,i6 ,i7 ,i8
                            ,i9 ,i10 ,i11 ,i12 ,i13 ,i14 ,i15 ,i16)))))

(define (jump-encoding inst word)
  ;; This encoding is used for jump instructions, which require a 26-bit immediate offset.
  ;; It is also used for the trap instruction.
  ;; ooooooii iiiiiiii iiiiiiii iiiiiiii
  (fresh (s o $d $s $t a i label)
    (fresh (o1 o2 o3 o4 o5 o6
               i1 i2 i3 i4 i5 i6 i7 i8
               i9 i10 i11 i12 i13 i14 i15 i16
               i17 i18 i19 i20 i21 i22 i23 i24
               i25 i26)
      (== word `((,o1  ,o2  ,o3  ,o4  ,o5  ,o6  ,i1  ,i2)
                 (,i3  ,i4  ,i5  ,i6  ,i7  ,i8  ,i9  ,i10)
                 (,i11 ,i12 ,i13 ,i14 ,i15 ,i16 ,i17 ,i18)
                 (,i19 ,i20 ,i21 ,i22 ,i23 ,i24 ,i25 ,i26)))
      ;;(binaryo 32 inst word)
      (instruction-syntax 'jump s inst
                          o $d $s $t a i label)
      (instruction/opcode-function/syntax o `(,o1 ,o2 ,o3 ,o4 ,o5 ,o6) s)
      (immediate/26 i `(,i1 ,i2 ,i3 ,i4 ,i5 ,i6 ,i7 ,i8
                            ,i9 ,i10 ,i11 ,i12 ,i13 ,i14 ,i15 ,i16
                            ,i17 ,i18 ,i19 ,i20 ,i21 ,i22 ,i23 ,i24
                            ,i25 ,i26)))))
  
(define (instruction-syntax e s t
                            o $d $s $t a i label)
  ;; This is a table of all the different types of instruction as they appear in the assembly listing.
  ;; Note that each syntax is associated with exactly one encoding which is used to encode all instructions which use that syntax.
  (conde
   ((== e 'register)
    (conde
     ((== s 'arith-log)   (== t `(,o ,$d ,$s ,$t)))
     ((== s 'div-mult)    (== t `(,o ,$s ,$t)))
     ((== s 'shift)       (== t `(,o ,$d ,$t ,a)))
     ((== s 'shift-v)     (== t `(,o ,$d ,$t ,$s)))
     ((== s 'jump-r)      (== t `(,o ,$s)))
     ((== s 'move-from)   (== t `(,o ,$d)))
     ((== s 'move-to)     (== t `(,o ,$s)))))
   ((== e 'immediate)
    (conde
     ((== s 'arith-log-i) (== t `(,o ,$t ,$s ,i)))
     ((== s 'load-i)      (== t `(,o ,$t ,i)))
     ((== s 'branch)      (== t `(,o ,$s ,$t ,label)))
     ((== s 'branch-z)    (== t `(,o ,$s ,label)))
     ((== s 'load-store)  (== t `(,o ,$t ,i (,$s))))))
   ((== e 'jump)
    (conde
     ((== s 'jump)        (== t `(,o ,label)))
     ((== s 'trap)        (== t `(,o ,i)))))))

(define (instruction/opcode-function/syntax o f s)
  ;; These tables list all of the available operations `o` in MIPS.
  ;; For each instruction, the 6-bit opcode or function `f` is shown.
  ;; The syntax column `s` indicates which syntax is used to write the instruction in assembly text files. Note that which syntax is used for an instruction also determines which encoding is to be used.
  (conde
   ((== o 'add)   (== f '(1 0 0 0 0 0)) (== s 'arith-log))
   ((== o 'addu)  (== f '(1 0 0 0 0 1)) (== s 'arith-log))
   ((== o 'addi)  (== f '(0 0 1 0 0 0)) (== s 'arith-log-i))
   ((== o 'addiu) (== f '(0 0 1 0 0 1)) (== s 'arith-log-i))
   ((== o 'and)   (== f '(1 0 0 1 0 0)) (== s 'arith-log-i))
   ((== o 'andi)  (== f '(0 0 1 1 0 0)) (== s 'arith-log-i))
   ((== o 'div)   (== f '(0 1 1 0 1 0)) (== s 'div-mult))
   ((== o 'divu)  (== f '(0 1 1 0 1 1)) (== s 'div-mult))
   ((== o 'mult)  (== f '(0 1 1 0 0 0)) (== s 'div-mult))
   ((== o 'multu) (== f '(0 1 1 0 0 1)) (== s 'div-mult))
   ((== o 'nor)   (== f '(1 0 0 1 1 1)) (== s 'arith-log))
   ((== o 'or)    (== f '(1 0 0 1 0 1)) (== s 'arith-log))
   ((== o 'ori)   (== f '(0 0 1 1 0 1)) (== s 'arith-log-i))
   ((== o 'sll)   (== f '(0 0 0 0 0 0)) (== s 'shift))
   ((== o 'sllv)  (== f '(0 0 0 1 0 0)) (== s 'shift-v))
   ((== o 'sra)   (== f '(0 0 0 0 1 1)) (== s 'shift))
   ((== o 'srav)  (== f '(0 0 0 1 1 1)) (== s 'shift-v))
   ((== o 'srl)   (== f '(0 0 0 0 1 0)) (== s 'shift))
   ((== o 'srlv)  (== f '(0 0 0 1 1 0)) (== s 'shift-v))
   ((== o 'sub)   (== f '(1 0 0 0 1 0)) (== s 'arith-log))
   ((== o 'subu)  (== f '(1 0 0 0 1 1)) (== s 'arith-log))
   ((== o 'xor)   (== f '(1 0 0 1 1 0)) (== s 'arith-log))
   ((== o 'xori)  (== f '(0 0 1 1 1 0)) (== s 'arith-log-i))
   ((== o 'lhi)   (== f '(0 1 1 0 0 1)) (== s 'load-i))
   ((== o 'llo)   (== f '(0 1 1 0 0 0)) (== s 'load-i))
   ((== o 'slt)   (== f '(1 0 1 0 1 0)) (== s 'arith-log))
   ((== o 'sltu)  (== f '(1 0 1 0 0 1)) (== s 'arith-log))
   ((== o 'slti)  (== f '(0 0 1 0 1 0)) (== s 'arith-log-i))
   ((== o 'sltiu) (== f '(0 0 1 0 0 1)) (== s 'arith-log-i))
   ((== o 'beq)   (== f '(0 0 0 1 0 0)) (== s 'branch))
   ((== o 'bgtz)  (== f '(0 0 0 1 1 1)) (== s 'branch-z))
   ((== o 'blez)  (== f '(0 0 0 1 1 0)) (== s 'branch-z))
   ((== o 'bne)   (== f '(0 0 0 1 0 1)) (== s 'branch))
   ((== o 'j)     (== f '(0 0 0 0 1 0)) (== s 'jump))
   ((== o 'jal)   (== f '(0 0 0 0 1 1)) (== s 'jump))
   ((== o 'jalr)  (== f '(0 0 1 0 0 1)) (== s 'jump-r))
   ((== o 'jr)    (== f '(0 0 1 0 0 0)) (== s 'jump-r))
   ((== o 'lb)    (== f '(1 0 0 0 0 0)) (== s 'load-store))
   ((== o 'lbu)   (== f '(1 0 0 1 0 0)) (== s 'load-store))
   ((== o 'lh)    (== f '(1 0 0 0 0 1)) (== s 'load-store))
   ((== o 'lhu)   (== f '(1 0 0 1 0 1)) (== s 'load-store))
   ((== o 'lw)    (== f '(1 0 0 0 1 1)) (== s 'load-store))
   ((== o 'sb)    (== f '(1 0 1 0 0 0)) (== s 'load-store))
   ((== o 'sh)    (== f '(1 0 1 0 0 1)) (== s 'load-store))
   ((== o 'sw)    (== f '(1 0 1 0 1 1)) (== s 'load-store))
   ((== o 'mfhi)  (== f '(0 1 0 0 0 0)) (== s 'move-from))
   ((== o 'mflo)  (== f '(0 1 0 0 1 0)) (== s 'move-from))
   ((== o 'mthi)  (== f '(0 1 0 0 0 1)) (== s 'move-to))
   ((== o 'mtlo)  (== f '(0 1 0 0 1 1)) (== s 'move-to))
   ((== o 'trap)  (== f '(0 1 1 0 1 0)) (== s 'trap))))





;; $ rlwrap larceny -r7rs -path .
;; Larceny v0.98 "General Ripper" (Mar 12 2015 23:11:16, precise:Linux:unified)
;; larceny.heap, built on Thu Mar 12 23:12:39 GMT 2015

;; > (include "minimips/mips.scm")
;; Reading minimips/mips.scm
;; Reading sorted-int-set.scm
;; Reading miruKanren.scm
;; instruction/opcode-function/syntax

;; > (runi (lambda (q) (fresh (x y) (== q (list x y)) (instruction x y))))

;; (((add _.0 _.1 _.2) ((0 0 0 0 0 0 _.3 _.4) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12) (_.13 _.14 _.15 _.16 _.17 _.18 _.19 _.20) (_.21 _.22 1 0 0 0 0 0))) (and unknown unknown unknown unknown unknown unknown unknown unknown))
;; another (y/n)?
;; y

;; (((addu _.0 _.1 _.2) ((0 0 0 0 0 0 _.3 _.4) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12) (_.13 _.14 _.15 _.16 _.17 _.18 _.19 _.20) (_.21 _.22 1 0 0 0 0 1))) (and unknown unknown unknown unknown unknown unknown unknown unknown))
;; another (y/n)?
;; y

;; (((nor _.0 _.1 _.2) ((0 0 0 0 0 0 _.3 _.4) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12) (_.13 _.14 _.15 _.16 _.17 _.18 _.19 _.20) (_.21 _.22 1 0 0 1 1 1))) (and unknown unknown unknown unknown unknown unknown unknown unknown))
;; another (y/n)?
;; y

;; (((or _.0 _.1 _.2) ((0 0 0 0 0 0 _.3 _.4) (_.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12) (_.13 _.14 _.15 _.16 _.17 _.18 _.19 _.20) (_.21 _.22 1 0 0 1 0 1))) (and unknown unknown unknown unknown unknown unknown unknown unknown))
;; another (y/n)?
;; y



;; > (runi (lambda (q) (fresh (x y) (instruction '(or 3 5 7) q))))

;; (((0 0 0 0 0 0 0 0) (1 0 1 0 0 1 1 1) (0 0 0 1 1 _.0 _.1 _.2) (_.3 _.4 1 0 0 1 0 1)) (and unknown unknown))
;; another (y/n)?
;; y

;; thats-all!












(define (inst/binary inst bin)
  ;; 23bdfff8
  (define (eighto w)
    (fresh (a b c d e f g h)
      (== w `(,a ,b ,c ,d ,e ,f ,g ,h))))
  (fresh (word a b c d cd bcd abcd)
    (== word `(,a ,b ,c ,d))
    (eighto a)
    (eighto b)
    (eighto c)
    (eighto d)
    (appendo c d cd)
    (appendo b cd bcd)
    (appendo a bcd abcd)
    (binaryo 32 bin abcd)
    (instruction inst word)
    ))

(define (assemble/disassemble program binary)
  (mapo inst/binary program binary))

(define ackermann '(#x23bdfff8 #xafb00004 #xafbf0000 #x14800002 #x20a20001 #x08100012 #x14a00004 #x2084ffff #x20050001 #x0c100000 #x08100012 #x00808020 #x20a5ffff #x0c100000 #x2204ffff #x00402820 #x0c100000 #x08100012 #x8fb00004 #x8fbf0000 #x23bd0008 #x03e00008))

;; > (run* (lambda (q) (assemble/disassemble q ackermann)))
;; ((((addi 29 29 65528)
;;    (sw 16 4 (29))
;;    (sw 31 0 (29))
;;    (bne 4 0 _.0)
;;    (addi 2 5 1)
;;    (j _.1)
;;    (bne 5 0 _.2)
;;    (addi 4 4 65535)
;;    (addi 5 0 1)
;;    (jal _.3)
;;    (j _.4)
;;    (add 16 4 0)
;;    (addi 5 5 65535)
;;    (jal _.5)
;;    (addi 4 16 65535)
;;    (add 5 2 0)
;;    (jal _.6)
;;    (j _.7)
;;    (lw 16 4 (29))
;;    (lw 31 0 (29))
;;    (addi 29 29 8)
;;    (jr 31))
;;   where))
