;;;============================================================================

;;; File: "_loongarch64#.scm"

;;; Copyright (c) 2019 by Abdelhakim Qbaich, All Rights Reserved.

;;;============================================================================

(namespace ("_loongarch64#"

loongarch64-implement

loongarch64-registers-implement
loongarch64-register-name
loongarch64-reg?
loongarch64-reg-field

loongarch64-r0
loongarch64-r1
loongarch64-r2
loongarch64-r3
loongarch64-r4
loongarch64-r5
loongarch64-r6
loongarch64-r7
loongarch64-r8
loongarch64-r9
loongarch64-r10
loongarch64-r11
loongarch64-r12
loongarch64-r13
loongarch64-r14
loongarch64-r15
loongarch64-r16
loongarch64-r17
loongarch64-r18
loongarch64-r19
loongarch64-r20
loongarch64-r21    ;;tp
loongarch64-r22    ;;fp
loongarch64-r23    ;;s0
loongarch64-r24    ;;s1
loongarch64-r25    ;;s2
loongarch64-r26
loongarch64-r27
loongarch64-r28
loongarch64-r29
loongarch64-r30
loongarch64-r31
loongarch64-pc

loongarch64-zero  ;;r0
loongarch64-ra    ;;r1
loongarch64-gp    ;;r2
loongarch64-sp    ;;r3
loongarch64-a0    ;;r4
loongarch64-v0    ;;r4
loongarch64-a1    ;;r5
loongarch64-v1    ;;r5
loongarch64-a2    ;;r6
loongarch64-a3    ;;r7
loongarch64-a4
loongarch64-a5
loongarch64-a6
loongarch64-a7    ;;r11
loongarch64-t0    ;;r12
loongarch64-t1    ;;r13
loongarch64-t2
loongarch64-t3
loongarch64-t4
loongarch64-t5
loongarch64-t6    ;;r18
loongarch64-t7    ;;r19
loongarch64-t8    ;;r20
loongarch64-tp    ;;r21
loongarch64-fp    ;;r22
loongarch64-s0    ;;r23
loongarch64-s1    ;;r24
loongarch64-s2    ;;r25
loongarch64-s3    ;;r26
loongarch64-s4    ;;r27
loongarch64-s5    ;;r28
loongarch64-s6    ;;r29
loongarch64-s7    ;;r30
loongarch64-s8    ;;r31

loongarch64-arch-set!
loongarch64-64bit-mode?
loongarch64-word-width

loongarch64-imm?
loongarch64-imm-int
loongarch64-imm-int?
loongarch64-imm-int-type
loongarch64-imm-int-value
loongarch64-imm-lbl
loongarch64-imm-lbl?
loongarch64-imm-lbl-offset
loongarch64-imm-lbl-label
loongarch64-imm->instr

loongarch64-listing

loongarch64-label
loongarch64-db
loongarch64-d2b
loongarch64-dh
loongarch64-ds
loongarch64-d4b
loongarch64-dw
loongarch64-dl
loongarch64-d8b
loongarch64-dd
loongarch64-dq

;; Macro command
loongarch64-nop
loongarch64-liw
loongarch64-lid
loongarch64-move
loongarch64-la
loongarch64-bgt
loongarch64-bgtu
loongarch64-ble
loongarch64-bleu
loongarch64-bltz
loongarch64-bgtz
loongarch64-blez
loongarch64-bgez
loongarch64-jr

;; presudo
loongarch64-not
loongarch64-sltz

loongarch64-ori
loongarch64-xori
loongarch64-slti
loongarch64-sltui
loongarch64-addiw
loongarch64-addid
loongarch64-lu52id
loongarch64-andi
loongarch64-addw
loongarch64-addd
loongarch64-subw
loongarch64-subd
loongarch64-slt
loongarch64-sltu
loongarch64-nor
loongarch64-and
loongarch64-or
loongarch64-xor
loongarch64-orn
loongarch64-andn


loongarch64-sllw
loongarch64-srlw
loongarch64-sraw
loongarch64-slld
loongarch64-srld
loongarch64-srad

loongarch64-slliw
loongarch64-sllid
loongarch64-srliw
loongarch64-srlid
loongarch64-sraiw
loongarch64-sraid

loongarch64-ldb
loongarch64-ldh
loongarch64-ldw
loongarch64-ldd
loongarch64-stb   ;;sb
loongarch64-sth   ;;sh
loongarch64-stw   ;;sw
loongarch64-std   ;;sd
loongarch64-ldbu
loongarch64-ldhu
loongarch64-ldwu

loongarch64-ldxb
loongarch64-ldxh
loongarch64-ldxw
loongarch64-ldxd
loongarch64-stxb
loongarch64-stxh
loongarch64-stxw
loongarch64-stxd
loongarch64-ldxbu
loongarch64-ldxhu
loongarch64-ldxwu

loongarch64-dbar
loongarch64-ibar

loongarch64-beqz
loongarch64-bnez
loongarch64-bceqz
loongarch64-bcnez
loongarch64-jirl
loongarch64-b
loongarch64-bl
loongarch64-beq
loongarch64-bne
loongarch64-blt
loongarch64-bge
loongarch64-bltu
loongarch64-bgeu

loongarch64-lu12iw    ;;lui
loongarch64-pcaddu12i ;;auipc

))

;;;============================================================================

(define-macro (loongarch64-implement)
  `(begin
     (loongarch64-registers-implement)))

(define-macro (loongarch64-define-registers . regs)
  (let* ((names (make-vector 35))
         (defs (apply
                 append
                 (map (lambda (r)
                        (let ((code (car r)))
                          (vector-set! names code (symbol->string (cadr r)))
                          (map (lambda (name)
                                 `(define-macro (,(string->symbol
                                                    (string-append "loongarch64-"
                                                                   (symbol->string name))))
                                    ,code))
                               (cdr r))))
                      regs)))) ; XXX
    `(begin
       (define-macro (loongarch64-registers-implement)
         `(begin
            (define (loongarch64-register-name reg)
              (vector-ref ',',names reg))))
       (define-macro (loongarch64-reg? x)
         `(fixnum? ,x))
       (define-macro (loongarch64-reg-field reg)
         reg)
       ,@defs)))

(loongarch64-define-registers
  (0 r0 zero)  ;; hardwired to 0, ignores writes
  (1 r1 ra)    ;; return address for jumps
  (2 r2 gp)    ;; stack pointer
  (3 r3 sp)    ;; global pointer
  (4 r4 a0)    ;; thread pointer
  (5 r4 v0)
  (6 r5 a1)    ;; temporary register 0
  (7 r5 v1)
  (8 r6 a2)    ;; temporary register 1
  (9 r7 a3)    ;; temporary register 2
  (10 r8 a4) ;; saved register 0 or frame pointer
  (11 r9 a5)    ;; saved register 1
  (12 r10 a6)  ;; return value or function argument 0
  (13 r11 a7)  ;; return value or function argument 1
  (14 r12 t0)  ;; function argument 2
  (15 r13 t1)  ;; function argument 3
  (16 r14 t2)  ;; function argument 4
  (17 r15 t3)  ;; function argument 5
  (18 r16 t4)  ;; function argument 6
  (19 r17 t5)  ;; function argument 7
  (20 r18 t6)  ;; saved register 2
  (21 r19 t7)  ;; saved register 3
  (22 r20 t8)  ;; saved register 4
  (23 r21 tp)  ;; saved register 5
  (24 r22 fp)  ;; saved register 6
  (25 r23 s0)  ;; saved register 7
  (26 r24 s1)  ;; saved register 8
  (27 r25 s2)  ;; saved register 9
  (28 r26 s3) ;; saved register 10
  (29 r27 s4) ;; saved register 11
  (30 r28 s5)  ;; temporary register 3
  (31 r29 s6)  ;; temporary register 4
  (32 r30 s7)  ;; temporary register 5
  (33 r31 s8)  ;; temporary register 6
  (34 pc))     ;; program counter
 
;;;============================================================================
