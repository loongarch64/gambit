;;==============================================================================

;;; File: "_t-cpu-backend-loongarch64.scm"

;;; Copyright (c) 2019 by Abdelhakim Qbaich, All Rights Reserved.

(include "generic.scm")

(include-adt "_loongarch64#.scm")
(include-adt "_asm#.scm")
(include-adt "_codegen#.scm")

;;------------------------------------------------------------------------------

(define (make-cgc-loongarch64)
  (let ((cgc (make-codegen-context)))
    (codegen-context-listing-format-set! cgc 'gnu)
    (asm-init-code-block cgc 0 'le)
    (loongarch64-arch-set! cgc 'loongarch64)
    cgc))

;;------------------------------------------------------------------------------
;;--------------------------- LOONGARCH 32-bit backend ----------------------------
;;------------------------------------------------------------------------------

(define (loongarch64-32-target)
  (cpu-make-target
    'loongarch64-32 '((".c" . LOONGARCH32-32))
    (make-backend make-cgc-loongarch64-32 (loongarch64-32-info) (loongarch64-instructions) (loongarch64-routines)))) ; XXX

(define (make-cgc-loongarch64-32)
  (make-cgc-loongarch64 'la32))

(define (loongarch64-32-info)
  (loongarch64-info 'la32 4 3))

;;------------------------------------------------------------------------------
;;--------------------------- LOONGARCH 64-bit backend ----------------------------
;;------------------------------------------------------------------------------

(define (loongarch64-64-target)
  (cpu-make-target
    'loongarch64-64 '((".c" . LOONGARCH64-64))
    (make-backend make-cgc-loongarch64-64 (loongarch64-64-info) (loongarch64-instructions) (loongarch64-routines)))) ; XXX

(define (make-cgc-loongarch64-64)
  (make-cgc-loongarch64 'lp64))

(define (loongarch64-64-info)
  (loongarch64-info 'lp64 8 2))

;;------------------------------------------------------------------------------

;; LOONGARCH backend info

(define (loongarch64-info arch width clo-trampoline-size)
  (cpu-make-info
    arch                    ;; Arch name
    width                   ;; Word width
    'le                     ;; Endianness
    #t                      ;; Load-store architecture?
    0                       ;; Frame offset
    clo-trampoline-size     ;; Closure trampoline size
    loongarch64-primitive-table   ;; Primitive table
    cpu-default-nb-gvm-regs ;; GVM register count
    cpu-default-nb-arg-regs ;; GVM register count for passing arguments
    loongarch64-registers         ;; Main registers
    (loongarch64-tp)             ;; Processor state pointer (PS)
    (loongarch64-sp)              ;; Stack pointer (SP)
    (loongarch64-gp)))           ;; Heap pointer (HP)

(define loongarch64-registers
  (vector
    (loongarch64-s2)   ;; R25
    (loongarch64-s3)   ;; R26
    (loongarch64-s4)   ;; R27
    (loongarch64-s5)   ;; R28
    (loongarch64-s6)   ;; R29
    (loongarch64-s7)   ;; R30
    (loongarch64-s8))) ;; R31

;;------------------------------------------------------------------------------

;; LOONGARCH64 Abstract machine instructions

(define (loongarch64-instructions)
  (make-instruction-dictionnary
    loongarch64-label-align           ;; am-lbl
    loongarch64-data-instr            ;; am-data
    loongarch64-mov-instr             ;; am-mov
    loongarch64-add-instr             ;; am-add
    loongarch64-sub-instr             ;; am-sub
    loongarch64-mul-instr             ;; am-mul
    loongarch64-div-instr             ;; am-div
    loongarch64-jmp-instr             ;; am-jmp
    loongarch64-cmp-jump-instr        ;; am-compare-jump
    loongarch64-cmp-move-instr))      ;; am-compare-move

(define (make-loongarch64-opnd opnd)
  (cond
    ((reg-opnd? opnd) opnd)
    ((int-opnd? opnd) (loongarch64-imm-int (int-opnd-value opnd)))
    ((lbl-opnd? opnd) (loongarch64-imm-lbl (lbl-opnd-label opnd) (lbl-opnd-offset opnd)))
    (else (compiler-internal-error "make-loongarch64-opnd - Unknown opnd: " opnd))))

(define (loongarch64-label-align cgc label-opnd #!optional (align #f))
  (if align ; XXX NOP in LA32I is 4 bytes with value 0x13
      (asm-align cgc (car align) (cdr align))
      (asm-align cgc (get-word-width cgc) 0))
  (loongarch64-label cgc (lbl-opnd-label label-opnd)))

(define loongarch64-data-instr
  (make-am-data loongarch64-db loongarch64-dh loongarch64-dw loongarch64-dd))

; TODO Deduplicate labels
(define (loongarch64-load-lbl cgc rd lbl-opnd)
  (let ((label (lbl-opnd-label lbl-opnd)))
    (loongarch64-load-data cgc rd
      (asm-label-id label) ; XXX
      (lambda (cgc)
        (codegen-fixup-lbl! cgc label (type-tag 'subtyped) #f (get-word-width-bits cgc) 2)))))

; TODO Deduplicate objects
(define (loongarch64-load-obj cgc rd obj-value)
  (loongarch64-load-data cgc rd
    (string-append "'" (object->string obj-value)) ; XXX
    (lambda (cgc)
      (codegen-fixup-obj! cgc obj-value (get-word-width-bits cgc) 2 #f))))

; TODO Deduplicate references to global variables
(define (loongarch64-load-glo cgc rd glo-name)
  (loongarch64-load-data cgc rd
    (string-append "&global[" (symbol->string glo-name) "]") ; XXX
    (lambda (cgc)
      (codegen-fixup-glo! cgc glo-name (get-word-width-bits cgc) 2 #f))))

(define (loongarch64-load-data cgc rd ref-name place-data)
  (define cgc-format? (codegen-context-listing-format cgc))

  (codegen-context-listing-format-set! cgc #f)
  (if (= (get-word-width cgc) 8)
      (begin
        ; Hack to load value
        (loongarch64-jirl cgc (loongarch64-s7) (loongarch64-r0) (loongarch64-imm-int 12 'J))
        (loongarch64-ldd cgc rd (loongarch64-s7) (loongarch64-imm-int 0))

        ; Worst case of load immediate
        ; (loongarch64-lu12iw cgc rd (loongarch64-imm-int 0 'U))
        ;; (loongarch64-addiw cgc rd rd (loongarch64-imm-int 0))
        ;; (loongarch64-addw cgc rd rd 0)
        ; (loongarch64-slliw cgc rd rd 0)
        ; (loongarch64-addiw cgc rd rd (loongarch64-imm-int 0))
        ; (loongarch64-slliw cgc rd rd 0)
        ; (loongarch64-addiw cgc rd rd (loongarch64-imm-int 0))
        ;; (loongarch64-slliw cgc rd rd 0)
        ;; (loongarch64-addiw cgc rd rd (loongarch64-imm-int 0))
        )
      (begin
        (loongarch64-lu12iw cgc rd (loongarch64-imm-int 0 'U))
        ; (loongarch64-addi cgc rd rd (loongarch64-imm-int 0))
        ))

  (if cgc-format?
      (let ((sym (if ref-name
                     (if (symbol? ref-name)
                         (symbol->string ref-name)
                         ref-name)
                     "???")))
        (loongarch64-listing cgc "liw" rd sym)
        (codegen-context-listing-format-set! cgc cgc-format?)))

  (place-data cgc))

(define (loongarch64-mov-instr cgc dst src #!optional (width #f))
  (define (with-reg func)
    (if (reg-opnd? dst)
        (func (make-loongarch64-opnd dst))
        (get-free-register cgc (list dst src)
          (lambda (reg)
            (func (make-loongarch64-opnd reg))))))

  (define (unaligned-mem-opnd? mem-opnd)
    (not (= 0 (modulo (mem-opnd-offset mem-opnd) (get-word-width cgc))))) ; XXX

  (define (regular-move src)
    (if (not (equal? dst src))
        (let ((64-bit? (= (get-word-width cgc) 8)))
          (cond
            ((reg-opnd? dst)
             (loongarch64-move cgc (make-loongarch64-opnd dst) src))
            ((glo-opnd? dst)
             (get-free-register cgc
               (list dst src src) ; XXX
               (lambda (reg)
                 (loongarch64-load-glo cgc reg (glo-opnd-name dst))
                 ((if 64-bit? loongarch64-std loongarch64-stw) cgc reg src (loongarch64-imm-int 0 'S))))) ; XXX
            ((mem-opnd? dst)
             (if (unaligned-mem-opnd? dst)
                 (get-free-register cgc
                   (list dst src src) ; XXX
                   (lambda (reg)
                     (am-add cgc reg (mem-opnd-base dst) (int-opnd (mem-opnd-offset dst)))
                     ((if 64-bit? loongarch64-std loongarch64-stw) cgc reg src (loongarch64-imm-int 0 'S)))) ; XXX
                 ((if 64-bit? loongarch64-std loongarch64-stw) cgc (mem-opnd-base dst) src (loongarch64-imm-int (mem-opnd-offset dst) 'S)))) ; XXX
            (else
              (compiler-internal-error
                "loongarch64-mov-instr - Unknown or incompatible destination: " dst))))))

  (if (not (equal? dst src))
      (let ((64-bit? (= (get-word-width cgc) 8)))
        (cond
          ((reg-opnd? src)
           (regular-move (make-loongarch64-opnd src)))
          ((int-opnd? src)
           (with-reg
             (lambda (reg)
               (loongarch64-liw cgc reg (make-loongarch64-opnd src))
               (regular-move reg))))
          ((lbl-opnd? src)
           (with-reg
             (lambda (reg)
               (loongarch64-load-lbl cgc reg src)
               (regular-move reg))))
          ((glo-opnd? src)
           (with-reg
             (lambda (reg)
               (loongarch64-load-glo cgc reg (glo-opnd-name src))
               ((if 64-bit? loongarch64-ldd loongarch64-ldw) cgc reg reg (loongarch64-imm-int 0)) ; XXX
               (regular-move reg))))
          ((obj-opnd? src)
           (with-reg
             (lambda (reg)
               (loongarch64-load-obj cgc reg (obj-opnd-value src))
               (regular-move reg))))
          ((mem-opnd? src)
           (with-reg
             (lambda (reg)
               (if (unaligned-mem-opnd? src)
                   (begin
                     (am-add cgc reg (mem-opnd-base src) (int-opnd (mem-opnd-offset src)))
                     ((if 64-bit? loongarch64-ldd loongarch64-ldw) cgc reg reg (loongarch64-imm-int 0))) ; XXX
                   ((if 64-bit? loongarch64-ldd loongarch64-ldw) cgc reg (mem-opnd-base src) (loongarch64-imm-int (mem-opnd-offset src)))) ; XXX
               (regular-move reg))))
          (else
            (compiler-internal-error "Cannot move : " dst " <- " src))))))

(define (loongarch64-arith-instr instr cgc dest opnd1 opnd2)
  (define (with-dest-reg dst)
    (load-multiple-if-necessary
      cgc
      '((reg) (reg int))
      (list opnd1 opnd2)
      (lambda (opnd1 opnd2) ; XXX addw, subw, addiw
        (cond ((reg-opnd? opnd2)
               (instr cgc dst opnd1 opnd2))
              ((in-range? -2048 2047 (int-opnd-value opnd2))
               (loongarch64-addiw cgc dst opnd1 (make-loongarch64-opnd opnd2)))
              (else
                (let ((use-reg
                        (lambda (reg)
                          (am-mov cgc reg opnd2)
                          (instr cgc dst opnd1 reg))))
                  (if (equal? dst opnd1)
                      (get-free-register cgc (list dest opnd1 opnd2) use-reg)
                      (use-reg dst)))))
        (am-mov cgc dest dst))))

  (if (reg-opnd? dest)
      (with-dest-reg dest)
      (get-free-register cgc (list dest opnd1 opnd2) with-dest-reg)))

(define (loongarch64-add-instr cgc dest opnd1 opnd2)
  (loongarch64-arith-instr loongarch64-addw cgc dest opnd1 opnd2))

(define (loongarch64-sub-instr cgc dest opnd1 opnd2)
  (loongarch64-arith-instr loongarch64-subw cgc dest opnd1
                     (if (int-opnd? opnd2)
                         (int-opnd-negative opnd2)
                         opnd2)))

(define (loongarch64-mul-instr cgc dest opnd1 opnd2)
  (compiler-internal-error "TODO loongarch64-mul-instr + encoding of M extension"))

(define (loongarch64-div-instr cgc dest opnd1 opnd2)
  (compiler-internal-error "TODO loongarch64-div-instr + encoding of M extension"))

(define (loongarch64-jmp-instr cgc opnd)
  (if (lbl-opnd? opnd)
      (loongarch64-jr cgc (make-loongarch64-opnd opnd))
      (load-if-necessary cgc '(reg int lbl) opnd
        (lambda (opnd)
          (if (reg-opnd? opnd)
              (loongarch64-jr cgc (make-loongarch64-opnd opnd))
              (loongarch64-jr cgc (make-loongarch64-opnd opnd)))))))

(define (loongarch64-branch-instrs condition)
  (case (get-condition condition)
    ((equal)
     (cons loongarch64-beq loongarch64-bne))
    ((greater)
     (cond
       ((and (cond-is-equal condition) (cond-is-signed condition))
        (cons loongarch64-bge loongarch64-blt))
       ((and (cond-is-equal condition) (not (cond-is-signed condition)))
        (cons loongarch64-bgeu loongarch64-bltu))
       ((and (not (cond-is-equal condition)) (cond-is-signed condition))
        (cons loongarch64-bgt loongarch64-ble))
       ((and (not (cond-is-equal condition)) (not (cond-is-signed condition)))
        (cons loongarch64-bgtu loongarch64-bleu))))
    ((not-equal lesser)
     (flip (loongarch64-branch-instrs (invert-condition condition))))
    (else
      (compiler-internal-error
        "loongarch64-branch-instrs - Unknown condition: " condition))))

(define (loongarch64-cmp-jump-instr cgc test loc-true loc-false #!optional (opnds-width #f))
  (let ((opnd1 (test-operand1 test))
        (opnd2 (test-operand2 test))
        (condition (test-condition test)))
    (let ((instrs (loongarch64-branch-instrs condition)))
      (load-multiple-if-necessary
        cgc
        '((reg) (reg))
        (list opnd1 opnd2)
        (lambda (reg1 reg2)
          (cond ((and loc-true loc-false)
                 ((car instrs) cgc reg1 reg2 (make-loongarch64-opnd loc-true))
                 (loongarch64-jr cgc (make-loongarch64-opnd loc-false)))
                ((and loc-true (not loc-false))
                 ((car instrs) cgc reg1 reg2 (make-loongarch64-opnd loc-true)))
                ((and (not loc-true) loc-false)
                 ((cdr instrs) cgc reg1 reg2 (make-loongarch64-opnd loc-false)))))))))

(define (loongarch64-cmp-move-instr cgc condition dest opnd1 opnd2 true-opnd false-opnd #!optional (opnds-width #f))
  (compiler-internal-error "TODO loongarch64-cmp-move-instr"))

;;------------------------------------------------------------------------------

;; Backend routines

(define (loongarch64-routines)
  (make-routine-dictionnary
    am-default-poll
    am-default-set-nargs
    am-default-check-nargs
    (am-default-allocate-memory
      (lambda (cgc dest-reg base-reg offset)
        (am-add cgc dest-reg base-reg (int-opnd offset))))
    am-default-place-extra-data))

;;------------------------------------------------------------------------------

;; Primitives

(define loongarch64-prim-##fixnum?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 temp1 temp2)
      (am-mov cgc temp1 (int-opnd type-tag-mask))
      (loongarch64-and cgc temp2 arg1 temp1) ; XXX
      (am-cond-return cgc result-action
        (lambda (cgc lbl) (loongarch64-beq cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) (loongarch64-bne cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##pair?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 temp1 temp2)
      (am-mov cgc temp1 (int-opnd type-tag-mask))
      (loongarch64-not cgc temp2 arg1)
      (loongarch64-and cgc temp2 temp1 temp2) ; XXX
      (am-cond-return cgc result-action
        (lambda (cgc lbl) (loongarch64-beq cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) (loongarch64-bne cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##special?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 temp1 temp2)
      (am-mov cgc temp1 (int-opnd (type-tag 'special)))
      (loongarch64-andi cgc temp2 arg1 (loongarch64-imm-int type-tag-mask)) ; XXX
      (am-cond-return cgc result-action
        (lambda (cgc lbl) (loongarch64-beq cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) (loongarch64-bne cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##mem-allocated?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 temp1 temp2)
      (am-mov cgc temp1 (int-opnd (bitwise-and (type-tag 'subtyped) (type-tag 'pair))))
      (loongarch64-and cgc temp2 arg1 temp1) ; XXX
      (am-cond-return cgc result-action
        (lambda (cgc lbl) (loongarch64-beq cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) (loongarch64-bne cgc temp1 temp2 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##char?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 temp1 temp2)
      (let ((test-int
              (- (type-tag 'special) (expt 2 (- (get-word-width-bits cgc) 1)))))
        (am-mov cgc temp1 (int-opnd test-int))
        (loongarch64-and cgc temp1 arg1 temp1)
        (am-mov cgc temp2 (int-opnd (type-tag 'special)))
        (am-cond-return cgc result-action
          (lambda (cgc lbl) (loongarch64-beq cgc temp1 temp2 (make-loongarch64-opnd lbl)))
          (lambda (cgc lbl) (loongarch64-bne cgc temp1 temp2 (make-loongarch64-opnd lbl)))
          true-opnd:  (int-opnd (imm-encode #t))
          false-opnd: (int-opnd (imm-encode #f)))))))

(define (loongarch64-prim-##boolean-or? desc)
  (const-nargs-prim 1 1 '((reg))
    (lambda (cgc result-action args arg1 tmp1)
      (let ((test-int (+ ((imm-encoder desc)) (- type-tag-mask (desc-type-tag desc)))))
        (loongarch64-andi cgc tmp1 arg1 (loongarch64-imm-int test-int))
        (am-if-eq cgc tmp1 (int-opnd ((imm-encoder desc)))
          (lambda (cgc) (am-return-const cgc result-action #t))
          (lambda (cgc) (am-return-const cgc result-action #f))
          #f
          (get-word-width-bits cgc))))))

(define loongarch64-prim-##subtyped?
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 tmp1 tmp2)
      (am-mov cgc tmp1 (int-opnd (type-tag 'subtyped)))
      (loongarch64-andi cgc tmp2 arg1 (loongarch64-imm-int type-tag-mask))
      (am-cond-return cgc result-action
        (lambda (cgc lbl) (loongarch64-beq cgc tmp1 tmp2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) (loongarch64-bne cgc tmp1 tmp2 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define (loongarch64-prim-##subtype? subtype-desc) ; XXX
  (const-nargs-prim 1 2 '((reg))
    (lambda (cgc result-action args arg1 tmp1 tmp2)
      (let ((width (get-word-width-bits cgc)))
        (am-mov cgc tmp1 (int-opnd (type-tag 'subtyped)))
        (loongarch64-andi cgc tmp2 arg1 (loongarch64-imm-int type-tag-mask))
        (am-cond-return cgc result-action
          (lambda (cgc lbl)
            (loongarch64-bne cgc tmp1 tmp2 (make-loongarch64-opnd lbl))
            (am-mov cgc tmp1 arg1)
            (am-mov cgc tmp1 (opnd-with-offset tmp1 (- 0 (type-tag 'subtyped) width width)))
            (loongarch64-andi cgc tmp1 tmp1 (loongarch64-imm-int subtype-tag-mask))
            (am-mov cgc tmp2 (int-opnd (subtype-tag (ref-subtype subtype-desc))))
            (loongarch64-bne cgc tmp1 tmp2 (make-loongarch64-opnd lbl)))
          (lambda (cgc lbl)
            (loongarch64-bne cgc tmp1 tmp2 (make-loongarch64-opnd lbl))
            (am-mov cgc tmp1 arg1)
            (am-mov cgc tmp1 (opnd-with-offset tmp1 (- 0 (type-tag 'subtyped) width width)))
            (loongarch64-andi cgc tmp1 tmp1 (loongarch64-imm-int subtype-tag-mask))
            (am-mov cgc tmp2 (int-opnd (subtype-tag (ref-subtype subtype-desc))))
            (loongarch64-bne cgc tmp1 tmp2 (make-loongarch64-opnd lbl)))
          true-opnd:  (int-opnd (imm-encode #f))
          false-opnd: (int-opnd (imm-encode #t)))))))

(define loongarch64-prim-##fx+
  (foldl-prim
    (lambda (cgc accum opnd) (am-add cgc accum accum opnd))
    allowed-opnds: '(reg mem int)
    allowed-opnds-accum: '(reg mem)
    start-value: 0
    start-value-null?: #t
    reduce-1: am-mov
    commutative: #t))

(define loongarch64-prim-##fx+? ; XXX
  (const-nargs-prim 2 1 '((reg))
    (lambda (cgc result-action args arg1 arg2 tmp1)
      (am-add cgc tmp1 arg1 arg2)
      (am-cond-return cgc result-action
        (lambda (cgc lbl)
          (loongarch64-slt cgc arg1 tmp1 arg1)
          (loongarch64-sltz cgc arg2 arg2)
          (loongarch64-beq cgc arg1 arg2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl)
          (loongarch64-slt cgc arg1 tmp1 arg1)
          (loongarch64-sltz cgc arg2 arg2)
          (loongarch64-bne cgc arg1 arg2 (make-loongarch64-opnd lbl)))
        true-opnd:  tmp1 ; XXX
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##fx-
  (foldl-prim
    (lambda (cgc accum opnd) (am-sub cgc accum accum opnd))
    allowed-opnds: '(reg mem int)
    allowed-opnds-accum: '(reg mem)
    ; start-value: 0 ;; Start the fold on the first operand
    reduce-1: (lambda (cgc dst opnd) (am-sub cgc dst (loongarch64-zero) opnd))
    commutative: #f))

(define loongarch64-prim-##fx-? ; XXX
  (const-nargs-prim 2 1 '((reg))
    (lambda (cgc result-action args arg1 arg2 tmp1)
      (am-sub cgc tmp1 arg1 arg2)
      (am-cond-return cgc result-action
        (lambda (cgc lbl)
          (loongarch64-slt cgc arg1 tmp1 arg1)
          (loongarch64-sltz cgc arg2 arg2)
          (loongarch64-beq cgc arg1 arg2 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl)
          (loongarch64-slt cgc arg1 tmp1 arg1)
          (loongarch64-sltz cgc arg2 arg2)
          (loongarch64-bne cgc arg1 arg2 (make-loongarch64-opnd lbl)))
        true-opnd:  tmp1 ; XXX
        false-opnd: (int-opnd (imm-encode #f))))))

(define (loongarch64-compare-prim condition)
  (foldl-compare-prim
    (lambda (cgc opnd1 opnd2 true-label false-label)
      (am-compare-jump cgc
        (mk-test condition opnd1 opnd2)
        false-label true-label
        (get-word-width-bits cgc)))
    allowed-opnds1: '(reg mem)
    allowed-opnds2: '(reg int)))

(define loongarch64-prim-##fx<  (loongarch64-compare-prim (condition-greater #t #t)))
(define loongarch64-prim-##fx<= (loongarch64-compare-prim (condition-greater #f #t)))
(define loongarch64-prim-##fx>  (loongarch64-compare-prim (condition-lesser #t #t)))
(define loongarch64-prim-##fx>= (loongarch64-compare-prim (condition-lesser #f #t)))
(define loongarch64-prim-##fx=  (loongarch64-compare-prim condition-not-equal))

(define (loongarch64-prim-##fxparity? parity)
  (const-nargs-prim 1 1 '((reg))
    (lambda (cgc result-action args arg1 tmp1)
      (loongarch64-andi cgc tmp1 arg1 (loongarch64-imm-int (imm-encode 1)))
      (am-cond-return cgc result-action
        (lambda (cgc lbl) ((if (eq? parity 'even) loongarch64-beqz loongarch64-bnez) cgc tmp1 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) ((if (eq? parity 'even) loongarch64-bnez loongarch64-beqz) cgc tmp1 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define (loongarch64-prim-##fxsign? sign)
  (const-nargs-prim 1 0 '((reg))
    (lambda (cgc result-action args arg1)
      (am-cond-return cgc result-action
        (lambda (cgc lbl) ((if (eq? sign 'positive) loongarch64-bgtz loongarch64-bltz) cgc arg1 (make-loongarch64-opnd lbl)))
        (lambda (cgc lbl) ((if (eq? sign 'positive) loongarch64-blez loongarch64-bgez) cgc arg1 (make-loongarch64-opnd lbl)))
        true-opnd:  (int-opnd (imm-encode #t))
        false-opnd: (int-opnd (imm-encode #f))))))

(define loongarch64-prim-##cons
  (lambda (cgc result-action args)
    (with-result-opnd cgc result-action args
      allowed-opnds: '(reg)
      fun:
        (lambda (result-reg result-opnd-in-args)
          (let* ((width (get-word-width cgc))
                 (size (* width 3))
                 (tag (desc-type-tag pair-desc))
                 (subtype (subtype-tag (ref-subtype pair-desc)))
                 (offset (+ tag (* 2 width))))

            (am-allocate-memory cgc result-reg size offset
              (codegen-context-frame cgc))

            (am-mov cgc
              (mem-opnd result-reg (- offset))
              (int-opnd (+ subtype (arithmetic-shift (* width 2) (fx+ head-type-tag-bits subtype-tag-bits))))
              (get-word-width-bits cgc))

            (am-mov cgc
              (mem-opnd result-reg (- width offset))
              (cadr args)
              (get-word-width-bits cgc))

            (am-mov cgc
              (mem-opnd result-reg (- (* 2 width) offset))
              (car args)
              (get-word-width-bits cgc))

            (am-return-opnd cgc result-action result-reg))))))

;; Doesn't support width not equal to (get-word-width cgc)
;; as am-return-opnd uses the default width
(define (loongarch64-object-dyn-read-prim desc #!optional (width #f))
  (if (imm-desc? desc)
    (compiler-internal-error "Object isn't a reference"))

  (const-nargs-prim 2 0 '((reg) (reg int))
    (lambda (cgc result-action args obj-reg index-opnd)
      (let* ((width (if width width (get-word-width cgc)))
             (index-shift (- (integer-length width) 1 type-tag-bits))
             (0-offset (body-offset (desc-type desc) width)))

        (if (> 0 index-shift)
          (compiler-internal-error "loongarch64-object-dyn-read-prim - Invalid index-shift"))

        (if (int-opnd? index-opnd)
          (am-return-opnd cgc result-action
            (mem-opnd obj-reg
              (+ (arithmetic-shift (int-opnd-value index-opnd) index-shift) 0-offset)))
          (with-result-opnd cgc result-action args
            allowed-opnds: '(reg)
            fun:
            (lambda (result-reg result-opnd-in-args)
              (cond
                ((<= 1 index-shift) ;; Multiply
                 (loongarch64-slliw cgc result-reg index-opnd index-shift)
                 (loongarch64-addw cgc result-reg obj-reg result-reg))
                ((>= -1 index-shift) ;; Divides
                 (loongarch64-srliw cgc result-reg index-opnd (- index-shift))
                 (loongarch64-addw cgc result-reg obj-reg result-reg))
                (else
                  (loongarch64-addw cgc result-reg obj-reg index-opnd)))
              (am-return-opnd cgc result-action (mem-opnd result-reg 0-offset)))))))))

;; Doesn't support width not equal to (get-word-width cgc)
;; as am-mov uses the default width
(define (loongarch64-object-dyn-set-prim desc #!optional (width #f))
  (if (imm-desc? desc)
    (compiler-internal-error "Object isn't a reference"))

  (const-nargs-prim 3 0 '((reg) (reg int))
    (lambda (cgc result-action args obj-reg index-opnd new-val-opnd)
      (let* ((width (if width width (get-word-width cgc)))
             (index-shift (- (integer-length width) 1 type-tag-bits))
             (0-offset (body-offset (desc-type desc) width)))

        (if (> 0 index-shift)
          (compiler-internal-error "loongarch64-object-dyn-set-prim - Invalid index-shift"))

        (if (int-opnd? index-opnd)
          (am-mov cgc
            (mem-opnd obj-reg
              (+ (arithmetic-shift (int-opnd-value index-opnd) index-shift) 0-offset))
            new-val-opnd)
          (with-result-opnd cgc result-action args
            allowed-opnds: '(reg)
            fun:
            (lambda (result-reg result-opnd-in-args)
              (cond
                ((<= 1 index-shift) ;; Multiply
                 (loongarch64-slliw cgc result-reg index-opnd index-shift)
                 (loongarch64-addw cgc result-reg obj-reg result-reg))
                ((>= -1 index-shift) ;; Divides
                 (loongarch64-srliw cgc result-reg index-opnd (- index-shift))
                 (loongarch64-addw cgc result-reg obj-reg result-reg))
                (else
                  (loongarch64-addw cgc result-reg obj-reg index-opnd)))
              (am-mov cgc (mem-opnd result-reg 0-offset) new-val-opnd))))
        (am-return-const cgc result-action (void))))))

(define (loongarch64-prim-##vector-length #!optional (width #f))
  (const-nargs-prim 1 0 '((reg))
    (lambda (cgc result-action args obj-reg)
      (let* ((width (if width width (get-word-width cgc)))
             (log2-width (- (integer-length width) 1))
             (header-offset (header-offset 'subtyped width))
             (shift-count (- (+ head-type-tag-bits subtype-tag-bits log2-width) type-tag-bits)))
        ;; Load header
        (am-mov cgc obj-reg (mem-opnd obj-reg header-offset))
        ;; Shift header in order to ony keep length in bytes
        ;; Divides that value by the number of bytes per word
        ;; Multiply by the tag width
        (loongarch64-srliw cgc obj-reg obj-reg shift-count)
        (am-return-opnd cgc result-action obj-reg)))))

(define loongarch64-primitive-table
  (let ((table (make-table test: equal?)))
    (table-set! table '##identity       (make-prim-obj ##identity-primitive    1 #t #t))
    (table-set! table '##not            (make-prim-obj ##not-primitive         1 #t #t #t))
    (table-set! table '##void           (make-prim-obj ##void-primitive        0 #t #t))
    (table-set! table '##eof-object     (make-prim-obj ##eof-object-primitive  0 #t #t))
    (table-set! table '##eof-object?    (make-prim-obj ##eof-object?-primitive 1 #t #t #t))
    (table-set! table '##eq?            (make-prim-obj ##eq?-primitive         2 #t #t #t))
    (table-set! table '##null?          (make-prim-obj ##null?-primitive       1 #t #f #t))
    (table-set! table '##fxzero?        (make-prim-obj ##fxzero?-primitive     1 #t #t #t))

    (table-set! table '##fixnum?        (make-prim-obj loongarch64-prim-##fixnum?        1 #t #t #t))
    (table-set! table '##pair?          (make-prim-obj loongarch64-prim-##pair?          1 #t #t #t))
    (table-set! table '##special?       (make-prim-obj loongarch64-prim-##special?       1 #t #t #t))
    (table-set! table '##mem-allocated? (make-prim-obj loongarch64-prim-##mem-allocated? 1 #t #t #t))
    (table-set! table '##char?          (make-prim-obj loongarch64-prim-##char?          1 #t #t #t))

    (table-set! table '##boolean?       (make-prim-obj (loongarch64-prim-##boolean-or? tru-desc)  1 #t #t #t))
    (table-set! table '##false-or-null? (make-prim-obj (loongarch64-prim-##boolean-or? nul-desc)  1 #t #t #t))
    (table-set! table '##false-or-void? (make-prim-obj (loongarch64-prim-##boolean-or? void-desc) 1 #t #t #t))

    (table-set! table '##subtyped?     (make-prim-obj loongarch64-prim-##subtyped? 1 #t #t #t))
    (table-set! table '##vector?       (make-prim-obj (loongarch64-prim-##subtype? vector-desc)       1 #t #t #t))
    (table-set! table '##ratnum?       (make-prim-obj (loongarch64-prim-##subtype? ratnum-desc)       1 #t #t #t))
    (table-set! table '##cpxnum?       (make-prim-obj (loongarch64-prim-##subtype? cpxnum-desc)       1 #t #t #t))
    (table-set! table '##structure?    (make-prim-obj (loongarch64-prim-##subtype? structure-desc)    1 #t #t #t))
    (table-set! table '##meroon?       (make-prim-obj (loongarch64-prim-##subtype? meroon-desc)       1 #t #t #t))
    (table-set! table '##jazz?         (make-prim-obj (loongarch64-prim-##subtype? jazz-desc)         1 #t #t #t))
    (table-set! table '##symbol?       (make-prim-obj (loongarch64-prim-##subtype? symbol-desc)       1 #t #t #t))
    (table-set! table '##keyword?      (make-prim-obj (loongarch64-prim-##subtype? keyword-desc)      1 #t #t #t))
    (table-set! table '##frame?        (make-prim-obj (loongarch64-prim-##subtype? frame-desc)        1 #t #t #t))
    (table-set! table '##continuation? (make-prim-obj (loongarch64-prim-##subtype? continuation-desc) 1 #t #t #t))
    (table-set! table '##promise?      (make-prim-obj (loongarch64-prim-##subtype? promise-desc)      1 #t #t #t))
    (table-set! table '##procedure?    (make-prim-obj (loongarch64-prim-##subtype? procedure-desc)    1 #t #t #t))
    (table-set! table '##return?       (make-prim-obj (loongarch64-prim-##subtype? return-desc)       1 #t #t #t))
    (table-set! table '##foreign?      (make-prim-obj (loongarch64-prim-##subtype? foreign-desc)      1 #t #t #t))
    (table-set! table '##string?       (make-prim-obj (loongarch64-prim-##subtype? string-desc)       1 #t #t #t))
    (table-set! table '##s8vector?     (make-prim-obj (loongarch64-prim-##subtype? s8vector-desc)     1 #t #t #t))
    (table-set! table '##u8vector?     (make-prim-obj (loongarch64-prim-##subtype? u8vector-desc)     1 #t #t #t))
    (table-set! table '##s16vector?    (make-prim-obj (loongarch64-prim-##subtype? s16vector-desc)    1 #t #t #t))
    (table-set! table '##u16vector?    (make-prim-obj (loongarch64-prim-##subtype? u16vector-desc)    1 #t #t #t))
    (table-set! table '##s32vector?    (make-prim-obj (loongarch64-prim-##subtype? s32vector-desc)    1 #t #t #t))
    (table-set! table '##u32vector?    (make-prim-obj (loongarch64-prim-##subtype? u32vector-desc)    1 #t #t #t))
    (table-set! table '##f32vector?    (make-prim-obj (loongarch64-prim-##subtype? f32vector-desc)    1 #t #t #t))
    (table-set! table '##s64vector?    (make-prim-obj (loongarch64-prim-##subtype? s64vector-desc)    1 #t #t #t))
    (table-set! table '##u64vector?    (make-prim-obj (loongarch64-prim-##subtype? u64vector-desc)    1 #t #t #t))
    (table-set! table '##f64vector?    (make-prim-obj (loongarch64-prim-##subtype? f64vector-desc)    1 #t #t #t))
    (table-set! table '##flonum?       (make-prim-obj (loongarch64-prim-##subtype? flonum-desc)       1 #t #t #t))
    (table-set! table '##bignum?       (make-prim-obj (loongarch64-prim-##subtype? bignum-desc)       1 #t #t #t))

    (table-set! table '##fx+            (make-prim-obj loongarch64-prim-##fx+  2 #t #f))
    (table-set! table '##fx+?           (make-prim-obj loongarch64-prim-##fx+? 2 #t #t #t))
    (table-set! table '##fx-            (make-prim-obj loongarch64-prim-##fx-  2 #t #f))
    (table-set! table '##fx-?           (make-prim-obj loongarch64-prim-##fx-? 2 #t #t #t))
    (table-set! table '##fx<            (make-prim-obj loongarch64-prim-##fx<  2 #t #t))
    (table-set! table '##fx<=           (make-prim-obj loongarch64-prim-##fx<= 2 #t #t))
    (table-set! table '##fx>            (make-prim-obj loongarch64-prim-##fx>  2 #t #t))
    (table-set! table '##fx>=           (make-prim-obj loongarch64-prim-##fx>= 2 #t #t))
    (table-set! table '##fx=            (make-prim-obj loongarch64-prim-##fx=  2 #t #t))

    (table-set! table '##fxeven?        (make-prim-obj (loongarch64-prim-##fxparity? 'even)   1 #t #t #t))
    (table-set! table '##fxodd?         (make-prim-obj (loongarch64-prim-##fxparity? 'odd)    1 #t #t #t))
    (table-set! table '##fxnegative?    (make-prim-obj (loongarch64-prim-##fxsign? 'negative) 1 #t #t #t))
    (table-set! table '##fxpositive?    (make-prim-obj (loongarch64-prim-##fxsign? 'positive) 1 #t #t #t))

    (table-set! table '##car            (make-prim-obj (object-read-prim pair-desc '(a)) 1 #t #f))
    (table-set! table '##cdr            (make-prim-obj (object-read-prim pair-desc '(d)) 1 #t #f))

    (table-set! table '##caar           (make-prim-obj (object-read-prim pair-desc '(a a)) 1 #t #f))
    (table-set! table '##cadr           (make-prim-obj (object-read-prim pair-desc '(a d)) 1 #t #f))
    (table-set! table '##cddr           (make-prim-obj (object-read-prim pair-desc '(d d)) 1 #t #f))
    (table-set! table '##cdar           (make-prim-obj (object-read-prim pair-desc '(d a)) 1 #t #f))

    (table-set! table '##caaar          (make-prim-obj (object-read-prim pair-desc '(a a a)) 1 #t #f))
    (table-set! table '##caadr          (make-prim-obj (object-read-prim pair-desc '(a a d)) 1 #t #f))
    (table-set! table '##cadar          (make-prim-obj (object-read-prim pair-desc '(a d a)) 1 #t #f))
    (table-set! table '##caddr          (make-prim-obj (object-read-prim pair-desc '(a d d)) 1 #t #f))
    (table-set! table '##cdaar          (make-prim-obj (object-read-prim pair-desc '(d a a)) 1 #t #f))
    (table-set! table '##cdadr          (make-prim-obj (object-read-prim pair-desc '(d a d)) 1 #t #f))
    (table-set! table '##cddar          (make-prim-obj (object-read-prim pair-desc '(d d a)) 1 #t #f))
    (table-set! table '##cdddr          (make-prim-obj (object-read-prim pair-desc '(d d d)) 1 #t #f))

    (table-set! table '##caaaar         (make-prim-obj (object-read-prim pair-desc '(a a a a)) 1 #t #f))
    (table-set! table '##cdaaar         (make-prim-obj (object-read-prim pair-desc '(d a a a)) 1 #t #f))
    (table-set! table '##cadaar         (make-prim-obj (object-read-prim pair-desc '(a d a a)) 1 #t #f))
    (table-set! table '##cddaar         (make-prim-obj (object-read-prim pair-desc '(d d a a)) 1 #t #f))
    (table-set! table '##caadar         (make-prim-obj (object-read-prim pair-desc '(a a d a)) 1 #t #f))
    (table-set! table '##cdadar         (make-prim-obj (object-read-prim pair-desc '(d a d a)) 1 #t #f))
    (table-set! table '##caddar         (make-prim-obj (object-read-prim pair-desc '(a d d a)) 1 #t #f))
    (table-set! table '##cdddar         (make-prim-obj (object-read-prim pair-desc '(d d d a)) 1 #t #f))
    (table-set! table '##caaadr         (make-prim-obj (object-read-prim pair-desc '(a a a d)) 1 #t #f))
    (table-set! table '##cdaadr         (make-prim-obj (object-read-prim pair-desc '(d a a d)) 1 #t #f))
    (table-set! table '##cadadr         (make-prim-obj (object-read-prim pair-desc '(a d a d)) 1 #t #f))
    (table-set! table '##cddadr         (make-prim-obj (object-read-prim pair-desc '(d d a d)) 1 #t #f))
    (table-set! table '##caaddr         (make-prim-obj (object-read-prim pair-desc '(a a d d)) 1 #t #f))
    (table-set! table '##cdaddr         (make-prim-obj (object-read-prim pair-desc '(d a d d)) 1 #t #f))
    (table-set! table '##cadddr         (make-prim-obj (object-read-prim pair-desc '(a d d d)) 1 #t #f))
    (table-set! table '##cddddr         (make-prim-obj (object-read-prim pair-desc '(d d d d)) 1 #t #f))

    (table-set! table '##set-car!       (make-prim-obj (object-set-prim pair-desc 2) 2 #t #f))
    (table-set! table '##set-cdr!       (make-prim-obj (object-set-prim pair-desc 1) 2 #t #f))

    (table-set! table '##cons           (make-prim-obj loongarch64-prim-##cons 2 #t #f))

    (table-set! table '##vector-ref     (make-prim-obj (loongarch64-object-dyn-read-prim vector-desc) 2 #t #t))
    (table-set! table '##vector-set!    (make-prim-obj (loongarch64-object-dyn-set-prim vector-desc) 3 #t #f))
    (table-set! table '##vector-length  (make-prim-obj (loongarch64-prim-##vector-length #f) 1 #t #t))

    table))

;;==============================================================================
