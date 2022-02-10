;;;============================================================================

;;; File: "_loongarch64.scm"

;;; Copyright (c) 2022 by Hanjianjun, All Rights Reserved.

;;;============================================================================

;;; This module implements the LOONGARCH64 instruction encoding.

(namespace ("_loongarch64#") ("" include))
(include "~~lib/gambit#.scm")

(include "_assert#.scm")
(include "_asm#.scm")
(include "_loongarch64#.scm")
(include "_codegen#.scm")

(loongarch64-implement)

;;;============================================================================

;;; Architecture selection (either loongarch32 or loongarch64).

(define (loongarch64-arch-set! cgc arch)
  (codegen-context-arch-set! cgc arch))

(define (loongarch64-64bit-mode? cgc)
  (eq? (codegen-context-arch cgc) 'loongarch64))

(define (loongarch64-word-width cgc)
  (if (loongarch64-64bit-mode? cgc) 64 32))

(define-macro (loongarch64-assert-64bit-mode cgc)
  `(assert (loongarch64-64bit-mode? ,cgc)
    "instruction only valid for LA64I"))

(define-macro (loongarch64-assert-32bit-mode cgc)
  `(assert (not (loongarch64-64bit-mode? ,cgc))
    "instruction only valid for LA32I"))

;;;----------------------------------------------------------------------------

;;; Instruction operands.

(define (loongarch64-imm? x) (pair? x))

(define (loongarch64-imm-int value #!optional (type 'I)) (cons type value))
(define (loongarch64-imm-int? x) (and (pair? x) (symbol? (car x)) (number? (cdr x)))) ; XXX
(define (loongarch64-imm-int-type x) (car x))
(define (loongarch64-imm-int-value x) (cdr x))

(define (loongarch64-imm-lbl label #!optional (offset 4)) (cons offset label)) ; XXX
(define (loongarch64-imm-lbl? x) (and (pair? x) (fixnum? (car x)) (vector? (cdr x))))
(define (loongarch64-imm-lbl-offset x) (car x))
(define (loongarch64-imm-lbl-label x) (cdr x))

(define (loongarch64-imm->instr imm)
  (let ((val (loongarch64-imm-int-value imm)))
    (fxand
      #xffffffff
      (case (loongarch64-imm-int-type imm)
        ((I) (fxarithmetic-shift val 20))
        ((S) (fx+ (fxarithmetic-shift (fxand val #x1f) 7)
                  (fxarithmetic-shift (fxand val #xfe0) 20)))
        ((B) (assert (fxeven? val)
                     "invalid immediate value")
             (fx+ (fxarithmetic-shift (fxand val #x800) -4)
                  (fxarithmetic-shift (fxand val #x1e) 7)
                  (fxarithmetic-shift (fxand val #x7e0) 20)
                  (fxarithmetic-shift (fxand val #x1000) 19)))
        ((U) (assert (fxzero? (fxand val #xfff))
                     "invalid immediate value")
             val)
        ((J) (assert (fxeven? val)
                     "invalid immediate value")
             (fx+ (fxand val #xff000)
                  (fxarithmetic-shift (fxand val #x800) 9)
                  (fxarithmetic-shift (fxand val #x7fe) 20)
                  (fxarithmetic-shift (fxand val #x100000) 11)))
        (else (error "invalid immediate type" imm))))))

;;;----------------------------------------------------------------------------

;;; Listing generation.

(define (loongarch64-listing cgc mnemonic . opnds)

  (define (opnd-format opnd)
    (cond ((loongarch64-reg? opnd)
           (loongarch64-register-name opnd))
          ((loongarch64-imm? opnd)
           (cond ((loongarch64-imm-int? opnd)
                  (loongarch64-imm-int-value opnd))
                 ((loongarch64-imm-lbl? opnd)
                  (asm-label-name (loongarch64-imm-lbl-label opnd)))
                 (else
                   (error "unsupported immediate" opnd))))
          (else
            opnd)))

  (define (instr-format)
    (let ((operands (asm-separated-list (map opnd-format opnds) ", ")))
      (cons #\tab
            (cons mnemonic
                  (if (pair? operands)
                      (cons #\tab operands)
                      '())))))

  (asm-listing cgc (instr-format)))

;;;----------------------------------------------------------------------------

;;; Pseudo operations.

(define (loongarch64-label cgc label)
  (asm-label cgc label)
  (if (codegen-context-listing-format cgc)
      (asm-listing cgc (list (asm-label-name label) ":"))))

(define (loongarch64-db cgc . elems)
  (loongarch64-data-elems cgc elems 8))

(define (loongarch64-d2b cgc . elems)
  (loongarch64-data-elems cgc elems 16))

(define (loongarch64-dh cgc . elems)
  (loongarch64-data-elems cgc elems 16))

(define (loongarch64-ds cgc . elems)
  (loongarch64-data-elems cgc elems 16))

(define (loongarch64-d4b cgc . elems)
  (loongarch64-data-elems cgc elems 32))

(define (loongarch64-dw cgc . elems)
  (loongarch64-data-elems cgc elems 32))

(define (loongarch64-dl cgc . elems)
  (loongarch64-data-elems cgc elems 32))

(define (loongarch64-d8b cgc . elems)
  (loongarch64-data-elems cgc elems 64))

(define (loongarch64-dd cgc . elems)
  (loongarch64-data-elems cgc elems 64))

(define (loongarch64-dq cgc . elems)
  (loongarch64-data-elems cgc elems 64))

(define (loongarch64-data-elems cgc elems width) ; XXX

  (define max-per-line 4)

  (let ((v (list->vector elems)))
    (let loop1 ((i 0))
      (if (fx< i (vector-length v))
          (let ((lim (fxmin (fx+ i max-per-line) (vector-length v))))
            (let loop2 ((j i) (rev-lst '()))
              (if (fx< j lim)
                  (let ((x (vector-ref v j)))
                    (asm-int-le cgc x width)
                    (loop2 (fx+ j 1) (cons x rev-lst)))
                  (begin
                    (if (codegen-context-listing-format cgc)
                        (asm-listing
                          cgc
                          (list #\tab
                                (cond ((fx= width 8)  ".byte")
                                      ((fx= width 16) ".half")
                                      ((fx= width 32) ".word")
                                      ((fx= width 64) ".dword")
                                      (else (error "unknown data width")))
                                #\tab
                                (asm-separated-list (reverse rev-lst) ","))))
                    (loop1 lim)))))))))

;;;----------------------------------------------------------------------------

;;; XXX Pseudo instructions.

; (define (loongarch64-la cgc rd symbol))
; (define (loongarch64-lb cgc rd symbol))
; (define (loongarch64-lh cgc rd symbol))
; (define (loongarch64-lw cgc rd symbol))
; (define (loongarch64-ld cgc rd symbol))
; (define (loongarch64-sb cgc rd symbol rt))
; (define (loongarch64-sh cgc rd symbol rt))
; (define (loongarch64-sw cgc rd symbol rt))
; (define (loongarch64-sd cgc rd symbol rt))

(define (loongarch64-nop cgc)
  (loongarch64-andi cgc (loongarch64-r0) (loongarch64-r0) (loongarch64-imm-int 0)))
(define (loongarch64-liw cgc rd immediate) ; XXX Implementation inspired from LLVM
  (let* ((val (loongarch64-imm-int-value immediate))
         (lo12 (- (bitwise-and (+ val #x800) #xfff) #x800))
         (hi52 (arithmetic-shift (+ val #x800) -12)))
    (if (and (>= val -2147483648) (<= val 2147483647))
        (let ((hi20 (bitwise-and hi52 #xfffff)))
          (if (not (zero? hi20))
              (loongarch64-lu12iw cgc rd (loongarch64-imm-int (arithmetic-shift hi20 12) 'U)))
          (if (or (not (zero? lo12)) (zero? hi20))
              (let ((instr (if (loongarch64-64bit-mode? cgc) loongarch64-addid loongarch64-addiw))
                    (rs (if (zero? hi20) (loongarch64-r0) rd)))
                (instr cgc rd rs (loongarch64-imm-int lo12)))))
        (begin
          (loongarch64-assert-64bit-mode cgc)
          (let* ((shamt (+ 12 (first-set-bit (asm-unsigned-lo64 hi52))))
                 (hi52 (arithmetic-shift
                         (asm-signed-lo64 (arithmetic-shift
                                            (arithmetic-shift hi52 (- (- shamt 12)))
                                            shamt))
                         (- shamt))))
            (loongarch64-liw cgc rd (loongarch64-imm-int hi52))
            (loongarch64-slliw cgc rd rd shamt)
            (if (not (zero? lo12))
                (loongarch64-addiw cgc rd rd (loongarch64-imm-int lo12))))))))
;;(define (loongarch64-liw cgc rd imm)
;;  (loongarch64-ori cgc rd (loongarch64-r0) (loongarch64-imm-int-value imm)))

(define (loongarch64-move cgc rd rs)
;;  (loongarch64-liw cgc rd imm))
   (loongarch64-or cgc rd rs (loongarch64-r0)))
(define (loongarch64-not cgc rd rs)
  (loongarch64-xori cgc rd rs (loongarch64-imm-int -1)))
(define (loongarch64-sltz cgc rd rj)        ;;sltz
  (loongarch64-slt cgc rd rj (loongarch64-r0)))

(define (loongarch64-bgt cgc rs rd offset)       ;;bgt
  (loongarch64-blt cgc rd rs offset))
(define (loongarch64-bgtu cgc rs rd offset)      ;;bgtu
  (loongarch64-bltu cgc rd rs offset))
(define (loongarch64-ble cgc rs rd offset)       ;;ble
  (loongarch64-bge cgc rd rs offset))
(define (loongarch64-bleu cgc rs rd offset)      ;;bleu
  (loongarch64-bgeu cgc rd rs offset))

(define (loongarch64-bltz cgc rs offset)           ;;bltz
  (loongarch64-blt cgc rs (loongarch64-r0) offset))
(define (loongarch64-bgtz cgc rs offset)           ;;bgtz
  (loongarch64-blt cgc (loongarch64-r0) rs offset))
(define (loongarch64-blez cgc rs offset)           ;;blez
  (loongarch64-bge cgc (loongarch64-r0) rs offset))
(define (loongarch64-bgez cgc rs offset)           ;;bgez
  (loongarch64-bge cgc rs (loongarch64-r0) offset))

;;(define (loongarch64-bl* cgc offset)
;;  (loongarch64-jirl cgc (loongarch64-r1) offset))
(define (loongarch64-jr cgc rs)
  (loongarch64-jirl cgc (loongarch64-r0) rs (loongarch64-imm-int 0)))
(define (loongarch64-jirl* cgc rs)
  (loongarch64-jirl cgc (loongarch64-r1) rs (loongarch64-imm-int 0)))
(define (loongarch64-ret cgc)
  (loongarch64-jirl cgc (loongarch64-r0) (loongarch64-r1) (loongarch64-imm-int 0)))
; XXX Repetition
;;(define (loongarch64-call cgc offset)
;;  (loongarch64-auipc cgc (loongarch64-r6)
;;               (loongarch64-imm-int (fxand (loongarch64-imm-int-value offset) #xfffff000) 'U)) ; XXX
;;  (loongarch64-jalr cgc (loongarch64-x1) (loongarch64-x6)
;;              (loongarch64-imm-int (fxand (loongarch64-imm-int-value offset) #xfff)))) ; XXX
;;(define (loongarch64-tail cgc offset)
;;  (loongarch64-auipc cgc (loongarch64-x6)
;;               (loongarch64-imm-int (fxand (loongarch64-imm-int-value offset) #xfffff000) 'U)) ; XXX
;;  (loongarch64-jalr cgc (loongarch64-x0) (loongarch64-x6)
;;              (loongarch64-imm-int (fxand (loongarch64-imm-int-value offset) #xfff)))) ; XXX

;; TODO Pseudoinstructions for accessing control and status registers

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 R-type instructions: ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
;;; ADDW, SUBW, SLLW, SRLW, SRAW.

(define (loongarch64-addw cgc rd rs1 rs2)        ;;addw
  (loongarch64-type-r cgc rd rs1 rs2 #x0000 #x00100))

(define (loongarch64-addd cgc rd rs1 rs2)        ;;addd
  (loongarch64-type-r cgc rd rs1 rs2 #x1000 #x00108))

(define (loongarch64-subw cgc rd rs1 rs2)        ;;subw
  (loongarch64-type-r cgc rd rs1 rs2 #x2000 #x00110))

(define (loongarch64-subd cgc rd rs1 rs2)        ;;subd
  (loongarch64-type-r cgc rd rs1 rs2 #x3000 #x00118))

(define (loongarch64-slt cgc rd rs1 rs2)         ;;slt
  (loongarch64-type-r cgc rd rs1 rs2 #x4000 #x00120))

(define (loongarch64-sltu cgc rd rs1 rs2)       ;;sltu
  (loongarch64-type-r cgc rd rs1 rs2 #x5000 #x00128))

(define (loongarch64-maskeqz cgc rd rs1 rs2)    ;;maskeqz
  (loongarch64-type-r cgc rd rs1 rs2 #x6000 #x00130))

(define (loongarch64-masknez cgc rd rs1 rs2)    ;;masknez
  (loongarch64-type-r cgc rd rs1 rs2 #x7000 #x00138))

(define (loongarch64-nor cgc rd rs1 rs2)        ;;nor
  (loongarch64-type-r cgc rd rs1 rs2 #x8000 #x00140))

(define (loongarch64-and cgc rd rs1 rs2)         ;;and
  (loongarch64-type-r cgc rd rs1 rs2 #x9000 #x00148))

(define (loongarch64-or cgc rd rs1 rs2)          ;;or
  (loongarch64-type-r cgc rd rs1 rs2 #xa000 #x00150))

(define (loongarch64-xor cgc rd rs1 rs2)         ;;xor
  (loongarch64-type-r cgc rd rs1 rs2 #xb000 #x00158))

(define (loongarch64-orn cgc rd rs1 rs2)         ;;orn
  (loongarch64-type-r cgc rd rs1 rs2 #xc000 #x00160))

(define (loongarch64-andn cgc rd rs1 rs2)        ;;andn
  (loongarch64-type-r cgc rd rs1 rs2 #xd000 #x00168))

(define (loongarch64-sllw cgc rd rs1 rs2)        ;;sllw
  (loongarch64-type-r cgc rd rs1 rs2 #xe000 #x00170))

(define (loongarch64-srlw cgc rd rs1 rs2)        ;;srlw
  (loongarch64-type-r cgc rd rs1 rs2 #xf000 #x00178))

(define (loongarch64-sraw cgc rd rs1 rs2)        ;;sraw
  (loongarch64-type-r cgc rd rs1 rs2 #x10000 #x00180))

(define (loongarch64-slld cgc rd rs1 rs2)        ;;slld
  (loongarch64-type-r cgc rd rs1 rs2 #x11000 #x00188))

(define (loongarch64-srld cgc rd rs1 rs2)        ;;srld
  (loongarch64-type-r cgc rd rs1 rs2 #x12000 #x00190))

(define (loongarch64-srad cgc rd rs1 rs2)        ;;srad
  (loongarch64-type-r cgc rd rs1 rs2 #x13000 #x00198))

;;(define (loongarch64-addw cgc rd rs1 rs2)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-r cgc rd rs1 rs2 #x0000 #x3b))

;;(define (loongarch64-subw cgc rd rs1 rs2)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-r cgc rd rs1 rs2 #x0000 #x3b #x40000000))

;;(define (loongarch64-sllw cgc rd rs1 rs2)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-r cgc rd rs1 rs2 #x1000 #x3b))

;;(define (loongarch64-srlw cgc rd rs1 rs2)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-r cgc rd rs1 rs2 #x5000 #x3b))

;;(define (loongarch64-sraw cgc rd rs1 rs2)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-r cgc rd rs1 rs2 #x5000 #x3b #x40000000))

(define (loongarch64-type-r cgc rd rs1 rs2 funct3 #!optional (opcode #x001) (funct7 #x0))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-reg? rs2))
          "invalid operands")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs2)
                    10)
		  (fxarithmetic-shift
                    (opcode)
		    15)))

  (if (codegen-context-listing-format cgc)
      (loongarch64-listing cgc
                     (list (vector-ref
                             ( #("addw" "addd" "subw" "subd" "slt" "sltu" "maskeqz" "masknez")
                               #("nor" "and" "or" "xor" "orn" "andn" "sllw" "srlw" "sraw" "slld" "srld" "srad"))
                             (fxarithmetic-shift funct3 -12))
                           )
                     rd
                     rs1
                     rs2)))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 I-type instructions: JALR, LB, LH, LW, LBU, LHU, ADDI, SLTI, SLTIU,
;;; XORI, ORI, ANDI, SLLI, SRLI, SRAI, LWU, LD, ADDIW, SLLIW, SRLIW, SRAIW.

;;(define (loongarch64-jirl cgc rd rs1 imm)
;;  (loongarch64-type-i cgc rd rs1 imm #x0000 #x4C)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "jirl" rd rs1 imm)))

(define (loongarch64-ldb cgc rd rs1 imm)            ;;ldb
  (loongarch64-type-i cgc rd rs1 imm #x0000 #x280))

(define (loongarch64-ldh cgc rd rs1 imm)            ;;ldh
  (loongarch64-type-i cgc rd rs1 imm #x1000 #x284))

(define (loongarch64-ldw cgc rd rs1 imm)
  (loongarch64-type-i cgc rd rs1 imm #x2000 #x288))

(define (loongarch64-ldd cgc rd rs1 imm)           ;;ldd
  (loongarch64-type-i cgc rd rs1 imm #x3000 #x28C))

(define (loongarch64-stb cgc rd rs1 imm)           ;;stb
  (loongarch64-type-i cgc rd rs1 imm #x4000 #x290))

(define (loongarch64-sth cgc rs1 rs2 imm)          ;;sth
  (loongarch64-type-i cgc rs1 rs2 imm #x5000 #x294))

(define (loongarch64-stw cgc rs1 rs2 imm)          ;;stw
  (loongarch64-type-i cgc rs1 rs2 imm #x6000 #x298))

(define (loongarch64-std cgc rs1 rs2 imm)          ;;std
  (loongarch64-assert-64bit-mode cgc)
  (loongarch64-type-i cgc rs1 rs2 imm #x7000 #x29c))

(define (loongarch64-ldbu cgc rd rs1 imm)          ;;ldbu
  (loongarch64-type-i cgc rd rs1 imm #x8000 #x2A0))

(define (loongarch64-ldhu cgc rd rs1 imm)          ;;ldhu
  (loongarch64-type-i cgc rd rs1 imm #x9000 #x2A4))

(define (loongarch64-ldwu cgc rd rs1 imm)          ;;ldwu
  (loongarch64-type-i cgc rd rs1 imm #xa000 #x2a8))

(define (loongarch64-slti cgc rd rs1 imm)        ;;slti
  (loongarch64-type-i cgc rd rs1 imm #x0000 #x020))

(define (loongarch64-sltui cgc rd rs1 imm)       ;;sltui
  (loongarch64-type-i cgc rd rs1 imm #x1000 #x024))

(define (loongarch64-addiw cgc rd rs1 imm)       ;;addiw
  (loongarch64-type-i cgc rd rs1 imm #x2000 #x028))

(define (loongarch64-addid cgc rd rs1 imm)       ;;addid
  (loongarch64-assert-64bit-mode cgc)
  (loongarch64-type-i cgc rd rs1 imm #x3000 #x02C))

(define (loongarch64-lu52id cgc rd rs1 imm)       ;;lu52id
  (loongarch64-assert-64bit-mode cgc)
  (loongarch64-type-i cgc rd rs1 imm #x4000 #x030))

(define (loongarch64-andi cgc rd rs1 imm)          ;;andi
  (loongarch64-type-i cgc rd rs1 imm #x5000 #x034))

(define (loongarch64-ori cgc rd rs1 imm)           ;;ori
  (loongarch64-type-i cgc rd rs1 imm #x6000 #x038))

(define (loongarch64-xori cgc rd rs1 imm)          ;;xori
  (loongarch64-type-i cgc rd rs1 imm #x7000 #x03c))

;;(define (loongarch64-addu16i cgc rd rs1 imm)       ;;addu16id
;;  (loongarch64-type-2i cgc rd rs1 imm #x0000 #x10)) 
;; imm = 20
(define (loongarch64-lu12iw cgc rd imm)
  (loongarch64-type-2i cgc rd imm #x0000 #x14))

(define (loongarch64-lu32id cgc rd imm)
  (loongarch64-type-2i cgc rd imm #x1000 #x16))

(define (loongarch64-pcaddi cgc rd imm)
  (loongarch64-type-2i cgc rd imm #x2000 #x18))

(define (loongarch64-pcalau12i cgc rd imm)
  (loongarch64-type-2i cgc rd imm #x3000 #x1A))

(define (loongarch64-pcaddu12i cgc rd imm)
  (loongarch64-type-2i cgc rd imm #x4000 #x1C))

(define (loongarch64-pcaddu18i cgc rd  imm)
  (loongarch64-type-2i cgc rd imm #x5000 #x1E))
;;imm = 14
(define (loongarch64-llw cgc rd rs1 imm)          ;;llw
  (loongarch64-type-3i cgc rd rs1 imm #x0000 #x20))

(define (loongarch64-scw cgc rd rs1 imm)          ;;scw
  (loongarch64-type-3i cgc rd rs1 imm #x1000 #x21))

(define (loongarch64-lld cgc rd rs1 imm)          ;;lld
  (loongarch64-type-3i cgc rd rs1 imm #x2000 #x22))

(define (loongarch64-scd cgc rd rs1 imm)          ;;scd
  (loongarch64-type-3i cgc rd rs1 imm #x3000 #x23))

(define (loongarch64-ldptrw cgc rd rs1 imm)       ;;ldptrw
  (loongarch64-type-3i cgc rd rs1 imm #x4000 #x24))

(define (loongarch64-stptrw cgc rd rs1 imm)       ;;stprtw
  (loongarch64-type-3i cgc rd rs1 imm #x5000 #x25))

(define (loongarch64-ldptrd cgc rd rs1 imm)       ;;ldptrd
  (loongarch64-type-3i cgc rd rs1 imm #x6000 #x26))

(define (loongarch64-stptrd cgc rd rs1 imm)       ;;stptrd
  (loongarch64-type-3i cgc rd rs1 imm #x7000 #x27))

;; opcode rd, rs1, shamt, shamt = 5or6
(define (loongarch64-slliw cgc rd rs1 shamt)      ;;slliw
  (assert (and (fx>= shamt 0) (fx<= shamt (5)))
          "improper shift amount")
  (loongarch64-type-4i cgc rd rs1 shamt #x1000 #x00408))

(define (loongarch64-srliw cgc rd rs1 shamt)      ;;srliw
  (assert (and (fx>= shamt 0) (fx<= shamt (5)))
          "improper shift amount")
  (loongarch64-type-4i cgc rd rs1 shamt #x5000 #x00448))

(define (loongarch64-sraiw cgc rd rs1 shamt)      ;;sraiw
  (assert (and (fx>= shamt 0) (fx<= shamt (5)))
          "improper shift amount")
  (loongarch64-type-4i cgc rd rs1 shamt #x5000 #x00488))

(define (loongarch64-sllid cgc rd rs1 shamt)      ;;sllid
  (assert (and (fx>= shamt 0) (fx<= shamt (6)))
          "improper shift amount")
  (loongarch64-type-5i cgc rd rs1 shamt #x1000 #x0041))

(define (loongarch64-srlid cgc rd rs1 shamt)      ;;srlid
  (assert (and (fx>= shamt 0) (fx<= shamt (6)))
          "improper shift amount")
  (loongarch64-type-5i cgc rd rs1 shamt #x5000 #x0045))

(define (loongarch64-sraid cgc rd rs1 shamt)      ;;sraid
  (assert (and (fx>= shamt 0) (fx<= shamt (6)))
          "improper shift amount")
  (loongarch64-type-5i cgc rd rs1 shamt #x5000 #x0049))

;;(define (loongarch64-ldwu cgc rd rs1 imm)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-i cgc rd rs1 imm #x6000 #x03))

;;(define (loongarch64-ldd cgc rd rs1 imm)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-i cgc rd rs1 imm #x3000 #x03))

;;(define (loongarch64-addiw cgc rd rs1 imm)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-i cgc rd rs1 imm #x0000 #x1b))

;;(define (loongarch64-slliw cgc rd rs1 shamt) ; XXX
;;  (loongarch64-assert-64bit-mode cgc)
;;  (assert (and (fx>= shamt 0) (fx< shamt 32))
;;          "improper shift amount")
;;  (loongarch64-type-i cgc rd rs1 (loongarch64-imm-int shamt) #x1000 #x1b))

;;(define (loongarch64-srliw cgc rd rs1 shamt) ; XXX
;;  (loongarch64-assert-64bit-mode cgc)
;;  (assert (and (fx>= shamt 0) (fx< shamt 32))
;;          "improper shift amount")
;;  (loongarch64-type-i cgc rd rs1 (loongarch64-imm-int shamt) #x5000 #x1b))

;;(define (loongarch64-sraiw cgc rd rs1 shamt) ; XXX
;;  (loongarch64-assert-64bit-mode cgc)
;;  (assert (and (fx>= shamt 0) (fx< shamt 32))
;;;          "improper shift amount")
;;  (loongarch64-type-i cgc rd rs1 (loongarch64-imm-int (fx+ #x400 shamt)) #x5000 #x1b))

;; opcode rd, imm; imm = 20
(define (loongarch64-type-2i cgc rd imm funct3 #!optional (opcode #x14))
  (assert (and (loongarch64-reg? rd)
	       (loongarch64-imm? imm))
	  "invalid operands")
  
  (assert (and (loongarch64-imm-int? imm)
	       (eq? (loongarch64-imm-int-type imm) 'I))
	  "incorrect immediate type")

  (asm-32-le cgc
	     (fx+ (fxarithmetic-shift
		    (loongarch64-reg-field rd)
		    0)
		  (fxarithmetic-shift
		    (loongarch64-imm->instr imm)
		    5)
		  (fxarithmetic-shift
		    (opcode)
		    25)))
  
 (if(codegen-context-listing-format cgc)
   (cond ((fx>= opcode #x14)
	 (loongarch64-listing cgc
			      (vector-ref
				#("lu12iw" "lu32id" "pcaddi" "pcalau12i" "pcaddu12i" "pcaddu18i")
				(fxarithmetic-shift funct3 -12))
			      rd
			      (loongarch64-imm-int-value imm)))
    )))

;; opcode rd, rj, imm;  imm = 14
(define (loongarch64-type-3i cgc rd rs1 imm funct3 #!optional (opcode #x28))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'I))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
		  (fxarithmetic-shift
                    (loongarch64-imm->instr imm)
		    10)
		  (fxarithmetic-shift
		    (opcode)
		   24)))

 (if(codegen-context-listing-format cgc)
   (cond ((fx>= opcode #x20)
   (loongarch64-listing cgc
      (vector-ref
	#("llw" "scw" "lld" "scd" "ldptrw" "stptrw" "ldptrd" "stptrd")
	(fxarithmetic-shift funct3 -12))
      rd
      rs1
      (loongarch64-imm-int-value imm))))
    ))

;; opcode rd, rj, imm;  imm = 5
(define (loongarch64-type-4i cgc rd rs1 imm funct3 #!optional (opcode #x00408))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'I))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
		  (fxarithmetic-shift
                    (loongarch64-imm->instr imm)
		    10)
		  (fxarithmetic-shift
		    (opcode)
		   15)))

 (if(codegen-context-listing-format cgc)
   (cond ((fx>= opcode #x00408)
   (loongarch64-listing cgc
      (vector-ref
	#("slliw" "srliw" "sraiw" "rotriw")
	(fxarithmetic-shift funct3 -12))
      rd
      rs1
      (loongarch64-imm-int-value imm))))
    ))

;; opcode rd, rj, imm;  imm = 6
(define (loongarch64-type-5i cgc rd rs1 imm funct3 #!optional (opcode #x0041))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'I))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
		  (fxarithmetic-shift
                    (loongarch64-imm->instr imm)
		    10)
		  (fxarithmetic-shift
		    (opcode)
		   16)))

 (if(codegen-context-listing-format cgc)
   (cond ((fx>= opcode #x0041)
   (loongarch64-listing cgc
      (vector-ref
	#("sllid" "srlid" "sraid" "rotrid")
	(fxarithmetic-shift funct3 -12))
      rd
      rs1
      (loongarch64-imm-int-value imm))))
    ))

;; opcode rd, rj, imm; imm=12
(define (loongarch64-type-i cgc rd rs1 imm funct3 #!optional (opcode #x28))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'I))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
		  (fxarithmetic-shift
                    (loongarch64-imm->instr imm)
		    10)
		  (fxarithmetic-shift
		    (opcode)
		   22)))

  (if (codegen-context-listing-format cgc)
      (cond ((fx>= opcode #x28)
             (loongarch64-listing cgc
                            (vector-ref
                              #("ldb" "ldh" "ldw" "ldd" "stb" "sth" "std" "ldbu" "ldhu" "ldwu")
                              (fxarithmetic-shift funct3 -12))
                            rd
                            (string-append (number->string (loongarch64-imm-int-value imm))
                                           "(" (loongarch64-register-name rs1) ")")))
            ((or (fx>= opcode #x020) )
             (loongarch64-listing cgc
                            (vector-ref
                                    (vector "slti" "sltui" "addiw" "addid" "lu52id" "andi" "ori" "xori")
                                    (fxarithmetic-shift funct3 -12))
                            rd
                            rs1
                           (loongarch64-imm-int-value imm))))
      ))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 S-type instructions: SB, SH, SW, SD.

;;(define (loongarch64-stb cgc rs1 rs2 imm)
;;  (loongarch64-type-s cgc rs1 rs2 imm #x0000))

;;(define (loongarch64-sth cgc rs1 rs2 imm)
;;  (loongarch64-type-s cgc rs1 rs2 imm #x1000))

;;(define (loongarch64-stw cgc rs1 rs2 imm)
;;  (loongarch64-type-s cgc rs1 rs2 imm #x2000))

;;(define (loongarch64-std cgc rs1 rs2 imm)
;;  (loongarch64-assert-64bit-mode cgc)
;;  (loongarch64-type-s cgc rs1 rs2 imm #x3000))

;;(define (loongarch64-type-s cgc rs1 rs2 imm funct3 #!optional (opcode #x290))

;;  (assert (and (loongarch64-reg? rs1)
;;               (loongarch64-reg? rs2)
;;               (loongarch64-imm? imm))
;;          "invalid operands")

;;  (assert (and (loongarch64-imm-int? imm)
;;               (eq? (loongarch64-imm-int-type imm) 'S))
;;          "incorrect immediate type")

;;  (asm-32-le cgc
;;             (fx- opcode
;;                  (fxarithmetic-shift
;;                    (loongarch64-reg-field rs1)
;;                    0)
;;                  (fxarithmetic-shift
;;                    (loongarch64-reg-field rs2)
;;                    5)
;;                  (loongarch64-imm->instr imm)))

;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc
;;                     (vector-ref
;;                       #("stb" "sth" "stw" "std")
;;                       (fxarithmetic-shift funct3 -12))
;;                     rs2
;;                     (string-append (number->string (loongarch64-imm-int-value imm))
;;                                    "(" (loongarch64-register-name rs1) ")"))))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 B-type instructions: BEQ, BNE, BLT, BGE, BLTU, BGEU.

(define (loongarch64-beqz cgc rs1 imm)           ;;beqz
  (loongarch64-type-2b cgc rs1 imm #x0000 #x40))

(define (loongarch64-bnez cgc rs1 imm)           ;;bnez
  (loongarch64-type-2b cgc rs1 imm #x1000 #x44))

(define (loongarch64-bceqz cgc rs1 imm)          ;;bceqz
  (loongarch64-type-2b cgc rs1 imm #x2000 #x48))

(define (loongarch64-bcnez cgc rs1 imm)          ;;bcnez
  (loongarch64-type-2b cgc rs1 imm #x3000 #x48))

(define (loongarch64-jirl cgc rd rs1 imm)         ;;jirl
  (loongarch64-type-3b cgc rd rs1 imm #x0000 #x4C))

(define (loongarch64-b cgc imm)                   ;;b
  (loongarch64-type-4b cgc imm #x0000 #x50))

(define (loongarch64-bl cgc imm)                  ;;bl
  (loongarch64-type-4b cgc imm #x1000 #x54))

(define (loongarch64-beq cgc rs1 rd imm)          ;;beq
  (loongarch64-type-b cgc rs1 rd imm #x0000 #x58))

(define (loongarch64-bne cgc rs1 rd imm)          ;;bne
  (loongarch64-type-b cgc rs1 rd imm #x1000 #x5c))

(define (loongarch64-blt cgc rs1 rd imm)          ;;blt
  (loongarch64-type-b cgc rs1 rd imm #x2000 #x60))

(define (loongarch64-bge cgc rs1 rd imm)          ;;bge
  (loongarch64-type-b cgc rs1 rd imm #x3000 #x64))

(define (loongarch64-bltu cgc rs1 rd imm)         ;;bltu
  (loongarch64-type-b cgc rs1 rd imm #x4000 #x68))

(define (loongarch64-bgeu cgc rs1 rd imm)         ;;bgeu
  (loongarch64-type-b cgc rs1 rd imm #x5000 #x6c))

;; opcode rj, rd, imm; imm = 16
(define (loongarch64-type-b cgc rs1 rd imm funct3 #!optional (opcode #x58))

  (define (label-dist self imm)
    (fx- (asm-label-pos (loongarch64-imm-lbl-label imm))
         (fx+ self (loongarch64-imm-lbl-offset imm))))

  (define (instr-code imm)
    (asm-32-le cgc
               (fx+ (fxarithmetic-shift
                      (loongarch64-reg-field rs1)
                      5)
                    (fxarithmetic-shift
                      (loongarch64-reg-field rd)
                      0)
		    (fxarithmetic-shift
                      (loongarch64-imm->instr imm)
		      10)
		    (fxarithmetic-shift
		      (opcode)
		      26))))

  (assert (and (loongarch64-reg? rs1)
               (loongarch64-reg? rd)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (or (loongarch64-imm-lbl? imm)
              (and (loongarch64-imm-int? imm)
                   (eq? (loongarch64-imm-int-type imm) 'I)))
          "incorrect immediate type")

  (if (loongarch64-imm-int? imm)
      (instr-code imm)
      (asm-at-assembly cgc
                       (lambda (cgc self)
                         4) ; XXX
                       (lambda (cgc self)
                         (let* ((dist (label-dist self imm))
                                (imm (loongarch64-imm-int dist 'B)))

                           (assert (and (fx>= dist -65536) (fx<= dist 65536))
                                   "branch label too far")

                           (instr-code imm)))))

  (if (codegen-context-listing-format cgc)
       (cond ((fx>= opcode #x58)
      (loongarch64-listing cgc
                     (vector-ref
                       #("beq" "bne" "blt" "bge" "bltu" "bgeu")
                       (fxarithmetic-shift funct3 -12))
                     rs1
                     rd
                     imm)))))

;;opcode cj, offs; offs = 20
(define (loongarch64-type-2b cgc rs1 imm funct3 #!optional (opcode #x40))

  (define (label-dist self imm)
    (fx- (asm-label-pos (loongarch64-imm-lbl-label imm))
         (fx+ self (loongarch64-imm-lbl-offset imm))))

  (define (instr-code imm)
    (asm-32-le cgc
               (fx+ (fxarithmetic-shift
                      (loongarch64-reg-field rs1)
                      5)
		    (fxarithmetic-shift 
                      ((fxarithmetic-shift-right
                        (loongarch64-imm->instr imm)
                        15))
		      0)
		    (fxarithmetic-shift
                      (fxarithmetic-shift-right
                        (fxarithmetic-shift-left
                          (loongarch64-imm->instr imm)
		          5)
		        5)
		     10)
		    (fxarithmetic-shift
		      (opcode)
		      26))))

  (assert (and (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (or (loongarch64-imm-lbl? imm)
              (and (loongarch64-imm-int? imm)
                   (eq? (loongarch64-imm-int-type imm) 'I)))
          "incorrect immediate type")

    (if (loongarch64-imm-int? imm)
      (instr-code imm)
      (asm-at-assembly cgc
                       (lambda (cgc self)
                         4) ; XXX
                       (lambda (cgc self)
                         (let* ((dist (label-dist self imm))
                                (imm (loongarch64-imm-int dist 'B))) ; XXX

                           (assert (and (fx>= dist -1048576) (fx<= dist 1048576))
                                   "branch label too far")

                           (instr-code imm)))))

      (if (codegen-context-listing-format cgc)
        (cond ((fx>= opcode #x40)
          (loongarch64-listing cgc
                     (vector-ref
                       #("beqz" "bnez" "bceqz" "bcnez")
                       (fxarithmetic-shift funct3 -12))
                     rs1
                     imm))))
 )

(define (loongarch64-type-3b cgc rd rs1 imm funct3 #!optional (opcode #x4C))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-reg? rs1)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'I))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx+ (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (fxarithmetic-shift
                    (loongarch64-reg-field rs1)
                    5)
		  (fxarithmetic-shift
                    (loongarch64-imm->instr imm)
		    10)
		  (fxarithmetic-shift
		    (opcode)
		    26)))

      (if (codegen-context-listing-format cgc)
        (cond ((fx>= opcode #x4C)
          (loongarch64-listing cgc
                     (vector-ref
                       #("jirl")
                       (fxarithmetic-shift funct3 -12))
                     rd
		     rs1
                     imm))))

)

;;opcode offs; offs = 26
(define (loongarch64-type-4b cgc imm funct3 #!optional (opcode #x50))

  (define (label-dist self imm)
    (fx- (asm-label-pos (loongarch64-imm-lbl-label imm))
         (fx+ self (loongarch64-imm-lbl-offset imm))))

  (define (instr-code imm)
    (asm-32-le cgc
               (fx+  (fxarithmetic-shift
		       (fxarithmetic-shift-right
		         (loongarch64-imm->instr imm)
		         16)
		      0)
		     (fxarithmetic-shift
		       (fxarithmetic-shift-right
		         (fxarithmetic-shift-left
		           (loongarch64-imm->instr imm)
		           10)
		       10)
		     10)
		     (fxarithmetic-shift
		       (opcode)
		       26))))

  (assert (or (loongarch64-imm-lbl? imm)
              (and (loongarch64-imm-int? imm)
                   (eq? (loongarch64-imm-int-type imm) 'I)))
          "incorrect immediate type")

    (if (loongarch64-imm-int? imm)
      (instr-code imm)
      (asm-at-assembly cgc
                       (lambda (cgc self)
                         4) ; XXX
                       (lambda (cgc self)
                         (let* ((dist (label-dist self imm))
                                (imm (loongarch64-imm-int dist 'B))) ; XXX

                           (assert (and (fx>= dist -67108864) (fx<= dist 67108864))
                                   "branch label too far")

                           (instr-code imm)))))

  (if (codegen-context-listing-format cgc)
      (loongarch64-listing cgc
                     (vector-ref
                       #("b" "bl")
                       (fxarithmetic-shift funct3 -12))
                     imm)))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 U-type instructions: LUI, AUIPC.

;;(define (loongarch64-lu12iw cgc rd imm)
;;  (loongarch64-type-u cgc rd imm #x14)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "lu12iw" rd imm)))

;;(define (loongarch64-pcaddu12i cgc rd imm)
;;  (loongarch64-type-u cgc rd imm #x1A)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "pcaddu12i" rd imm)))

(define (loongarch64-type-u cgc rd imm opcode)

  (assert (and (loongarch64-reg? rd)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (and (loongarch64-imm-int? imm)
               (eq? (loongarch64-imm-int-type imm) 'U))
          "incorrect immediate type")

  (asm-32-le cgc
             (fx- opcode
                  (fxarithmetic-shift
                    (loongarch64-reg-field rd)
                    0)
                  (loongarch64-imm->instr imm))))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 J-type instructions: JAL.

;;(define (loongarch64-jal cgc rd imm)
;;  (loongarch64-type-j cgc rd imm #x6f)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "jal" rd imm)))

(define (loongarch64-type-j cgc rd imm opcode)

  (define (label-dist self imm)
    (fx- (asm-label-pos (loongarch64-imm-lbl-label imm))
         (fx+ self (loongarch64-imm-lbl-offset imm))))

  (define (instr-code imm)
    (asm-32-le cgc
               (fx- opcode
                    (fxarithmetic-shift
                      (loongarch64-reg-field rd)
                      0)
                    (loongarch64-imm->instr imm))))

  (assert (and (loongarch64-reg? rd)
               (loongarch64-imm? imm))
          "invalid operands")

  (assert (or (loongarch64-imm-lbl? imm)
              (and (loongarch64-imm-int? imm)
                   (eq? (loongarch64-imm-int-type imm) 'J)))
          "incorrect immediate type")

  (if (loongarch64-imm-int? imm)
      (instr-code imm)
      (asm-at-assembly cgc
                       (lambda (cgc self)
                         4) ; XXX
                       (lambda (cgc self)
                         (let* ((dist (label-dist self imm))
                                (imm (loongarch64-imm-int dist 'J))) ; XXX

                           (assert (and (fx>= dist -1048576) (fx<= dist 1048574))
                                   "branch label too far")

                           (instr-code imm))))))

;;;----------------------------------------------------------------------------

;;; LOONGARCH64 instructions: FENCE, FENCE.I, ECALL, EBREAK.

;;(define (loongarch64-fence cgc #!optional (pred #b1111) (succ #b1111)) ; XXX

;;  (define (print-iorw val)
;;    (string-append (if (fxbit-set? 3 val) "i" "")
;;                   (if (fxbit-set? 2 val) "o" "")
;;                   (if (fxbit-set? 1 val) "r" "")
;;                   (if (fxbit-set? 0 val) "w" "")))

;;  (assert (and (fx> pred 0) (fx< pred 16))
;;          "improper predecessor value")
;;  (assert (and (fx> succ 0) (fx< succ 16))
;;          "improper successor value")

;;  (asm-32-le cgc (fx+ #xf
;;                      (fxarithmetic-shift succ 20)
;;                      (fxarithmetic-shift pred 24)))

;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "fence" (print-iorw pred) (print-iorw succ)))

;;(define (loongarch64-fence.i cgc)
;;  (asm-32-le cgc #x100f)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "fence.i")))

;;(define (loongarch64-ecall cgc)
;;  (asm-32-le cgc #x73)
;; (if (codegen-context-listing-format cgc)
;;     (loongarch64-listing cgc "ecall")))

;;(define (loongarch64-ebreak cgc)
;;  (asm-32-le cgc #x100073)
;;  (if (codegen-context-listing-format cgc)
;;      (loongarch64-listing cgc "ebreak")))

;;;----------------------------------------------------------------------------

;;; TODO LOONGARCH64 CSR instructions: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI.

;;;============================================================================
