#lang racket
(require racket/fixnum
         "constants.rkt"
         "bignum_config.rkt")
(provide bignum
         bignum-adigits
         bignum?
         @@bignum?
         @@bignum.negative?
         @@bignum.adigit-length
         @@bignum.mdigit-length
         @@bignum.adigit-<
         @@fixnum->bignum
         @@bignum.make
         @@bignum.adigit-add!
         @@bignum.adigit-sub!
         @@bignum.adigit-inc!
         @@bignum.adigit-dec!
         @@bignum.adigit-shrink!
         @@bignum.adigit-copy!
         @@bignum.adigit-cat!
         @@bignum.adigit-zero?
         @@bignum.adigit-negative?
         @@bignum.adigit-ones?
         @@bignum.mdigit-ref
         @@bignum.mdigit-set!
         @@bignum.mdigit-mul!
         @@bignum.mdigit-div!
         @@bignum.mdigit-quotient
         @@bignum.mdigit-remainder
         @@bignum.mdigit-test?
         @@bignum.fdigit-width
         @@bignum.mdigit-width
         @@bignum.adigit-width
         bignum.-)

#|
Bignum Representation
adigit := value between 0 and 2 ^ 64 - 1
mdigit := value between 0 and 2 ^ 16 - 1
fdigit := value between 0 and 2 ^ 8 - 1
Probably the representation of bignums uses 64 bit values for adigits.
As a result mdigits come in multiples of 4
The representation is a normal two's complement number
Our represention is a vector of adigits, represented a racket number
|#

(struct bignum (adigits) #:mutable #:transparent)
(define-syntax-rule (adigits x) (bignum-adigits x))

(define @@bignum? bignum?)

(define (@@bignum.negative? x)
  (not (zero? (bitwise-and most-significant-adigit-bit
                           (vector-ref (adigits x) (- (vector-length (adigits x)) 1))))))
(define (@@bignum.adigit-length x) (vector-length (adigits x)))
(define (@@bignum.mdigit-length x) (* (@@bignum.adigit-length x) mdigits-in-adigit))
(define (@@bignum.adigit-< x y i)
  (< (vector-ref (adigits x) i) (vector-ref (adigits y) i)))
(define (@@fixnum->bignum x) (bignum (vector (modulo (+ x adigit-modulus) adigit-modulus))))
(define (@@bignum.make k x complement?)
  (define y (make-vector k 0))
  (for ((adigit (in-vector (adigits (or x (bignum (vector))))))
        (i (in-range k)))
    (vector-set! y i adigit))
  (when complement?
    (for ((i (in-range k)))
      (vector-set! y i (bitwise-xor adigit-ones (vector-ref y i)))))
  (bignum y))
(define (@@bignum.adigit-add! x i y j carry)
  (define sum (+ (vector-ref (adigits x) i) (vector-ref (adigits y) j) carry))
  (vector-set! (adigits x) i (modulo sum adigit-modulus))
  (quotient sum adigit-modulus))
(define (@@bignum.adigit-sub! x i y j carry)
  (define diff (- (vector-ref (adigits x) i) (vector-ref (adigits y) j) carry))
  (vector-set! (adigits x) i (modulo (+ diff adigit-modulus) adigit-modulus))
  (if (< diff 0) 1 0))
(define (@@bignum.adigit-inc! x i)
  (define sum (add1 (vector-ref (adigits x) i)))
  (vector-set! (adigits x) i (modulo sum adigit-modulus))
  (quotient sum adigit-modulus))
(define (@@bignum.adigit-dec! x i)
  (define diff (sub1 (vector-ref (adigits x) i)))
  (vector-set! (adigits x) i (modulo (+ diff adigit-modulus) adigit-modulus))
  (if (< diff 0) 1 0))
(define (@@bignum.adigit-shrink! x n)
  (set-bignum-adigits! x (vector-take (adigits x) n)))
(define (@@bignum.adigit-copy! x i y j)
  (vector-set! (adigits x) i (vector-ref (adigits y) j)))
(define (@@bignum.adigit-cat! x i hi j lo k divider)
  (displayln x)
  (displayln i)
  (displayln hi)
  (displayln j)
  (displayln lo)
  (displayln k)
  (displayln divider)
  (error '@@bignum.adigit-cat!)) ;; Incomplete
(define (@@bignum.adigit-zero? x i)
  (zero? (vector-ref (adigits x) i)))
(define (@@bignum.adigit-ones? x i)
  (= (vector-ref (adigits x) i) adigit-ones))
(define (@@bignum.adigit-negative? x i)
  (not (zero? (bitwise-and most-significant-adigit-bit (vector-ref (adigits x) i)))))
(define (@@bignum.mdigit-ref x i)
  (define adigit-index (quotient i mdigits-in-adigit))
  (define mdigit-subindex (modulo i mdigits-in-adigit))
  (modulo (quotient (vector-ref (adigits x) adigit-index)
                    (expt mdigit-modulus mdigit-subindex)) mdigit-modulus))
(define (@@bignum.mdigit-set! x i mdigit)
  (define adigit-index (quotient i mdigits-in-adigit))
  (define mdigit-subindex (modulo i mdigits-in-adigit))
  (define mask (* mdigit-ones (expt mdigit-modulus mdigit-subindex)))
  (define old-adigit (vector-ref (adigits x) adigit-index))
  (define new-adigit
    (bitwise-ior
      (bitwise-and old-adigit (bitwise-not mask))
      (* mdigit (expt mdigit-modulus mdigit-subindex))))
  (vector-set! (adigits x) adigit-index new-adigit))
(define (@@bignum.mdigit-mul! x i y j multiplier carry)
  (error 'unimplemented)) ;; Incomplete
(define (@@bignum.mdigit-div! x i y j quotient borrow)
  (error 'unimplemented)) ;; Incomplete
; (top-2*mdigit-width-bits-of-v
;   (@@bignum.arithmetic-shift-into! v (@@fx- (@@fx* @@bignum.mdigit-width 2) v-bits) temp))
; (v_n-1
;   (@@bignum.mdigit-ref top-2*mdigit-width-bits-of-v 1))
; (v_n-2
;   (@@bignum.mdigit-ref top-2*mdigit-width-bits-of-v 0))
(define (@@bignum.mdigit-quotient u j v_n-1)
  (error 'unimplemented)) ;; Incomplete
(define (@@bignum.mdigit-remainder u j v_n-1 q-hat)
  (error 'unimplemented)) ;; Incomplete
(define (@@bignum.mdigit-test? q-hat v_n-2 r-hat u_j-2)
  (error 'unimplemented)) ;; Incomplete
(define @@bignum.fdigit-width fdigit-width)
(define @@bignum.mdigit-width mdigit-width)
(define @@bignum.adigit-width adigit-width)

(define most-significant-adigit-bit (expt 2 @@bignum.adigit-width))
(define adigit-modulus (expt 2 @@bignum.adigit-width))
(define mdigit-modulus (expt 2 @@bignum.mdigit-width))
(define adigit-ones (- adigit-modulus 1))
(define mdigit-ones (- mdigit-modulus 1))
(define mdigits-in-adigit (/ @@bignum.adigit-width @@bignum.mdigit-width))

(define-syntax-rule (fixnum-bounds x)
  (if (or (< x min-fixnum) (> x max-fixnum)) #f x))
(define-syntax-rule (fixnum-fail exps ...)
  (with-handlers ((exn:fail:contract:non-fixnum-result? (lambda (e) #f)))
    exps ...))

(define @@fx< fx<)
(define @@fx+ fx+)
(define @@fx+? (lambda (x y) (fixnum-fail (fixnum-bounds (fx+ x y)))))
(define @@fx* fx*)
(define @@fx*? (lambda (x y) (fixnum-fail (fixnum-bounds (fx* x y)))))
(define @@fx- fx-)
(define @@fx= fx=)
(define @@eq? eq?)
(define @@fxzero? (lambda (x) (fx= x 0)))
(define @@not not)
(define @@fxarithmetic-shift-left fxlshift)
(define-syntax-rule (@@declare args ...) (void))
(define-syntax-rule (define-prim args ...) (define args ...))

(define @@bignum.mdigit-base
  (@@fxarithmetic-shift-left 1 @@bignum.mdigit-width))
(define @@bignum.mdigit-base-minus-1
  (@@fx- @@bignum.mdigit-base 1))

(define (@@bignum.- x y)

  ;; x is an unnormalized bignum, y is an unnormalized bignum

  (let ((x-length (@@bignum.adigit-length x))
        (y-length (@@bignum.adigit-length y)))
    (if (@@fx< x-length y-length)

        (let* ((result-length
                (@@fx+ y-length
                       (if (@@eq? (@@bignum.negative? x)
                                  (@@bignum.negative? y))
                           0
                           1)))
               (result
                (@@bignum.make result-length y #t)))

          (@@declare (not interrupts-enabled))

          (let loop1 ((i 0)
                      (carry 1))
            (if (@@fx< i x-length)
                (loop1 (@@fx+ i 1)
                       (@@bignum.adigit-add! result i x i carry))
                (@@bignum.propagate-carry-and-normalize!
                 result
                 result-length
                 x-length
                 (@@bignum.negative? x)
                 (@@fxzero? carry)))))

        (let* ((result-length
                (@@fx+ x-length
                       (if (@@eq? (@@bignum.negative? x)
                                  (@@bignum.negative? y))
                           0
                           1)))
               (result
                (@@bignum.make result-length x #f)))

          (@@declare (not interrupts-enabled))

          (let loop2 ((i 0)
                      (borrow 0))
            (if (@@fx< i y-length)
                (loop2 (@@fx+ i 1)
                       (@@bignum.adigit-sub! result i y i borrow))
                (@@bignum.propagate-carry-and-normalize!
                 result
                 result-length
                 y-length
                 (@@not (@@bignum.negative? y))
                 (@@not (@@fxzero? borrow)))))))))

(define-prim (@@bignum.propagate-carry-and-normalize!
              result
              result-length
              i
              borrow?
              propagate?)

  (@@declare (not interrupts-enabled))

  (if (@@eq? borrow? propagate?)
      (if borrow?

          (let loop1 ((i i)
                      (borrow 1))
            (if (and (@@not (@@fxzero? borrow))
                     (@@fx< i result-length))
                (loop1 (@@fx+ i 1)
                       (@@bignum.adigit-dec! result i))
                (@@bignum.normalize! result)))

          (let loop2 ((i i)
                      (carry 1))
            (if (and (@@not (@@fxzero? carry))
                     (@@fx< i result-length))
                (loop2 (@@fx+ i 1)
                       (@@bignum.adigit-inc! result i))
                (@@bignum.normalize! result))))

      (@@bignum.normalize! result)))

(define-prim (@@bignum->fixnum? bn)
  (let* ((i
          (@@fx- (@@bignum.mdigit-length bn) 1))
         (n
          (@@bignum.mdigit-ref bn i))
         (bias
          (if (@@fx< (@@fx* 2 n) @@bignum.mdigit-base)
              0
              @@bignum.mdigit-base-minus-1)))
    (let loop ((n (@@fx- n bias))
               (i (@@fx- i 1)))
      (if (@@fx< i 0)
          (if (@@fx= 0 bias)
              n
              (@@fx+? n -1))
          (let ((n1 (@@fx*? n @@bignum.mdigit-base)))
            (and n1
                 (let ((n2 (@@fx+? n1 (@@fx- (@@bignum.mdigit-ref bn i) bias))))
                   (and n2
                        (loop n2
                              (@@fx- i 1))))))))))

(define-prim (@@bignum.normalize! result)
  (@@declare (not interrupts-enabled))
  (or (@@bignum->fixnum? result)

      (let ((n (@@fx- (@@bignum.adigit-length result) 1)))
        (cond ((@@bignum.adigit-zero? result n)
               (let loop1 ((i (@@fx- n 1)))
                 (cond ((@@fx< i 0)
                        0)
                       ((@@bignum.adigit-zero? result i)
                        (loop1 (@@fx- i 1)))
                       ((@@bignum.adigit-negative? result i)
                        (@@bignum.adigit-shrink! result (@@fx+ i 2)))
                       (else
                        (@@bignum.adigit-shrink! result (@@fx+ i 1))))))

              ((@@bignum.adigit-ones? result n)
               (let loop2 ((i (@@fx- n 1)))
                 (cond ((@@fx< i 0)
                        -1)
                       ((@@bignum.adigit-ones? result i)
                        (loop2 (@@fx- i 1)))
                       ((@@not (@@bignum.adigit-negative? result i))
                        (@@bignum.adigit-shrink! result (@@fx+ i 2)))
                       (else
                        (@@bignum.adigit-shrink! result (@@fx+ i 1))))))

              (else
               result)))))

(define bignum.- @@bignum.-)
  