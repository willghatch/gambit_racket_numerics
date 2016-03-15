#lang racket

(require "bignum_config.rkt")

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
         @@bignum.adigit-width)

;; The bignum representation is a struct-wrapped vector of adigits, represented by racket numbers

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
  (error 'unimplemented)) ;; Incomplete
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
  