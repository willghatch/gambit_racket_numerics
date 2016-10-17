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
;; Note that the most significant adigit is at the highest index.

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
  (define y (make-vector k (if (and (bignum? x) (@@bignum.negative? x)) adigit-ones 0)))
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
  (set-bignum-adigits! x (vector-take (adigits x) n))
  ;; return the bignum rather than void
  x)
(define (@@bignum.adigit-copy! x i y j)
  (vector-set! (adigits x) i (vector-ref (adigits y) j)))
(define (@@bignum.adigit-cat! x i hi j lo k divider)
  (define hi-mask (* adigit-ones (expt 2 divider)))
  (define lo-mask (quotient adigit-ones (expt 2 (- @@bignum.adigit-width divider))))
  (vector-set! (adigits x) i
               (+ (bitwise-and hi-mask (vector-ref (adigits hi) j))
                  (bitwise-and lo-mask (vector-ref (adigits lo) k))))
  x)
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

;; The following bignum.mdigit-whatever functions are basically lifted straight
;; from <gambit-root>/gsc/_t-univ-4.scm

#;(define (@@bignum.mdigit-mul! x i y j multiplier carry)
    (define inttemp1 (+ (+ (array-index (bignum-digits x) (fixnum-unbox i))
                           (* (array-index (bignum-digits y) (fixnum-unbox j))
                              (fixnum-unbox multiplier)))
                        (fixnum-unbox carry)))
    (vector-set! (bignum-digits x) (fixnum-unbox i)
                 (cast 'bigdigit (bitand (rts-field-use 'inttemp1) (int 16383))))
    ;; return
    (fixnum-box (>> (rts-field-use 'inttemp1) (int 14))))
(define (@@bignum.mdigit-mul! product p-ix bfactor bf-ix fixnum-factor carry)
  (define tmp (+ (@@bignum.mdigit-ref product p-ix)
                 (* (@@bignum.mdigit-ref bfactor bf-ix)
                    fixnum-factor)
                 carry))
  (@@bignum.mdigit-set! product p-ix (bitwise-and tmp mdigit-ones))
  ;; return new carry
  (arithmetic-shift tmp (- mdigit-width)))

(define (@@bignum.mdigit-div! x i y j quotient borrow)
  (define inttemp1
    (+ (- (@@bignum.mdigit-ref x i)
          (* (@@bignum.mdigit-ref y j) quotient))
       borrow))
  (@@bignum.mdigit-set! x i (bitwise-and inttemp1 mdigit-ones))
  (define temp-shifted (arithmetic-shift inttemp1 (- mdigit-width)))
  (if (> temp-shifted 0)
      (- temp-shifted mdigit-ones)
      temp-shifted))
(define (@@bignum.mdigit-quotient u j v_n-1)
  (quotient
   (+ (arithmetic-shift (@@bignum.mdigit-ref u j)
                        mdigit-width)
      (@@bignum.mdigit-ref u
                  (sub1 j)))
   v_n-1))
(define (@@bignum.mdigit-remainder arg1 arg2 arg3 arg4)
  (- (+ (arithmetic-shift (@@bignum.mdigit-ref arg1
                                      arg2)
                          mdigit-width)
        (@@bignum.mdigit-ref arg1
                    (sub1 arg2)))
     (* arg3
        arg4)))
(define (@@bignum.mdigit-test? arg1 arg2 arg3 arg4)
  (> (* arg1 arg2)
     (+ (arithmetic-shift arg3 mdigit-width)
        arg4)))

(define @@bignum.fdigit-width fdigit-width)
(define @@bignum.mdigit-width mdigit-width)
(define @@bignum.adigit-width adigit-width)

(define most-significant-adigit-bit (expt 2 (sub1 @@bignum.adigit-width)))
(define adigit-modulus (expt 2 @@bignum.adigit-width))
(define mdigit-modulus (expt 2 @@bignum.mdigit-width))
(define adigit-ones (- adigit-modulus 1))
(define mdigit-ones (- mdigit-modulus 1))
(define mdigits-in-adigit (/ @@bignum.adigit-width @@bignum.mdigit-width))

