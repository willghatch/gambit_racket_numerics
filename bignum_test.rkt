#lang racket
(require "bignum.rkt"
         "constants.rkt"
         rackunit)

(define adigit-modulus (expt 2 @@bignum.adigit-width))
(define-syntax-rule (bignumv args ...) (bignum (vector args ...)))

#;(check-equal? (from-rktnum 0) 0)
#;(check-equal? (from-rktnum -1) (vector (- (expt 2 64) 1)))
#;(check-equal? (from-rktnum max-fixnum) max-fixnum)
#;(check-equal? (from-rktnum min-fixnum) min-fixnum)
#;(check-equal? (from-rktnum (+ max-fixnum 1))
              (vector (+ max-fixnum 1)))
#;(check-equal? (from-rktnum (- min-fixnum 1))
              (vector (modulo (+ (- min-fixnum 1) adigit-modulus) adigit-modulus)))
#;(check-equal? (from-rktnum (- adigit-modulus 1)) (vector (- adigit-modulus 1) 0))

(define a (bignumv (- (expt 2 63) 1) (- (expt 2 63) 2)))
(define b (bignumv (- (expt 2 63) 2) (- (expt 2 63) 1)))

(check-false (@@bignum.adigit-< a b 0))
(check-true (@@bignum.adigit-< a b 1))

(define c (bignumv (- adigit-modulus 1)))

(check-equal? (@@bignum.make 1 c #f) (bignumv (- (expt 2 64) 1)))
(check-equal? (@@bignum.make 2 c #f) (bignumv (- (expt 2 64) 1) 0))
(check-equal? (@@bignum.make 3 c #f) (bignumv (- (expt 2 64) 1) 0 0))
(check-equal? (@@bignum.make 1 c #t) (bignumv 0))
(check-equal? (@@bignum.make 2 c #t) (bignumv 0 (- (expt 2 64) 1)))
(check-equal? (@@bignum.make 3 c #t) (bignumv 0 (- (expt 2 64) 1) (- (expt 2 64) 1)))

(let ((a (bignumv (expt 2 62) (expt 2 62)))
      (b (bignumv (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-add! a 0 b 0 0) 0)
  (check-equal? a (bignumv (expt 2 63) (expt 2 62))))

(let ((a (bignumv (expt 2 62) (expt 2 62)))
      (b (bignumv (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 1) 0)
  (check-equal? a (bignumv (expt 2 62) (+ (expt 2 63) 1))))

(let ((a (bignumv (expt 2 62) (expt 2 63)))
      (b (bignumv (expt 2 62) (expt 2 63))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 0) 1)
  (check-equal? a (bignumv (expt 2 62) 0)))

(let ((a (bignumv (expt 2 62) (expt 2 63)))
      (b (bignumv (expt 2 62) (- (expt 2 63) 1))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 0) 0)
  (check-equal? a (bignumv (expt 2 62) (- (expt 2 64) 1))))

(let ((a (bignumv (expt 2 62) (expt 2 63)))
      (b (bignumv (expt 2 62) (- (expt 2 63) 1))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 1) 1)
  (check-equal? a (bignumv (expt 2 62) 0)))

(let ((a (bignumv (expt 2 62) (expt 2 62)))
      (b (bignumv (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-sub! a 0 b 0 0) 0)
  (check-equal? a (bignumv 0 (expt 2 62))))

(let ((a (bignumv (expt 2 62) (expt 2 62)))
      (b (bignumv (expt 2 63) (expt 2 62))))
  (check-equal? (@@bignum.adigit-sub! a 0 b 0 0) 1)
  (check-equal? a (bignumv (* 3 (expt 2 62)) (expt 2 62))))

(check-equal? @@bignum.fdigit-width 8)
(check-equal? @@bignum.mdigit-width 16)
(check-equal? @@bignum.adigit-width 64)
