#lang racket
(require "bignum.rkt"
         "translate.rkt"
         "constants.rkt"
         rackunit)

(define adigit-modulus (expt 2 @@bignum.adigit-width))

(check-equal? (from-rktnum 0) 0)
(check-equal? (from-rktnum -1) (vector (- (expt 2 64) 1)))
(check-equal? (from-rktnum max-fixnum) max-fixnum)
(check-equal? (from-rktnum min-fixnum) min-fixnum)
(check-equal? (from-rktnum (+ max-fixnum 1))
              (vector (+ max-fixnum 1)))
(check-equal? (from-rktnum (- min-fixnum 1))
              (vector (modulo (+ (- min-fixnum 1) adigit-modulus) adigit-modulus)))
(check-equal? (from-rktnum (- adigit-modulus 1)) (vector (- adigit-modulus 1) 0))

(define a (from-rktnum (+ (- (expt 2 63) 1) (* (expt 2 64) (- (expt 2 63) 2)))))
(define b (from-rktnum (+ (- (expt 2 63) 2) (* (expt 2 64) (- (expt 2 63) 1)))))

(check-false (@@bignum.adigit-< a b 0))
(check-true (@@bignum.adigit-< a b 1))

(define c (from-rktnum (- adigit-modulus 1)))

(check-equal? (@@bignum.make 1 c #f) (vector (- (expt 2 64) 1)))
(check-equal? (@@bignum.make 2 c #f) (vector (- (expt 2 64) 1) 0))
(check-equal? (@@bignum.make 3 c #f) (vector (- (expt 2 64) 1) 0 0))
(check-equal? (@@bignum.make 1 c #t) (vector 0))
(check-equal? (@@bignum.make 2 c #t) (vector 0 (- (expt 2 64) 1)))
(check-equal? (@@bignum.make 3 c #t) (vector 0 (- (expt 2 64) 1) (- (expt 2 64) 1)))

(let ((a (vector (expt 2 62) (expt 2 62)))
      (b (vector (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-add! a 0 b 0 0) 0)
  (check-equal? a (vector (expt 2 63) (expt 2 62))))

(let ((a (vector (expt 2 62) (expt 2 62)))
      (b (vector (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 0) 0)
  (check-equal? a (vector (expt 2 62) (+ (expt 2 63) 1))))

(let ((a (vector (expt 2 62) (expt 2 63)))
      (b (vector (expt 2 62) (expt 2 63))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 0) 1)
  (check-equal? a (vector (expt 2 62) 0)))

(let ((a (vector (expt 2 62) (expt 2 63)))
      (b (vector (expt 2 62) (- (expt 2 63) 1))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 0) 0)
  (check-equal? a (vector (expt 2 62) (- (expt 2 64) 1))))

(let ((a (vector (expt 2 62) (expt 2 63)))
      (b (vector (expt 2 62) (- (expt 2 63) 1))))
  (check-equal? (@@bignum.adigit-add! a 1 b 1 1) 1)
  (check-equal? a (vector (expt 2 62) 0)))

(let ((a (vector (expt 2 62) (expt 2 62)))
      (b (vector (expt 2 62) (expt 2 62))))
  (check-equal? (@@bignum.adigit-sub! a 0 b 0 0) 0)
  (check-equal? a (vector 0 (expt 2 62))))

(let ((a (vector (expt 2 62) (expt 2 62)))
      (b (vector (expt 2 63) (expt 2 62))))
  (check-equal? (@@bignum.adigit-sub! a 0 b 0 0) 1)
  (check-equal? a (vector (* 3 (expt 2 62)) (expt 2 62))))

(check-equal? @@bignum.mdigit-width 8)
(check-equal? @@bignum.mdigit-width 16)
(check-equal? @@bignum.adigit-width 64)
