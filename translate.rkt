#lang racket

(require "definitions.rkt"
         "cheat.rkt"
         "constants.rkt")

(provide to-rktnum from-rktnum rktnum->bignum bignum->rktnum)
(provide @@eqv?)

(define (to-rktnum x)
  (cond ((fixnum? x) x)
        ((flonum? x) x)
        ((bignum? x) (bignum->rktnum x))
        ((ratnum? x) (/ (ratnum-numerator x) (ratnum-denominator x)))
        ((cpxnum? x) (make-rectangular (to-rktnum (cpxnum-real x))
                                       (to-rktnum (cpxnum-imag x))))
        (else x)))

(define (from-rktnum x)
  (if (number? x)
      (if (real? x)
          (if (exact? x)
              (if (integer? x)
                  (if (or (> x max-fixnum) (< x min-fixnum))
                      (rktnum->bignum x)
                      x)
                  (ratnum (from-rktnum (numerator x))
                          (from-rktnum (denominator x))))
              x)
          (cpxnum (from-rktnum (real-part x))
                  (from-rktnum (imag-part x))))
      x))

(define (rktnum->bignum x)
  (define adigit-modulus (expt 2 @@bignum.adigit-width))
  (define magnitude (abs x))
  (define adigits
    (for/vector ((_ (in-naturals)) #:break (zero? magnitude))
      (define adigit (modulo magnitude adigit-modulus))
      (set! magnitude (quotient magnitude adigit-modulus))
      adigit))
  (if (negative? x) (bignum.- (@@fixnum->bignum 0) (bignum adigits)) (bignum adigits)))

(define (bignum->rktnum x)
  (define adigit-modulus (expt 2 @@bignum.adigit-width))
  (if (@@bignum.negative? x)
      (- (bignum->rktnum (bignum.- (@@fixnum->bignum 0) x)))
      (for/sum ((adigit (in-vector (bignum-adigits x)))
                (i (in-naturals)))
        (* adigit (expt adigit-modulus i)))))

(define @@eqv? (λ (a b)
                 (eprintf "eqv? with ~a ~b~n" a b)
                 (cond
                   [(bignum? a)
                    (@@eqv? (bignum->rktnum a) b)]
                   [(bignum? b)
                    (eqv? a (bignum->rktnum b))]
                   [else (eqv? a b)])))
