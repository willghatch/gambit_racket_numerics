#lang racket
(require "_num_.rkt")
(provide to-rktnum from-rktnum)

(define (bignum->rktnum x)
  (define magnitude
    (for/sum ((digit (in-bytes (bignum-digits x)))
              (i (in-naturals)))
      (* digit (expt 256 i))))
  `(if (bignum-negative x) (- magnitude) magnitude))

(define (to-rktnum x)
  (cond ((fixnum? x) x)
        ((flonum? x) x)
        ((bignum? x) (bignum->rktnum x))
        ((ratnum? x) (/ (ratnum-numerator x) (ratnum-denominator x)))
        ((cpxnum? x) (make-rectangular (to-rktnum (cpxnum-real x))
                                       (to-rktnum (cpxnum-imag x))))
        (else x)))

(define (rktnum->bignum x)
  (define magnitude (abs x))
  (define digits
    (for/list ((_ (in-naturals)) #:break (zero? magnitude))
      (define digit (modulo magnitude 256))
      (set! magnitude (quotient magnitude 256))
      digit))
  (bignum (negative? x) (apply bytes digits)))

(define (from-rktnum x)
  (if (number? x)
      (if (real? x)
          (if (exact? x)
              (if (integer? x)
                  (if (and (>= x -128) (< x 128)) x
                      (rktnum->bignum x))
                  (ratnum (from-rktnum (numerator x))
                          (from-rktnum (denominator x))))
              x)
          (cpxnum (from-rktnum (real-part x))
                  (from-rktnum (imag-part x))))
      x))
