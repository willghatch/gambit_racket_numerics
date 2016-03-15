#lang racket

(require racket/fixnum
         "constants.rkt"
         "bignum.rkt")

(provide bignum.-)

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
