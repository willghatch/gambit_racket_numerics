#lang racket

(require "bignum.rkt"
         "translate.rkt"
         rackunit)

;; Require one postional argument with a path to gsi.
(define gsi-path (vector-ref (current-command-line-arguments) 0))

;; Execute fambit code return result and string of executed code.
(define (gambit-exec code)
  (define untransformed-output (open-output-bytes))
  (write code untransformed-output)
  (define untransformed-bytes (get-output-bytes untransformed-output))
  (close-output-port untransformed-output)
  (define transformed-bytes
    (regexp-replace* #rx#"@@" untransformed-bytes #"##"))
  (define-values (gsi stdout stdin stderr)
    (subprocess #f #f (current-error-port)
                gsi-path #"-:d-" #"-e" #"(write (eval (read)))"))
  (display transformed-bytes stdin)
  (close-output-port stdin)
  (define result (read stdout))
  (close-input-port stdout)
  (subprocess-wait gsi)
  (define status (subprocess-status gsi))
  (values (if (not (= status 0)) `(error ,status) result) transformed-bytes))

;; Test syntax
(define-syntax-rule (test-bignum racket gambit)
  (let-values
    (((racket-results) racket)
     ((gambit-results transformed-bytes) (gambit-exec `gambit)))
    (check-equal? racket-results gambit-results
                  (format "\nracket code:\n~a\ngambit code:\n~a\n"
                          `racket transformed-bytes))))

(define adigit-modulus (expt 2 @@bignum.adigit-width))
(define adigit-ones (- adigit-modulus 1))

(define-syntax-rule (gambit-bignum adigits ...)
  `(let* ((mdigit-modulus (expt 2 @@bignum.mdigit-width))
          (mdigits-in-adigit (/ @@bignum.adigit-width
                                @@bignum.mdigit-width))
          (lst (list adigits ...))
          (k (length lst))
          (x (@@bignum.make k #f #f)))
     (let loop1 ((i 0) (lst lst))
       (if (pair? lst)
           (let ()
             (let loop2 ((j 0))
               (if (< j mdigits-in-adigit)
                   (let ()
                     (@@bignum.mdigit-set!
                       x (+ (* i mdigits-in-adigit) j)
                       (modulo (quotient (car lst) (expt mdigit-modulus j))
                               mdigit-modulus))
                     (loop2 (+ j 1)))))
             (loop1 (+ i 1) (cdr lst)))
           x))))

(define-syntax-rule (racket-bignum adigits ...)
  (bignum (vector adigits ...)))

(define-syntax-rule (gambit-decode x)
  `(let ((mdigit-modulus (expt 2 @@bignum.mdigit-width))
         (mdigits-in-adigit (/ @@bignum.adigit-width
                               @@bignum.mdigit-width)))
     (let loop1 ((i 0))
       (if (< i (@@bignum.adigit-length x))
           (cons (let loop2 ((j 0))
                   (if (< j mdigits-in-adigit)
                       (+ (* (expt mdigit-modulus j)
                             (@@bignum.mdigit-ref x (+ (* i mdigits-in-adigit) j)))
                          (loop2 (+ j 1)))
                         0))
                 (loop1 (+ i 1)))
           (list)))))

(define-syntax-rule (racket-decode x)
  (vector->list (bignum-adigits x)))

;; Tests

(test-bignum (list @@bignum.fdigit-width
                   @@bignum.mdigit-width
                   @@bignum.adigit-width)
             (list @@bignum.fdigit-width
                   @@bignum.mdigit-width
                   @@bignum.adigit-width))

(test-bignum (let* ((x (racket-bignum adigit-ones adigit-ones 0))
                    (ans (@@bignum.adigit-inc! x 1)))
               (list (racket-decode x) ans))
             (let* ((x ,(gambit-bignum ,adigit-ones ,adigit-ones 0))
                    (ans (@@bignum.adigit-inc! x 1)))
               (list ,(gambit-decode x) ans)))


(test-bignum (let* ((x (racket-bignum 0 0))
                    (hi (racket-bignum 0))
                    (lo (racket-bignum 36028797018963968)))
               (@@bignum.adigit-cat! x 1 hi 0 lo 0 8)
               (racket-decode x))
             (let* ((x ,(gambit-bignum 0 0))
                    (hi ,(gambit-bignum 0))
                    (lo ,(gambit-bignum 36028797018963968)))
               (@@bignum.adigit-cat! x 1 hi 0 lo 0 8)
               ,(gambit-decode x)))
