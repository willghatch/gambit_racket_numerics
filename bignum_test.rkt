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

;; Tests

(test-bignum (list @@bignum.fdigit-width
                   @@bignum.mdigit-width
                   @@bignum.adigit-width)
             (list @@bignum.fdigit-width
                   @@bignum.mdigit-width
                   @@bignum.adigit-width))

(test-bignum (let* ((x (bignum (vector adigit-ones adigit-ones 0)))
                    (ans (@@bignum.adigit-inc! x 1)))
               (list (to-rktnum x) ans))
             (let* ((x ,(to-rktnum (bignum (vector adigit-ones adigit-ones 0))))
                    (ans (@@bignum.adigit-inc! x 1)))
               (list x ans)))
