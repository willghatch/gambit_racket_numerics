#lang racket

(require "_num.rkt")

(provide (all-defined-out))

(define gambit-+
  (case-lambda
    (() 0)
    ((x) x)
    ((x y . r) (apply gambit-+ (@@+ x y) r))))

(define gambit--
  (case-lambda
    ((x) (@@- 0 x))
    ((x y) (@@- x y))
    ((x y . r) (apply gambit-- (@@- x y) r))))
