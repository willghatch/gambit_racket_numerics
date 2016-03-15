#lang racket

(require "_num.rkt")

(provide (all-defined-out))

(define gambit-+
  (case-lambda
    (() 0)
    ((x) x)
    ((x y . r) (apply gambit-+ (@@+ x y) r))))
