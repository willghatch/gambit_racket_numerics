#lang racket

(require "_num.rkt"
         "translate.rkt"
         rackunit)

(define-syntax-rule (test proc expected args ...)
  (check-equal? (to-rktnum (apply proc (map from-rktnum (list args ...)))) expected))

(test @@= #t 3 3)
(test @@= #t 5872438972384957230489570234895732408957230489572340985723
             5872438972384957230489570234895732408957230489572340985723)
(test @@= #t 3.0 3.0)
(test @@= #t 3/5 3/5)
(test @@= #t 3+4i 3+4i)

(test @@= #f 3 4)
(test @@= #f 5872438972384957230489570234895732408957230489572340985723
             5872438972384957230489570234895732408957230489572340985724)
(test @@= #f 3.0 3.1)
(test @@= #f 3/5 4/5)
(test @@= #f 3+4i 3+5i)
