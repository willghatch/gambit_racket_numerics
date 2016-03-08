#lang racket

(require "_num.rkt"
         "translate.rkt")
(provide gambit-lookup
         from-rktnum
         to-rktnum)

(define gambit-+ (void))
(define gambit-= (void))
(define gambit-< (void))

(define (gambit-lookup fun)
  (case fun
    ((+) gambit-+)
    ((=) gambit-=)
    ((<) gambit-<)
    (else #f)))
