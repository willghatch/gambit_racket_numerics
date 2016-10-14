#lang racket

(require "toplevel.rkt"
         "translate.rkt")

(provide gambit-lookup
         from-rktnum
         to-rktnum)

(define lookup-table
  `((,+ . ,gambit-+)
    (,- . ,gambit--)
    ))

(define (gambit-lookup fun)
  (let ((result (assoc fun lookup-table)))
    (and result (cdr result))))
