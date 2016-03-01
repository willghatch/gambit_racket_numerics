#lang racket
(require compatibility/defmacro
         racket/fixnum
         racket/flonum
         "bignum.rkt"
         "constants.rkt")
(provide (all-defined-out) (all-from-out "bignum.rkt") flvector)

(define-syntax-rule (@@define-macro args ...) (define-macro args ...))
(define-syntax-rule (@@declare args ...) (void))

(define-syntax define-prim
  (lambda (stx)
    (syntax-case stx ()
      ((_ (id . params) body ...)
       #'(define-prim id
           (lambda params
             body ...)))
       ((_ id val)
        #'(define id val)))))

(define-syntax-rule (fixnum-fail exps ...)
  (with-handlers ((exn:fail:contract:non-fixnum-result? (lambda (e) #f)))
    exps ...))

(define-syntax-rule (fixnum-bounds x)
  (if (or (< x min-fixnum) (> x max-fixnum)) #f x))

(struct ratnum (numerator denominator) #:transparent)
(define-syntax-rule (macro-ratnum-numerator x) (ratnum-numerator x))
(define-syntax-rule (macro-ratnum-denominator x) (ratnum-denominator x))
(define-syntax-rule (macro-ratnum-make n d) (ratnum n d))

(struct cpxnum (real imag) #:transparent)
(define-syntax-rule (macro-cpxnum-real x) (cpxnum-real x))
(define-syntax-rule (macro-cpxnum-imag x) (cpxnum-imag x))
(define-syntax-rule (macro-cpxnum-make r i) (cpxnum r i))

(define @@eqv? eqv?)
(define @@not not)
(define @@fixnum? fixnum?)
(define @@fx= fx=)
(define @@fx< fx<)
(define @@fx-? (case-lambda ((x) (fixnum-fail (fixnum-bounds (fx- 0 x))))
                            ((l r) (fixnum-fail (fixnum-bounds (fx- l r))))))
(define @@fx+ fx+)
(define @@fx+? (lambda (x y) (fixnum-fail (fixnum-bounds (fx+ x y)))))
(define @@fxzero? (lambda (x) (fx= x 0)))
(define @@fxnegative? (lambda (x) (fx< x 0)))
(define @@fxodd? (lambda (x) (not (zero? (fxmodulo x 2)))))
(define @@fx*? (lambda (x y) (fixnum-fail (fx* x y))))
(define @@fx* fx*)
(define @@fxarithmetic-shift-left fxlshift)
(define @@flonum? flonum?)
(define @@fixnum->flonum-exact? (lambda (x) (fx= (fl->fx (fx->fl x)) x)))
(define @@fl= fl=)
(define @@fl< fl<)
(define @@fl* fl*)
(define @@fl/ fl/)
(define @@fl- (case-lambda ((x) (fl- 0.0 x))
                           ((l r) (fl- l r))))
(define @@fixnum->flonum fx->fl)
(define @@flnan? (lambda (x) (not (fl= x x))))
(define @@flfinite? (lambda (x) (not (or (@@flnan? x) (fl= +inf.0 x) (fl= -inf.0 x)))))
(define @@flpositive? (lambda (x) (fl< 0.0 x)))
(define @@flnegative? (lambda (x) (fl> 0.0 x)))
(define @@flzero? (lambda (x) (fl= 0.0 x)))
(define @@flinteger? (lambda (x) (fl= x (flfloor x))))
(define @@ratnum? ratnum?)
(define @@cpxnum? cpxnum?)
(define @@eq? eq?)
(define @@fx-
  (case-lambda ((x) (fx* -1 x))
               ((l r) (fx- l r))))
(define @@max-fixnum max-fixnum)
(define @@min-fixnum min-fixnum)

(define-prim (@@raise-type-exception arg-num type-id proc args)
  (raise type-id))

; agnostic implementation
(define (@@flonum->exact x)
  (let ((exact (inexact->exact x)))
    (if (integer? exact) exact (ratnum (numerator exact) (denominator exact)))))

(define-macro (define-check-type type-id type predicate . arguments)
  (define (sym . lst)
    (string->symbol (apply string-append (map symbol->string lst))))
  (let ()
    (define macro-check-type (sym 'macro-check- type-id))
    (define @@fail-check-type (sym '@@fail-check- type-id))
    `(begin
       (define-macro (,(sym 'implement-check-type- type-id));;;;;;;;;;
         '(define-fail-check-type ,type-id ,type))
     (define-macro (,macro-check-type var arg-num form expr)
       (define (rest-param x)
         (if (pair? x)
             (rest-param (cdr x))
             x))
       (define (nonrest-params x)
         (if (pair? x)
           (cons (car x) (nonrest-params (cdr x)))
           '()))
       (define (key-params x)
         (if (pair? x)
           (if (keyword? (car x))
             (cons (car x) (cons (cadr x) (key-params (cddr x))))
             (key-params (cdr x)))
           '()))
       (define (prekey-params x)
         (if (or (not (pair? x)) (keyword? (car x)))
           '()
           (cons (car x) (prekey-params (cdr x)))))
       (define (failure name)
         (let* ((k (key-params (cdr form)))
                (r (rest-param (cdr form)))
                (nr (nonrest-params (cdr form)))
                (pk (prekey-params nr)))
           (if (and (null? k) (not (null? r)))
             `(,name ,arg-num '() ,(car form) ,@pk ,r)
             `(,name
               ,arg-num
               ,(if (and (null? k) (null? r))
                  (car form)
                  `(list ,(car form) ,@k ,@(if (null? r) '() (list r))))
               ,@pk))))
       `(macro-if-checks
          (if (,',predicate ,var ,@',arguments)
            ,expr
            ,(failure '(unquote @@fail-check-type)))
          ,expr)))))

#;(define-check-type real 'real
  @@real?)
#;(define-check-type fixnum 'fixnum
  @@fixnum?)
#;(define-check-type flonum 'flonum
  @@flonum?)

(@@define-macro (define-fail-check-type type-name . type-id)
  (define (sym . lst)
    (string->symbol (apply string-append (map symbol->string lst))))
  (let ()
    (define @@fail-check-type (sym '@@fail-check- type-name))
    `(define-prim ((unquote @@fail-check-type) arg-num proc . args)
       (@@raise-type-exception
        arg-num
        ,(if (pair? type-id) (car type-id) `',type-name)
        proc
        args))))

(define-fail-check-type real 'real)
(define-fail-check-type number 'number)
(define-fail-check-type integer 'integer)

(@@define-macro (macro-exact-int? obj) ;; obj can be any object
  `(or (@@fixnum? ,obj)
       (@@bignum? ,obj)))

(@@define-macro (macro-flonum-int? obj) ;; obj must be a flonum
  `(@@flinteger? ,obj))

(@@define-macro (macro-cpxnum-are-possibly-real?) #f)

(@@define-macro (macro-cpxnum-real? obj) ;; obj must be a cpxnum
  `(and (macro-cpxnum-are-possibly-real?)
        (let ((imag (macro-cpxnum-imag ,obj)))
          (and (@@flonum? imag)
               (@@flzero? imag)))))

(@@define-macro (macro-bignum-odd? x);;;;;;;;;;;;;;;;;;;;
  `(@@fxodd? (@@bignum.mdigit-ref ,x 0)))

(@@define-macro (macro-special-case-exact-zero?) #t)

(@@define-macro (macro-cpxnum-int? obj) ;; obj must be a cpxnum
  `(and (macro-cpxnum-are-possibly-real?)
        (macro-cpxnum-real? ,obj)
        (let ((real (macro-cpxnum-real ,obj)))
          (macro-noncpxnum-int? real))))

(@@define-macro (macro-noncpxnum-int? obj) ;; obj must be in fixnum/bignum/ratnum/flonum
  `(if (@@flonum? ,obj)
     (macro-flonum-int? ,obj)
     (@@not (@@ratnum? ,obj))))

(@@define-macro (macro-inexact-+2)     2.0)
(@@define-macro (macro-inexact--2)    -2.0)
(@@define-macro (macro-inexact-+1)     1.0)
(@@define-macro (macro-inexact--1)    -1.0)
(@@define-macro (macro-inexact-+1/2)   0.5)
(@@define-macro (macro-inexact-+0)     0.0)
(@@define-macro (macro-inexact--0)    -0.0)
(@@define-macro (macro-inexact-+pi)    3.141592653589793)
(@@define-macro (macro-inexact--pi)   -3.141592653589793)
(@@define-macro (macro-inexact-+pi/2)  1.5707963267948966)
(@@define-macro (macro-inexact--pi/2) -1.5707963267948966)
(@@define-macro (macro-inexact-+pi/4)   .7853981633974483)
(@@define-macro (macro-inexact-+3pi/4) 2.356194490192345)
(@@define-macro (macro-inexact-+inf)  (/ +1. 0.))
(@@define-macro (macro-inexact--inf)  (/ -1. 0.))
(@@define-macro (macro-inexact-+nan)  (/ 0. 0.))
(@@define-macro (macro-cpxnum-+2i)    +2i)
(@@define-macro (macro-cpxnum--i)     -i)
(@@define-macro (macro-cpxnum-+i)     +i)

(define-syntax macro-number-dispatch
  (lambda (stx)
    (syntax-case stx ()
      ((_ num err fix big rat flo cpx)
       #'(cond ((@@fixnum? num) fix)
               ((@@flonum? num) flo)
               ((@@bignum? num) big)
               ((@@ratnum? num) rat)
               ((@@cpxnum? num) cpx)
               (else            err))))))
