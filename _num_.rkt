#lang racket
(require compatibility/defmacro
         racket/fixnum
         racket/flonum)
(provide (all-defined-out))

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

(struct bignum (value) #:transparent)

(struct ratnum (numerator denominator) #:transparent)
(define-syntax-rule (macro-ratnum-numerator x) (ratnum-numerator x))
(define-syntax-rule (macro-ratnum-denominator x) (ratnum-denominator x))
(define-syntax-rule (macro-ratnum-make n d) (ratnum n d))

(struct cpxnum (real imag) #:transparent)
(define-syntax-rule (macro-cpxnum-real x) (cpxnum-real x))
(define-syntax-rule (macro-cpxnum-imag x) (cpxnum-imag x))
(define-syntax-rule (macro-cpxnum-make r i) (cpxnum r i))

(define @@fixnum? fixnum?)
(define @@fx= fx=)
(define @@fx< fx<)
(define @@flonum? flonum?)
(define @@fixnum->flonum-exact? (lambda (x) (fx= (fl->fx (fx->fl x)) x)))
(define @@fl= fl=)
(define @@fixnum->flonum fx->fl)
(define @@flnan? (lambda (x) (not (fl= x x))))
(define @@flfinite? (lambda (x) (not (or (@@flnan? x) (fl= +inf.0 x) (fl= -inf.0 x)))))
(define @@bignum? bignum?)
(define @@ratnum? ratnum?)
(define @@cpxnum? cpxnum?)
(define @@eq? eq?)
(define @@fx-
  (case-lambda ((x) (fx* -1 x))
               ((l r) (fx- l r))))
(define @@bignum.negative? (lambda (x) (negative? (bignum-value x))))

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

(define-check-type real 'real
  @@real?)
(define-check-type fixnum 'fixnum
  @@fixnum?)
(define-check-type flonum 'flonum
  @@flonum?)

(@@define-macro (macro-exact-int? obj) ;; obj can be any object
  `(or (@@fixnum? ,obj)
       (@@bignum? ,obj)))

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
