#lang racket
(require compatibility/defmacro
         racket/fixnum
         racket/flonum
         "bignum.rkt"
         "constants.rkt")
(provide (all-defined-out)
         (all-from-out "bignum.rkt")
         flvector
         (rename-out [-if if]))

(define-syntax-rule (@@define-macro args ...) (define-macro args ...))
(define-syntax-rule (@@declare args ...) (void))
(define-syntax-rule (declare args ...) (void))

(define-syntax (-if stx)
  (syntax-case stx ()
    ((_ test then else)
     #'(if test then else))
    ((_ test then)
     #'(if test then (void)))))

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

(define (use-fast-bignum-algorithms) #f)
(define (@@bignum.fast-gcd-size) (void))

(define (@@make-table . args) (void))
(define (@@table-ref . args) #f)
(define (@@table-set! . args) (void))

(define @@cons (lambda (x y) (vector x y)))
(define @@car (lambda (x) (vector-ref x 0)))
(define @@cdr (lambda (x) (vector-ref x 1)))
(define @@set-car! (lambda (x y) (vector-set! x 0 y)))
(define @@set-cdr! (lambda (x y) (vector-set! x 1 y)))
(define @@eqv? eqv?)
(define @@not not)
(define @@fixnum? fixnum?)
(define @@fxnot fxnot)
(define @@fxfirst-bit-set (lambda (x) (fxand 1 (fxnot (fxand x 1)))))
(define @@fx= fx=)
(define @@fx< fx<)
(define @@fx<= fx<=)
(define @@fx> fx>)
(define @@fx>= fx>=)
(define @@fx-?
  (case-lambda ((x) (fixnum-fail (fixnum-bounds (fx- 0 x))))
               ((l r) (fixnum-fail (fixnum-bounds (fx- l r))))
               ((l r . rem)
                (let ((result (fixnum-fail (fixnum-bounds (fx- l r)))))
                  (and result (apply @@fx-? result r))))))
(define @@fx-
  (case-lambda ((x) (fx- 0 x))
               ((l r) (fx- l r))
               ((l r . rem) (apply @@fx-? (fx- l r) rem))))
(define @@fx+ (case-lambda ((x) x) ((x y . r) (apply @@fx+ (fx+ x y) r))))
(define @@fx+? (lambda (x y) (fixnum-fail (fixnum-bounds (fx+ x y)))))
(define @@fxzero? (lambda (x) (fx= x 0)))
(define @@fxnegative? (lambda (x) (fx< x 0)))
(define @@fxpositive? (lambda (x) (fx> x 0)))
(define @@fxodd? (lambda (x) (not (zero? (fxmodulo x 2)))))
(define @@fx*? (lambda (x y) (fixnum-fail (fixnum-bounds (fx* x y)))))
(define @@fx* fx*)
(define @@fxquotient fxquotient)
(define @@fxmodulo fxmodulo)
(define @@fxremainder fxremainder)
(define @@fxmax fxmax)
(define @@fxmin fxmin)
(define @@fxlength
  (lambda (x)
    (if (fx< x 0)
        (@@fxlength (fx- -1 x))
        (let loop ((x x) (counter 0))
          (if (zero? x) counter
              (loop (fxrshift x 1) (fx+ counter 1)))))))
(define @@fxarithmetic-shift-left fxlshift)
(define @@fxarithmetic-shift-left? (lambda (x y) (fixnum-fail (fixnum-bounds (fxlshift x y)))))
(define @@fxarithmetic-shift-right fxrshift)
(define @@fxand fxand)
(define @@flonum? flonum?)
(define @@flonum->fixnum fl->fx)
(define @@fixnum->flonum-exact? (lambda (x) (fx= (fl->fx (fx->fl x)) x)))
(define @@fl= fl=)
(define @@fl< fl<)
(define @@fl* fl*)
(define @@fl/ fl/)
(define @@fl+ fl+)
(define @@flcos flcos)
(define @@flcosh (lambda (x) (fl/ (fl+ (exp x) (exp (fl- 0.0 x))) 2.0)))
(define @@flsin flsin)
(define @@flsinh (lambda (x) (fl/ (fl- (exp x) (exp (fl- 0.0 x))) 2.0)))
(define @@flmax flmax)
(define @@flatan flatan)
(define @@flexp flexp)
(define @@flexpt flexpt)
(define @@fllog fllog)
(define @@fllog1p (lambda (x) (fllog (+ x 1))))
(define @@flilogb (lambda (x) (fltruncate (fl/ (fllog x) (fllog 2.0))))) ; unsure
(define @@flscalbn (lambda (x y) (fl* x (flexpt 2.0 y)))) ; unsure
(define @@flabs flabs)
(define @@flfloor flfloor)
(define @@flsqrt flsqrt)
(define @@flsquare (lambda (x) (* x x)))
(define @@flcopysign (lambda (x y) (fl* (flabs x) (fl/ y (flabs y)))))
(define @@fl<= fl<=)
(define @@fl- (case-lambda ((x) (fl- 0.0 x))
                           ((l r) (fl- l r))))
(define @@fixnum->flonum fx->fl)
(define @@flnan? (lambda (x) (not (fl= x x))))
(define @@flinfinite? (lambda (x) (or (fl= +inf.0 x) (fl= -inf.0 x))))
(define @@flfinite? (lambda (x) (not (or (@@flnan? x) (fl= +inf.0 x) (fl= -inf.0 x)))))
(define @@flpositive? (lambda (x) (fl< 0.0 x)))
(define @@flnegative? (lambda (x) (fl> 0.0 x)))
(define @@flzero? (lambda (x) (fl= 0.0 x)))
(define @@flinteger? (lambda (x) (fl= x (flfloor x))))
(define @@ratnum? ratnum?)
(define @@cpxnum? cpxnum?)
(define @@eq? eq?)
(define @@max-fixnum max-fixnum)
(define @@min-fixnum min-fixnum)
(define @@fixnum-width 30)
(define @@make-f64vector make-flvector)
(define @@f64vector-set! flvector-set!)
(define @@f64vector-ref flvector-ref)
(define @@f64vector flvector)
(define @@vector vector)
(define @@vector-ref vector-ref)

(define-prim (@@raise-type-exception arg-num type-id proc args)
  (raise type-id))
(define-prim (@@raise-range-exception arg-num proc args ...)
  (raise 'range))
(define-prim (@@raise-divide-by-zero-exception arg-num proc args ...)
  (raise 'divide-by-zero))
(define-prim (@@raise-heap-overflow-exception arg-num proc args ...)
  (raise 'heap-overflow))

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
(define-fail-check-type exact-integer 'exact-integer)
(define-fail-check-type rational 'rational)
(define-fail-check-type finite-real 'finite-real)

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

(@@define-macro (macro-cpxnum-+1/2+sqrt3/2i)
  (make-rectangular 1/2 (/ (sqrt 3) 2)))

(@@define-macro (macro-cpxnum-+1/2-sqrt3/2i)
  (make-rectangular 1/2 (- (/ (sqrt 3) 2))))

(@@define-macro (macro-cpxnum--1/2+sqrt3/2i)
  (make-rectangular -1/2 (/ (sqrt 3) 2)))

(@@define-macro (macro-cpxnum--1/2-sqrt3/2i)
  (make-rectangular -1/2 (- (/ (sqrt 3) 2))))

(@@define-macro (macro-cpxnum-+sqrt3/2+1/2i)
  (make-rectangular (/ (sqrt 3) 2) 1/2))

(@@define-macro (macro-cpxnum-+sqrt3/2-1/2i)
  (make-rectangular (/ (sqrt 3) 2) -1/2))

(@@define-macro (macro-cpxnum--sqrt3/2+1/2i)
  (make-rectangular (- (/ (sqrt 3) 2)) 1/2))

(@@define-macro (macro-cpxnum--sqrt3/2-1/2i)
  (make-rectangular (- (/ (sqrt 3) 2)) -1/2))

(@@define-macro (macro-flonum-rational? obj) ;; obj must be a flonum
  `(@@flfinite? ,obj))

(@@define-macro (macro-cpxnum-rational? obj) ;; obj must be a cpxnum
  `(and (macro-cpxnum-are-possibly-real?)
        (let ((imag (macro-cpxnum-imag ,obj)))
          (and (@@flonum? imag)
               (@@flzero? imag)
               (let ((real (macro-cpxnum-real ,obj)))
                 (macro-noncpxnum-rational? real))))))

(@@define-macro (macro-noncpxnum-rational? obj) ;; obj must be in fixnum/bignum/ratnum/flonum
  `(or (@@not (@@flonum? ,obj))
       (macro-flonum-rational? ,obj)))

(@@define-macro (macro-inexact-log-2)    (log 2))
(@@define-macro (macro-inexact-exp-+1/2) (exp +1/2))
(@@define-macro (macro-inexact-exp--1/2) (exp -1/2))

(@@define-macro (macro-inexact-epsilon) 1.1102230246251565e-16)     ; (- 1 epsilon) <> 1, epsilon smallest
(@@define-macro (macro-inexact-lambda)  2.2250738585072014e-308)    ; smallest positive flonum
(@@define-macro (macro-inexact-omega)   1.7976931348623157e308)     ; largest finite flonum

(@@define-macro (macro-flonum-m-bits)
  52)

(@@define-macro (macro-flonum-m-bits-plus-1)
  53)

(@@define-macro (macro-flonum-m-bits-plus-1*2)
  106)

(@@define-macro (macro-flonum-e-bits)
  11)

(@@define-macro (macro-flonum-sign-bit) ;; (expt 2 (+ (macro-flonum-e-bits) (macro-flonum-m-bits)))
  #x8000000000000000)

(@@define-macro (macro-flonum-m-min) ;; (expt 2.0 (macro-flonum-m-bits))
  4503599627370496.0)

(@@define-macro (macro-flonum-+m-min) ;; (expt 2 (macro-flonum-m-bits))
  4503599627370496)

(@@define-macro (macro-flonum-+m-max-plus-1) ;; (expt 2 (macro-flonum-m-bits-plus-1))
  9007199254740992)

(@@define-macro (macro-flonum-+m-max) ;; (- (macro-flonum-+m-max-plus-1) 1)
  9007199254740991)

(@@define-macro (macro-flonum-+m-max-plus-1-inexact);; (exact->inexact (macro-flonum-+m-max-plus-1))
  9007199254740992.0)

(@@define-macro (macro-flonum-inverse-+m-max-plus-1-inexact);; (/ (macro-flonum-+m-max-plus-1-inexact))
  (/ 9007199254740992.0))

(@@define-macro (macro-flonum--m-min) ;; (- (macro-flonum-+m-min))
  -4503599627370496)

(@@define-macro (macro-flonum--m-max) ;; (- (macro-flonum-+m-max))
  -9007199254740991)

(@@define-macro (macro-flonum-e-bias) ;; (- (expt 2 (- (macro-flonum-e-bits) 1)) 1)
  1023)

(@@define-macro (macro-flonum-e-bias-plus-1) ;; (+ (macro-flonum-e-bias) 1)
  1024)

(@@define-macro (macro-flonum-e-bias-minus-1) ;; (- (macro-flonum-e-bias) 1)
  1022)

(@@define-macro (macro-flonum-e-min) ;; (- (+ (macro-flonum-e-bias) (macro-flonum-m-bits) -1))
  -1074)

(@@define-macro (macro-flonum-min-normal) ;; (expt 2.0 (+ (macro-flonum-m-bits) (macro-flonum-e-min)))
  (expt 2.0 (+ 52 -1074)))

(@@define-macro (macro-scale-down) ;; (expt 2 (- (+ (quotient (macro-flonum-e-bias-plus-1) 2) 1)))
  (expt 2 -513))

(@@define-macro (macro-inexact-scale-down) ;; (expt 2.0 (- (+ (quotient (macro-flonum-e-bias-plus-1) 2) 1)))
  (expt 2.0 -513))

(@@define-macro (macro-scale-up) ;; (expt 2 (+ (quotient (macro-flonum-e-bias-plus-1) 2) (macro-flonum-m-bits-plus-1)))
  (expt 2 565))

(@@define-macro (macro-inexact-scale-up) ;; (expt 2.0 (+ (quotient (macro-flonum-e-bias-plus-1) 2) (macro-flonum-m-bits-plus-1)))
  (expt 2.0 565))

(@@define-macro (macro-inexact-radix) ;; (exact->inexact (macro-radix))
  16384.0)

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

(begin-for-syntax
  (define @@compilation-options '()))

(define-syntax macro-force-vars
  (lambda (stx)
    (syntax-case stx ()
      ((_ vars expr)
       (if ((if (and (pair? @@compilation-options)
                     (pair? (car @@compilation-options)))
                assq
                memq)
            'force
            @@compilation-options)

           (syntax-case (datum->syntax
                         #'vars
                         (map (lambda (x) `(,x (@@force ,x)))
                              (syntax->list #'vars)))
               ()
             (bindings #'(let bindings expr)))

           #'expr)))))
