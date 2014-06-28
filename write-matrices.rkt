#lang racket

(require "forms.rkt"
         "prims.rkt")

(define (flatten/exp e)
  (reverse (fold/exp cons empty e)))

(define program (parse (read)))
(define exps (flatten/exp program))

;;; Domains

; S: States
; L: Lambdas
; I: Numbers 
; Y: Symbols
; B: Booleans and Types
; V: Values (L I Y B)
; A: Addresses (X V)

(define (state? e) 
  (or (constant-exp? e)
      (ref-exp? e)
      (lambda-exp? e)
      (let-exp? e)
      (prim-exp? e)
      (call-exp? e)
      (if-exp? e)
      (set!-exp? e)))

; States
(define S
  (for/fold ([S empty])
    ([e exps]
     #:when (state? e))
    (cons e S)))

; Lambdas
(define L
  (set->list (for/fold ([S (set)])
               ([e exps]
                #:when (lambda-exp? e))
               (set-add S (exp-label e)))))


(define (variable e)
  (match e
    [(? ref-exp? e) (ref-exp-id e)]
    [(? param-exp? e) (param-exp-id e)]
    [else #f]))

; Variables
(define X
  (for/fold ([X empty])
    ([e exps]
     #:when (variable e))
    (let ([id (variable e)])
      (if (not (member id X))
          (cons id X)
          X))))

(define (number-exp? e)
  (and (constant-exp? e)
       (number? (constant-exp-val e))))


; Integers
(define I
  (set->list (for/fold ([I (set)])
               ([e exps]
                #:when (number-exp? e))
               (set-add I (exp-label e)))))

(define (quote-exp? e)
  (match e
    [(constant-exp _ `(quote . ,_)) #t]
    [else #f]))

; Symbols
(define Y
  (set->list (for/fold ([Y (set)])
               ([e exps]
                #:when (quote-exp? e))
               (set-add Y (exp-label e)))))

; Boooleans and Types
(define B '(SYM LIST VOID TRUE FALSE INT))

; Values
(define V (append L I Y B))

; Addresses
(define A (append X V))


(define arg-size
  (for/fold ([arg-size 3])
            ([e exps])
    (match e
      [(prim-exp _ op args)
       (max (length args) arg-size)]
      [(call-exp _ _ args)
       (max (length args) arg-size)]
      [else arg-size])))

(define prim-list-max
  (for/fold ([prim-list-max 0])
            ([e exps])
    (cond
      [(and (prim-exp? e) 
            (primlistop? (prim-exp-op e)))
       (max prim-list-max (length (prim-exp-args e)))]
      [else prim-list-max])))


(define (list-index lst v)
  (for/or ([w lst] [i (in-naturals)] #:when (equal? v w)) i))

(define-syntax-rule (define-lookup id hash)
  (define id
    (let ([h hash])
      (lambda (k) (hash-ref h k)))))

(define-lookup exp->addr
  (for/fold ([h (hash)])
    ([e exps])
    (cond
      [(or (lambda-exp? e)
           (number-exp? e)
           (quote-exp? e))
       (hash-set h e (list-index A (exp-label e)))]
      [(and (constant-exp? e)
            (eq? #t (constant-exp-val e)))
       (hash-set h e (- (length A) 3))]
      [(and (constant-exp? e)
            (eq? #f (constant-exp-val e)))
       (hash-set h e (- (length A) 2))]
      [(ref-exp? e)
       (hash-set h e (list-index A (ref-exp-id e)))]
      [(param-exp? e)
       (hash-set h e (list-index A (param-exp-id e)))]
      [else h])))

(define-lookup exp->state
  (for/fold ([h (hash)])
            ([s S]
             [n (in-naturals)])
    (hash-set h s n)))

(define-lookup label->exp
  (for/fold ([h (hash)])
            ([e exps])
    (hash-set h (exp-label e) e)))

(define labeled-value? number?)


;;; Matrices

(printf "r ~a 1~n" (length S))
(for ([n (in-naturals)]
      [e S]
      #:when (eq? program e))
  (printf "~a 0~n" n))
(newline)


(printf "sigma ~a ~a~n" (length A) (length V))
(for ([n (in-range 0 (length V))])
  (printf "~a ~a~n" (+ n (length X)) n))
(newline)


(printf "Fun ~a ~a~n" (length S) (length A))
(for ([e S]
      [n (in-naturals)])
  (match e
    [(call-exp _ fun _)
     (printf "~a ~a~n" n (exp->addr fun))]
    [else (void)]))
(newline)


(printf "CondTrue ~a ~a~n" (length S) (length S))
(for ([e S])
  (match e
    [(if-exp _ _ true _)
     (printf "~a ~a~n" (exp->state e) (exp->state true))]
    [else (void)]))
(newline)


(printf "CondFalse ~a ~a~n" (length S) (length S))
(for ([e S])
  (match e
    [(if-exp _ _ _ false)
     (printf "~a ~a~n" (exp->state e) (exp->state false))]
    [else (void)]))
(newline)


(printf "Body ~a ~a~n" (length V) (length S))
(for ([v V]
      [n (in-naturals)]
      #:when (labeled-value? v))
  (match (label->exp v)
    [(lambda-exp label params body)
     (printf "~a ~a~n" n (exp->state body))]
    [else (void)]))
(newline)


(for ([i (in-range arg-size 0 -1)])
  (printf "Arg~a ~a ~a~n" i (length S) (length A))
  (for ([s S]
        [n (in-naturals)])
    (match s
      [(let-exp _ id val-exp body)
       (cond
         [(= i 1) (printf "~a ~a~n" n (exp->addr id))]
         [else (void)])]
      [(set!-exp _ id val-exp)
       (cond
         [(= i 1) (printf "~a ~a~n" n (exp->addr id))]
         [(= i 2) (printf "~a ~a~n" n (exp->addr val-exp))]
         [else    (void)])]
      [(prim-exp _ op args)
       (when (and (primlistop? op) (<= i (length args)))
         (printf "~a ~a~n" n (exp->addr (list-ref args (- i 1)))))]
      [(if-exp _ test _ _)
       (when (= i 1) 
         (printf "~a ~a~n" n (exp->addr test)))]
      [(call-exp _ _ args)
       (when (<= i (length args))
         (printf "~a ~a~n" n (exp->addr (list-ref args (- i 1)))))]
      [else (void)]))
  (newline))


(for ([i (in-range arg-size 0 -1)])
  (printf "Var~a ~a ~a~n" i (length V) (length A))
  (for ([v V]
        [n (in-naturals)]
        #:when (labeled-value? v))
    (match (label->exp v)
      [(lambda-exp _ params body)
       (when (<= i (length params))
         (printf "~a ~a~n" n (exp->addr (list-ref params (- i 1)))))]
      [else (void)]))
  (newline))


(for ([i (in-range arg-size -1 -1)])
  (printf "Call~a ~a 1~n" i (length S))
  (for ([s S]
        [n (in-naturals)])
    (match s
      [(call-exp _ _ args)
       (when (= i (length args))
         (printf "~a 0~n" n))]
      [else (void)]))
  (newline))


(printf "If ~a 1~n" (length S))
(for ([s S]
      [n (in-naturals)])
  (when (if-exp? s)
     (printf "~a 0~n" n)))
(newline)


(printf "Set ~a 1~n" (length S))
(for ([s S]
      [n (in-naturals)])
  (when (set!-exp? s)
     (printf "~a 0~n" n)))
(newline)


(printf "PrimNum ~a 1~n" (length S))
(for ([s S]
      [n (in-naturals)])
  (when (and (prim-exp? s) (primnumop? (prim-exp-op s)))
     (printf "~a 0~n" n)))
(newline)


(printf "PrimBool ~a 1~n" (length S))
(for ([s S]
      [n (in-naturals)])
  (when (and (prim-exp? s) (primboolop? (prim-exp-op s)))
     (printf "~a 0~n" n)))
(newline)


(printf "PrimVoid ~a 1~n" (length S))
(for ([s S]
      [n (in-naturals)])
  (when (and (prim-exp? s) (primvoidop? (prim-exp-op s)))
     (printf "~a 0~n" n)))
(newline)


(for ([i (in-range prim-list-max -1 -1)])
  (printf "PrimList~a ~a 1~n" i (length S))
  (for ([s S]
        [n (in-naturals)])
    (when (and (prim-exp? s) 
               (primlistop? (prim-exp-op s))
               (= i (length (prim-exp-args s))))
       (printf "~a 0~n" n)))
  (newline))



(printf "LetVal ~a ~a~n" (length S) (length S))
(for ([s S])
  (match s
    [(let-exp _ _ val _)
     (printf "~a ~a~n" (exp->state s) (exp->state val))]
    [else (void)]))
(newline)


(printf "LetBody ~a ~a~n" (length S) (length S))
(for ([s S])
  (match s
    [(let-exp _ _ _ body)
     (printf "~a ~a~n" (exp->state s) (exp->state body))]
    [else (void)]))
(newline)


(printf "Push ~a ~a~n~n" (length S) (length S))
(printf "Pop ~a ~a~n~n" (length S) (length S))
(printf "Empty ~a ~a~n~n" (length S) (length S))


(printf "epsilon ~a ~a~n" (length S) (length S))
(for ([s S]
      [n (in-naturals)])
  (printf "~a ~a~n" n n))

