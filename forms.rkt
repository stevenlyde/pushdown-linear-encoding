#lang racket

(provide (struct-out exp)
         (struct-out constant-exp)
         (struct-out ref-exp)
         (struct-out param-exp)
         (struct-out lambda-exp)
         (struct-out let-exp)
         (struct-out prim-exp)
         (struct-out call-exp)
         (struct-out if-exp)
         (struct-out set!-exp)
         parse
         labeled
         fold/exp)

; AE ::= constant
;     |  symbol
;     |  (lambda (symbol ...) E)
; E  ::= (let ((symbol E)) E)
;     |  (prim op AE ...)
;     |  (AE AE ...)
;     |  (if AE E E)
;     |  (set! symbol AE)
;     |  AE

(struct exp (label))

(struct constant-exp exp (val))
(struct ref-exp exp (id))
(struct param-exp exp (id))
(struct lambda-exp exp (params body))

(struct let-exp exp (id val-exp body))
(struct prim-exp exp (op args))
(struct call-exp exp (fun args))
(struct if-exp exp (test then else))
(struct set!-exp exp (id val-exp))


(define genlab
  (let ([label 0])
    (lambda ()
      (begin0 label (set! label (+ 1 label))))))

(define (parse e)
  (match e
    [`(let ((,id ,val-exp)) ,body)
     (let ([id (param-exp (genlab) id)]
           [val-exp (parse val-exp)]
           [body (parse body)])
       (let-exp (genlab) id val-exp body))]
    [`(quote ,datum)
     (constant-exp (genlab) e)]
    [`(prim ,op ,args ...)
     (let ([args (map parse args)])
       (prim-exp (genlab) op args))]
    [`(set! ,id ,val-exp)
     (let ([id (parse id)]
           [val-exp (parse val-exp)])
       (set!-exp (genlab) id val-exp))]
    [`(if ,test ,then ,else)
     (let ([test (parse test)]
           [then (parse then)]
           [else (parse else)])
       (if-exp (genlab) test then else))]
    [`(lambda ,params ,body)
     (let ([params (map (λ (param) (param-exp (genlab) param)) params)]
           [body (parse body)])
       (lambda-exp (genlab) params body))]
    [`(,fun ,args ...)
     (let ([fun (parse fun)]
           [args (map parse args)])
       (call-exp (genlab) fun args))]
    [(? symbol? v)
     (ref-exp (genlab) v)]
    [else
     (constant-exp (genlab) e)]))


(define (labeled e)
  (match e
    [(constant-exp label val)
     `(label ,label ,val)]
    [(ref-exp label id)
     `(label ,label ,id)]
    [(param-exp label id)
     `(label ,label ,id)]
    [(lambda-exp label params body)
     `(label ,label (lambda ,(map labeled params) ,(labeled body)))]
    [(let-exp label id exp body)
     `(label ,label (let ((,(labeled id) ,(labeled exp))) ,(labeled body)))]
    [(prim-exp label op args)
     `(label ,label (prim ,op ,@(map labeled args)))]
    [(call-exp label fun args)
     `(label ,label (,(labeled fun) ,@(map labeled args)))]
    [(if-exp label test then else)
     `(label ,label (if ,(labeled test) ,(labeled then) ,(labeled else)))]
    [(set!-exp label id exp)
     `(label ,label (set! ,(labeled id) ,(labeled exp)))]))


(define (fold/exp f result e)
  (match e
    [(constant-exp label val)
     (f e result)]
    [(ref-exp label var)
     (f e result)]
    [(param-exp label var)
     (f e result)]
    [(lambda-exp label params body)
     (let* ([result (f e result)]
            [result (fold/exp f result body)]
            [result (for/fold ([result result])
                              ([param params])
                      (fold/exp f result param))])
       result)]
    [(let-exp label id exp body)
     (let* ([result (f e result)]
            [result (fold/exp f result id)]
            [result (fold/exp f result exp)]
            [result (fold/exp f result body)])
       result)]
    [(prim-exp label op args)
     (let* ([result (f e result)]
            [result (for/fold ([result result])
                              ([arg args])
                      (fold/exp f result arg))])
       result)]
    [(call-exp label fun args)
     (let* ([result (f e result)]
            [result (fold/exp f result fun)]
            [result (for/fold ([result result])
                              ([arg args])
                      (fold/exp f result arg))])
       result)]
    [(if-exp label test then else)
     (let* ([result (f e result)]
            [result (fold/exp f result test)]
            [result (fold/exp f result then)]
            [result (fold/exp f result else)])
       result)]
    [(set!-exp label id val-exp)
     (let* ([result (f e result)]
            [result (fold/exp f result id)]
            [result (fold/exp f result val-exp)])
       result)]))
