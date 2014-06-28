#lang racket

(provide (struct-out exp)
         (struct-out constant-exp)
         (struct-out ref-exp)
         (struct-out param-exp)
         (struct-out lambda-exp)
         (struct-out prim-exp)
         (struct-out call-exp)
         (struct-out if-exp)
         (struct-out set!/k-exp)
         (struct-out halt-exp)
         parse
         labeled
         fold/exp)

; AE ::= constant
;     |  symbol
;     |  (lambda (symbol ...) E)
; E  ::= (prim op AE AE ...)
;     |  (AE AE ...)
;     |  (if AE E E)
;     |  (set!/k symbol AE AE)

(struct exp (label))

(struct constant-exp exp (val))
(struct ref-exp exp (id))
(struct param-exp exp (id))
(struct lambda-exp exp (params body))

(struct prim-exp exp (op args k))
(struct call-exp exp (fun args))
(struct if-exp exp (test then else))
(struct set!/k-exp exp (id val-exp k))

(struct halt-exp exp ())


(define genlab
  (let ([label 0])
    (lambda ()
      (begin0 label (set! label (+ 1 label))))))

(define (parse e)
  (match e
    [`(quote ,datum)
     (constant-exp (genlab) e)]
    [`(prim ,op ,args ... ,k)
     (let ([args (map parse args)]
           [k (parse k)])
       (prim-exp (genlab) op args k))]
    [`(halt)
     (halt-exp (genlab))]
    [`(set!/k ,id ,val-exp ,cont)
     (let ([id (parse id)]
           [val-exp (parse val-exp)]
           [cont (parse cont)])
       (set!/k-exp (genlab) id val-exp cont))]
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
    [(prim-exp label op args k)
     `(label ,label (prim ,op ,@(map labeled args) (labeled k)))]
    [(call-exp label fun args)
     `(label ,label (,(labeled fun) ,@(map labeled args)))]
    [(if-exp label test then else)
     `(label ,label (if ,(labeled test) ,(labeled then) ,(labeled else)))]
    [(set!/k-exp label id exp k)
     `(label ,label (set!/k ,(labeled id) ,(labeled exp) ,(labeled k)))]
    [(halt-exp label)
     `(label ,label `(halt))]))


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
            [result (fold/exp f result body)])
       (for/fold ([result result])
                 ([param params])
         (fold/exp f result param)))]
    [(prim-exp label op args k)
     (let* ([result (f e result)]
            [result (for/fold ([result result])
                              ([arg args])
                      (fold/exp f result arg))])
       (fold/exp f result k))]
    [(call-exp label fun args)
     (let* ([result (f e result)]
            [result (fold/exp f result fun)])
       (for/fold ([result result])
                 ([arg args])
         (fold/exp f result arg)))]
    [(if-exp label test then else)
     (let* ([result (f e result)]
            [result (fold/exp f result test)]
            [result (fold/exp f result then)])
       (fold/exp f result else))]
    [(set!/k-exp label id val-exp cont)
     (let* ([result (f e result)]
            [result (fold/exp f result id)]
            [result (fold/exp f result val-exp)])
       (fold/exp f result cont))]
    [(halt-exp label)
     (f e result)]))
