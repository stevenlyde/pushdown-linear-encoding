; Desugars a subset of Scheme into a simplified CPS IR
; Copyright (c) Thomas Gilray, 2011-2014


; TODO : handles quotes (i.e. 'hello and '()) improperly
#lang racket


; determines if its input is a primitive function
(define (primop? op)
  (member op
   '(+ - / * expt modulo quotient > < >= <= = equal? eq?
     if print string-append string-length substring
     void sqrt pretty-print max min list? gensym
     list cons null? car cdr append length assq list?
     not number? reverse memq member vector->list
     make-vector vector-length vector-ref vector-set!
     exact->inexact fl+ fl- fl/ fl* fl> flsin boolean?)))


; input language
; --------
; P := ((D | E) ... E)
; D := (define (symbol ...) E)
;    | (define symbol E)
; E := constant
;    | symbol
;    | (lambda (symbol ...) E)
;    | (letrec ((symbol E) ...) E)
;    | (let ((symbol E) ...) E)
;    | (if E E E)
;    | (begin E ...)
;    | (cond (symbol E) ... [else E])
;    | (set! E E)
;    | (E E ...)


; Takes a list of definitions in the input language and returns a single nested expression
(define (collapse-defines e)
        (if (null? (cdr e))
            (car e)
            (if (and (list? (car e)) (equal? (first (car e)) 'define))
                (if (symbol? (second (car e)))
                    `(letrec ((,(second (car e)) ,(third (car e))))
                          ,(collapse-defines (cdr e)))
                    `(letrec ((,(car (second (car e))) (lambda ,(cdr (second (car e))) ,(third (car e)))))
                          ,(collapse-defines (cdr e))))
                `(begin ,(car e) ,(collapse-defines (cdr e))))))
    

; After definitions are collapsed
; -----------
; E := constant
;    | symbol
;    | (lambda (symbol ...) E)
;    | (letrec ((symbol E) ...) E)
;    | (let ((symbol E) ...) E)
;    | (if E E E)
;    | (begin E ...)
;    | (cond (symbol E) ... [else E])
;    | (set! symbol E)
;    | (E E ...)


; Simplifies the language by removing cond, begin, 
(define (simplify e)
        (match e
               ; Quote
               [`(quote ,(? list? eq))
                `(prim list . ,(map simplify (map (lambda (eq+) `(quote ,eq+)) eq)))]
               [`(quote ,(? number? n)) n]
               [`(quote ,(? boolean? b)) b]
          
               ; Lambda 
               [`(lambda ,args ,eb)
                `(lambda ,args ,(simplify eb))]
          
               ; Empty letrec
               [`(letrec () ,eb)
                (simplify eb)]
               
               ; letrec (desugars into a stack of lets+set!s)
               [`(letrec ,lets ,eb)
                `(let ((,(first (car lets)) (prim void)))
                      ,(simplify `(begin (set! ,(first (car lets)) ,(second (car lets))) (letrec ,(cdr lets) ,eb))))]
          
               ; Empty let
               [`(let () ,eb)
                (simplify eb)]
               
               ; let (desugars into a stack of lets)
               [`(let ,lets ,eb)
                `(let ((,(first (car lets)) ,(simplify (second (car lets))))) ,(simplify `(let ,(cdr lets) ,eb)))]
               
               ; if
               [`(if ,ec ,et ,ef)
                `(if ,(simplify ec) ,(simplify et) ,(simplify ef))]
               
               ; and (short circut evaluation)
               [`(and ,e1 ,e2)
                `(if ,(simplify e1) ,(simplify e2) #f)]
               [`(and ,e1 ,es ...)
                `(if ,(simplify e1) ,(simplify `(and . ,es)) #f)]
               
               ; or (short circut evaluation)
               [`(or ,e1 ,e2)
                `(if ,(simplify e1) #t ,(simplify e2))]
               [`(or ,e1 ,es ...)
                `(if ,(simplify e1) #t ,(simplify `(or . ,es)))]
               
               ; begin (desugars into an invokation)
               [`(begin ,es ... ,er)
                `((lambda ,(map (lambda (na) (gensym 'na)) es) ,(simplify er))
                  .
                  ,(map simplify es))]
          
               ; cond (base case)
               [`(cond)
                `(prim void)]
          
               ; cond (else case)
               [`(cond (else ,eb))
                (simplify eb)]     
          
               ; cond (desugars into if)
               [`(cond (,ec ,er) ,more ...)
                `(if ,(simplify ec) ,(simplify er) ,(simplify `(cond . ,more)))]
               
               ; set!
               [`(set! ,x ,eb)
                `(set! ,x ,(simplify eb))]
               
               ; call-site (desugars into a user call-site or a prim)
               [`(,es ...)
                (if (primop? (first es))
                    `(prim . ,(map simplify es))
                    (map simplify es))]
          
               ; symbols/constants
               [else e]))


; After simplification
; -----------
; E := constant
;    | symbol
;    | (let ((symbol E)) E)
;    | (lambda (symbol ...) E)
;    | (set! symbol E)
;    | (if E E E)
;    | (E E ...)
;    | (prim op E ...)


; A-Normalizes; adapted from Flanagan et al.
(define (anf-convert e)
        (define (normalize-name e k)
                (normalize e (lambda (norm) (if (list? norm)
                                                (let ([t (gensym 'nn)])
                                                     `(let ((,t ,norm)) ,(k t)))
                                                (k norm)))))
        (define (normalize-name* e* k)
                (if (null? e*)
                    (k '())
                    (normalize-name (car e*)
                                    (lambda (t) (normalize-name* (cdr e*)
                                                                 (lambda (t*) (k `(,t . ,t*))))))))
        (define (normalize e k)
                (match e
                       [`(quote ,eq) (k e)]
                  
                       [`(lambda ,args ,eb)
                         (k `(lambda ,args ,(anf-convert eb)))]
                       
                       [`(let ((,x ,elet)) ,eb)
                         (normalize elet 
                                    (lambda (norm)
                                            `(let ((,x ,norm)) ,(normalize eb k))))]
                       
                       [`(if ,ec ,et ,ef)
                         (normalize-name ec
                                         (lambda (name) 
                                                 (k `(if ,name ,(anf-convert et) ,(anf-convert ef)))))]
                       
                       [`(set! ,x ,eb)
                         (normalize-name eb
                                         (lambda (name) 
                                                 `(let ((,(gensym 'na) (set! ,x ,name))) ,(k `(prim void)))))]
                       
                       [`(prim ,op ,e* ...)
                         (normalize-name* e*
                                          (lambda (name*)
                                                  (k `(prim ,op . ,name*))))]
                       
                       [`(,ef ,e* ...)
                         (normalize-name ef
                                         (lambda (name) 
                                                 (normalize-name* e* 
                                                                  (lambda (name*)
                                                                          (k `(,name . ,name*))))))]
                       
                       [else (k e)]))
        (normalize e (lambda (x) x)))


; After ANF conversion
; -----------
; AE := constant
;     | symbol
;     | (lambda (symbol ...) E)
; E  := (let ((symbol E)) E)
;     | (prim op AE ...)
;     | (AE AE ...)
;     | (if AE E E)
;     | (set! symbol AE)
;     | AE


(define (cps-convert e)
        (define (T-ae ae)
                (match ae
                       [`(lambda ,x* ,e)
                        (define lamk (gensym 'lamk))
                        `(lambda (,@x* ,lamk) ,(T-e e lamk))]
                       [else ae]))
        (define (T-e e k)
                (match e
                       [`(quote ,s) `(,k ,(T-ae e))]
                       [`(if ,ae ,et ,ef)
                        `(if ,(T-ae ae) ,(T-e et k) ,(T-e ef k))]
                       [`(set! ,x ,ae)
                        `(set!/k ,x ,(T-ae ae) ,k)]
                       [`(prim ,op ,aes ...)
                        `(prim ,op ,@(map T-ae aes) ,k)]
                       [`(let ((,x ,e)) ,eb)
                        (define letk (gensym 'letk))
                        (T-e e `(lambda (,x) ,(T-e eb k)))]
                       [`(lambda . ,_)
                        `(,k ,(T-ae e))]
                       [(? list?)
                        `(,@(map T-ae e) ,k)]
                       [else `(,k ,(T-ae e))]))
        (T-e e `(lambda (v) (halt))))


; After CPS conversion
; -----------
; AE := constant
;     | symbol
;     | (lambda (symbol ...) E)
; E  := (prim op AE AE ...)
;     | (AE AE ...)
;     | (if AE E E)
;     | (set!/k symbol AE AE)


; Alpha-rename (every binding uses a unique variable name)
(define (alpha-rename e)
        (define (rename e env pos)
                (match e
                       ; quote
                       [`(quote ,eq)
                        `(,e . ,pos)]
                  
                       ; lambda
                       [`(lambda ,args ,eb)
                        (let ([env+ (car (foldl (lambda (arg env/n) 
                                                        `(,(hash-set (car env/n)
                                                                     arg
                                                                     (string->symbol (string-append "a" (number->string (cdr env/n)))))
                                                          . ,(+ (cdr env/n) 1)))
                                                `(,env . ,pos)
                                                args))])
                             (let ([soln (rename eb env+ (+ pos (length args)))])
                                  `((lambda ,(car (rename args env+ pos)) ,(car soln)) . ,(cdr soln))))]
                       
                       ; let
                       [`(let ((,x ,e1)) ,e2)
                         (let* ([x+ (string->symbol (string-append "a" (number->string pos)))]
                                [env+ (hash-set env x x+)]
                                [pos+ (+ pos 1)]
                                [e1+ (rename e1 env+ pos+)]
                                [e2+ (rename e2 env+ (cdr e1+))])
                                `((let ((,x+ ,(car e1+))) ,(car e2+)) . ,(cdr e2+)))]
                       
                       ; if
                       [`(if ,ae ,es ...)
                        (let* ([ae+ (rename ae env pos)]
                               [es+ (rename es env (cdr ae+))])
                              `(,(append `(if ,(car ae+)) (car es+)) . ,(cdr es+)))]
                  
                       ; call-site, prim, set!
                       [`(,es ...)
                        (foldl (lambda (e acc) (let ([soln (rename e env (cdr acc))])
                                                    `(,(append (car acc) (list (car soln))) . ,(cdr soln))))
                               `(() . ,pos)
                               es)]
                       
                       ; symbol
                       [(? symbol?) `(,(hash-ref env e (lambda () e)) . ,pos)]
                       
                       ; constant
                       [else `(,e . ,pos)]))
            
        (car (rename e (hash) 0)))


; Reads all s-exdpressions from a file
(define (read-all)
        (let ([e (read)])
             (if (equal? e eof)
                 '()
                 `(,e . ,(read-all)))))


; Read from stdin, translate, and print to stdout
;(display (simplify (collapse-defines (read))))
(pretty-write
  ;(alpha-rename
    (cps-convert
      (anf-convert
        (simplify
          (collapse-defines
            (read-all)
          )
        )
      )
    )
  ;)
)
(newline)

