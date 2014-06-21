#lang racket


;; Read the desugared source tree and flatten it, assigning a number to each language form

(define label 0)
(define saved (make-hash))

(define (save! e)
        (define l label)
        (set! label (+ label 1))
        (hash-set! saved l e)
        l)

(define (process-input e)
        (match e
               [`(quote ,s) (save! e)]
               [`(prim ,op ,ae* ...)
                (save! `(prim ,op . ,(map process-input ae*)))]
               [`(halt)
                (save! `(halt))]
               [`(set!/k ,x ,aev ,aek)
                (save! `(set!/k ,(process-input x) ,(process-input aev) ,(process-input aek)))]
               [`(if ,ae ,et ,ef)
                (save! `(if ,(process-input ae) ,(process-input et) ,(process-input ef)))]
               [`(lambda ,args ,eb)
                (save! `(lambda ,(map process-input args) ,(process-input eb)))]
               [`(,ae* ...)
                (save! (map process-input ae*))]
               [else (save! e)]))

(define (pretty-program [l root])
        (define e (hash-ref saved l))
        (match e
               [`(quote ,s) `(label ,l ,e)]
               [`(halt)
                `(label ,l `(halt))]
               [`(prim ,op ,ae* ...)
                `(label ,l (prim ,op . ,(map pretty-program ae*)))]
               [`(set!/k ,x ,aev ,aek)
                `(label ,l (set!/k ,(pretty-program x) ,(pretty-program aev) ,(pretty-program aek)))]
               [`(if ,ae ,et ,ef)
                `(label ,l (if ,(pretty-program ae) ,(pretty-program et) ,(pretty-program ef)))]
               [`(lambda ,args ,eb)
                `(label ,l (lambda ,(map pretty-program args) ,(pretty-program eb)))]
               [(? list?)
                `(label ,l ,(map pretty-program e))]
               [else `(label ,l ,e)]))


(define root (process-input (read)))



;; Utility for iterating over the program
(define (iter callback! [l root])
        (define e (hash-ref saved l))
        (callback! l e)
        (match e
               [`(quote ,s) (void)]
               [`(halt) (void)]
               [`(prim ,op ,ae* ...)
                 (map (lambda (l) (iter callback! l)) ae*)]
               [`(set!/k ,x ,aev ,aek)
                 (iter callback! x)
                 (iter callback! aev)
                 (iter callback! aek)]
               [`(if ,ae ,et ,ef)
                 (iter callback! ae)
                 (iter callback! et)
                 (iter callback! ef)]
               [`(lambda ,args ,eb)
                 (iter callback! eb)
                 (map (lambda (l) (iter callback! l)) args)]
               [(? list?)
                (map (lambda (l) (iter callback! l)) e)]
               [else (void)])
        (void))


; Valid PrimList opsA
(define primlistops '(list append cons cdr car reverse memq member vector-ref make-vector))


; ARGSN
(define ARGSN 2)
(define (build-ARGSN! l e)
        ; e is a primlist
        (when (and (list? e) (equal? (first e) 'prim) (member (second e) primlistops))
              (set! ARGSN (max ARGSN (- (length e) 3))))
        ; e is a callsite
        (when (and (not (or (not (list? e))
                            (member (first e) '(prim set!/k if lambda halt))))
                   (> (- (length e) 1) 0))
              (set! ARGSN (max ARGSN (- (length e) 1)))))
(iter build-ARGSN!)


; primlistmax
(define primlistmax 0)
(define (build-primlistmax! l e)
        (when (and (list? e) (equal? (first e) 'prim) (member (second e) primlistops))
              ; e is a primlist
              (when (> (- (length e) 3) 0)
                    (set! primlistmax (max primlistmax (- (length e) 3))))))
(iter build-primlistmax!)


;; Build S

(define S '())
(define (build-S! l e)
        (when (and (list? e) (not (member (first e) '(lambda quote))))
              (set! S (cons l S))))
(iter build-S!)


;; Build X

(define X '())
(define (build-X! l e)
        (when (and (symbol? e) (not (equal? e 'halt)))
              (when (not (member e X))
                    (set! X (cons e X)))))
(iter build-X!)


;; Build LAM and FREE and V

(define LAM (set))
(define (build-LAM! l e)
        (when (and (list? e) (equal? (first e) 'lambda))
              ; e is a lambda-abstraction
              (set! LAM (set-add LAM l))))
(iter build-LAM!)
(define L (set->list LAM))


(define INTLOCS (set))
(define (build-INTLOCS! l e)
        (when (number? e)
              ; e is a literal
              (set! INTLOCS (set-add INTLOCS l))))
(iter build-INTLOCS!)
(define I (set->list INTLOCS))

(define SYMLOCS (set))
(define (build-SYMLOCS! l e)
        (when (and (list? e) (equal? (first e) 'quote))
              ; e is a literal
              (set! SYMLOCS (set-add SYMLOCS l))))
(iter build-SYMLOCS!)
(define SY (set->list SYMLOCS))

(define B '(SYM LIST VOID TRUE FALSE INT))
(define V (append L I SY B))
(define A (append X V))


(define allprims (set))
(define (build-allprims! l e)
        (when (and (list? e) (equal? (first e) 'prim))
              (set! allprims (set-add allprims (second e)))))
(iter build-allprims!)


; lengths
(define lenV (length V)) 
(define lenI (length I)) 
(define lenSY (length SY)) 
(define lenL (length L)) 
(define lenA (length A)) 
(define lenS (length S)) 
(define lenX (length X)) 



;;;;;;; DEBUG
;(pretty-print (pretty-program))
;(pretty-print C)
;(pretty-print S)
;(pretty-print A)
;(pretty-print X*T)
;(pretty-print CLO)
;(pretty-print X)
;(pretty-print T)
;(pretty-print (hash-ref saved 8706))
;(pretty-print root)


; print out all prims
;(pretty-print allprims)

;(pretty-print saved)
;(pretty-print (list-ref V 96))



;; Table building Utils
(define (find-clo l)
        (define (find-clo-rem rem n)
                (if (null? rem)
                    (error `(NO-SUCH-CLOSURE ,l))
                    (if (equal? l (car rem))
                        n
                        (find-clo-rem (cdr rem) (+ n 1)))))
        (find-clo-rem L 0))

(define (find-state l)
        (define (find-state-rem rem n)
                (if (null? rem)
                    (error `(NO-SUCH-STATE ,l))
                    (if (equal? l (car rem))
                        n
                        (find-state-rem (cdr rem) (+ n 1)))))
        (find-state-rem S 0))

(define (ae->A ael)
        (define ae (hash-ref saved ael))
        (cond [(symbol? ae)
               (- lenX (length (member ae X)))]
              [(number? ae) (+ lenX lenL (- lenI (length (member ael I))))]
              [(equal? ae #t) (- lenA 3)]  ;;;;;;;;;;; this needs to change as B changes 
              [(equal? ae #f) (- lenA 2)]  ;;;;;;;;;;; this needs to change as B changes 
              [(and (list? ae) (equal? (first ae) 'quote)) (+ lenX lenL lenI (- lenSY (length (member ael SY))))]
              [(and (list? ae) (equal? (first ae) 'lambda))
               (define clooff (find-clo ael))
               (+ lenX clooff)]))

(define (forupto f! i)
        (if (= i 0)
            (void)
            (begin (f! i)
                   (forupto f! (- i 1)))))



;; Write the output file matrices.txt
(define out (open-output-file "matrices.txt" #:mode 'text #:exists 'replace))

;; Write table r
(display "r " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-r cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (equal? root l)
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-r (+ 1 cS) (cdr remS))))
(display-r 0 S)
(newline out)
(pretty-print 13)


;; Write table sigma
(display "sigma " out)
(display lenA out)
(display " " out)
(display lenV out)
(newline out)
(define (display-sigma [n 0])
        (if (< n lenV)
            (begin (display (+ n lenX) out)
                   (display " " out)
                   (display n out)
                   (newline out)
                   (display-sigma (+ n 1)))
            (void)))
(display-sigma)
(newline out)
(pretty-print 12)


;; Write table Fun
(display "Fun " out)
(display lenS out)
(display " " out)
(display lenA out)
(newline out)
(define (display-Fun cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'set!/k))
                    (define aek (last e))
                    (define aoff (ae->A aek))
                    (display cS out)
                    (display " " out)
                    (display aoff out)
                    (newline out))
              (when (and (list? e) (equal? (first e) 'prim))
                    (define aek (last e))
                    (define aoff (ae->A aek))
                    (display cS out)
                    (display " " out)
                    (display aoff out)
                    (newline out))
              (when (not (or (not (list? e))
                             (member (first e) '(prim set!/k if lambda halt))))
                    (define aef (first e))
                    (define aoff (ae->A aef))
                    (display cS out)
                    (display " " out)
                    (display aoff out)
                    (newline out))
              (display-Fun (+ 1 cS) (cdr remS))))
(display-Fun 0 S)
(newline out)
(pretty-print 11)


;; Write table CondTrue
(display "CondTrue " out)
(display lenS out)
(display " " out)
(display lenS out)
(newline out)
(define (display-CondTrue cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'if))
                    (define aet (third e))
                    (define soff (find-state aet))
                    (display cS out)
                    (display " " out)
                    (display soff out)
                    (newline out))
              (display-CondTrue (+ 1 cS) (cdr remS))))
(display-CondTrue 0 S)
(newline out)
(pretty-print 10)


;; Write table CondFalse
(display "CondFalse " out)
(display lenS out)
(display " " out)
(display lenS out)
(newline out)
(define (display-CondFalse cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'if))
                    (define aef (fourth e))
                    (define soff (find-state aef))
                    (display cS out)
                    (display " " out)
                    (display soff out)
                    (newline out))
              (display-CondFalse (+ 1 cS) (cdr remS))))
(display-CondFalse 0 S)
(newline out)
(pretty-print 9)


;; Write table Body
(display "Body " out)
(display lenV out)
(display " " out) 
(display lenS out)
(newline out)
(define (display-Body cV remV)
        (when (not (null? remV))
              (define l (car remV))
              (when (hash-has-key? saved l)
                    (define e (hash-ref saved l))
                    (when (and (list? e) (equal? (first e) 'lambda))
                          (define bodyl (third e))
                          (define soff (find-state bodyl))
                          (display cV out)
                          (display " " out)
                          (display soff out)
                          (newline out)))
              (display-Body (+ 1 cV) (cdr remV))))
(display-Body 0 V)
(newline out)
(pretty-print 7)


;; Write tables Arg_i
(define (write-argi i)
        (display "Arg" out)
        (display i out)
        (display " " out)
        (display lenS out)
        (display " " out)
        (display lenA out)
        (newline out)
        (define (display-Arg cS remS)
                (when (not (null? remS))
                      (define l (car remS))
                      (define e (hash-ref saved l))
                      (when (and (list? e) (equal? (first e) 'set!/k) (< i 3))
                            (define ae (list-ref e i))
                            (define aoff (ae->A ae))
                            (display cS out)
                            (display " " out)
                            (display aoff out)
                            (newline out))
                      (when (and (list? e) (equal? (first e) 'prim) (member (second e) primlistops) (<= i (- (length e) 3)))
                            (define ae (list-ref e (+ 1 i)))
                            (define aoff (ae->A ae))
                            (display cS out)
                            (display " " out)
                            (display aoff out)
                            (newline out))
                      (when (and (list? e) (equal? (first e) 'if) (= i 1))
                            (define ae (list-ref e i))
                            (define aoff (ae->A ae))
                            (display cS out)
                            (display " " out)
                            (display aoff out)
                            (newline out))
                      (when (and (not (or (not (list? e))
                                          (member (first e) '(prim set!/k if lambda halt))))
                                 (< i (length e)))
                             (define aei (list-ref e i))
                             (define aoff (ae->A aei))
                             (display cS out)
                             (display " " out)
                             (display aoff out)
                             (newline out))
                      (display-Arg (+ 1 cS) (cdr remS))))
        (display-Arg 0 S)
        (newline out))
(forupto write-argi (max ARGSN 2))
(pretty-print 6)


;; Write tables Var_i
(define (write-vari i)
        (display "Var" out)
        (display i out)
        (display " " out)
        (display lenV out)
        (display " " out)
        (display lenA out)
        (newline out)
        (define (display-Var cV remV)
                (when (not (null? remV))
                      (define vl (car remV))
                      (when (hash-has-key? saved vl)
                           (define e (hash-ref saved vl))
                           (when (and (list? e) (equal? (first e) 'lambda) (< (- i 1) (length (second e))))
                                 (define ail (list-ref (second e) (- i 1)))
                                 (define xoff (- lenX (length (member (hash-ref saved ail) X))))
                                 (display cV out)
                                 (display " " out)
                                 (display xoff out)
                                 (newline out)))
                      (display-Var (+ 1 cV) (cdr remV))))
        (display-Var 0 V)
        (newline out))
(forupto write-vari ARGSN)
(pretty-print 5)


;; Write Tables Call_i
(define (write-calli i)
        (display "Call" out)
        (display (- i 1) out)
        (display " " out)
        (display lenS out)
        (display " 1" out)
        (newline out)
        (define (display-Call cS remS)
                (when (not (null? remS))
                      (define l (car remS))
                      (define e (hash-ref saved l))
                      (when (and (not (or (not (list? e))
                                          (member (first e) '(prim set!/k if lambda halt))))
                                 (= (- i 1) (- (length e) 1)))
                            (display cS out)
                            (display " 0" out)
                            (newline out))
                      (display-Call (+ 1 cS) (cdr remS))))
        (display-Call 0 S)
        (newline out))
(forupto write-calli (+ 1 ARGSN))
(pretty-print 4)


;; Write table If
(display "If " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-If cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'if))
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-If (+ 1 cS) (cdr remS))))
(display-If 0 S)
(newline out)
(pretty-print 3)


;; Write table Set
(display "Set " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-Set cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'set!/k))
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-Set (+ 1 cS) (cdr remS))))
(display-Set 0 S)
(newline out)
(pretty-print 2)


;; Write table PrimINT
(display "PrimNum " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-PrimINT cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'prim) (member (second e) '(+ - * / sqrt expt max min vector-length length exact->inexact fl+ fl- fl/ fl* flsin modulo)))
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-PrimINT (+ 1 cS) (cdr remS))))
(display-PrimINT 0 S)
(newline out)
(pretty-print 1)


;; Write table PrimBOOL
(display "PrimBool " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-PrimBOOL cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'prim) 
                         (member (second e) '(< > <= >= = equal? null? not and or fl> fl< list? number? boolean?)))
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-PrimBOOL (+ 1 cS) (cdr remS))))
(display-PrimBOOL 0 S)
(newline out)
(pretty-print 0)


;; Write table PrimVOID
(display "PrimVoid " out)
(display lenS out)
(display " 1" out)
(newline out)
(define (display-PrimVoid cS remS)
        (when (not (null? remS))
              (define l (car remS))
              (define e (hash-ref saved l))
              (when (and (list? e) (equal? (first e) 'prim) (member (second e) '(void print pretty-print vector-set!)))
                    (display cS out)
                    (display " 0" out)
                    (newline out))
              (display-PrimVoid (+ 1 cS) (cdr remS))))
(display-PrimVoid 0 S)
(newline out)
(pretty-print 0)



;; Write table PrimLIST
(define (write-primlisti i)
        (display "PrimList" out)
        (display (- i 1) out)
        (display " " out)
        (display lenS out)
        (display " 1" out)
        (newline out)
        (define (display-PrimList cS remS)
                (when (not (null? remS))
                      (define l (car remS))
                      (define e (hash-ref saved l))
                      (when (and (list? e) 
                                 (equal? (first e) 'prim) 
                                 (member (second e) primlistops)
                                 (= (- i 1) (- (length e) 3)))
                            (display cS out)
                            (display " 0" out)
                            (newline out))
                      (display-PrimList (+ 1 cS) (cdr remS))))
        (display-PrimList 0 S)
        (newline out))
(forupto write-primlisti (+ 1 primlistmax))
(pretty-print 1)





