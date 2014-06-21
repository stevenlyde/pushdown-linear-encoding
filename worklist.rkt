#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define args (current-command-line-arguments))

(define program (open-input-file (vector-ref args 0)))
(define store-matrix (open-input-file (vector-ref args 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

(define root (process-input (read program)))



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
(define lenL (length L)) 
(define lenA (length A)) 
(define lenS (length S)) 
(define lenX (length X)) 



(display "Labels: ")
(pretty-print saved)
(newline)



;;;;;;;;;;;;;;;;;;;;;;; WORKLIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (atomic ael store)
        (define ae (hash-ref saved ael))
        (match ae
               [#t (set 'TRUE)]
               [#f (set 'FALSE)]
               [`(quote ,s) (set ael)]
               [(? number?) (set ael)]
               [(? symbol?) (hash-ref store ae (lambda () (set)))]
               [`(lambda ,args ,eb) (set ae)]))


(define (delta op aevs)
        (match (case op ((+ - * / sqrt expt max min exact->inexact fl+ fl- fl/ fl* flsin length vector-length modulo) 'NUM) 
                        ((void print pretty-print vector-set!) 'VOID) 
                        ((< > <= >= = null? equal? not fl< fl> list? number? boolean?) 'BOOL) 
                        ((list append cons cdr car reverse memq member vector-ref make-vector) 'LIST))
               ['NUM (set 'INT)]
               ['VOID (set 'VOID)]
               ['BOOL (set 'TRUE 'FALSE)]
               ['LIST (set-union (foldl set-union (set) aevs) (set 'LIST))]))


(define (store-join st1 st2)
        (foldl (lambda (k sigma)
                       (hash-set sigma k (set-union (hash-ref st1 k (lambda () (set))) 
                                                    (hash-ref st2 k (lambda () (set))))))
               (hash)
               (set->list (set-union (list->set (hash-keys st1)) (list->set (hash-keys st2))))))


(define (successors state)
        (match state [`(,l ,sigma)
                      (define e (hash-ref saved l))
                      (match e
                             [`(halt) '()]
                             [`(prim ,op ,aes ... ,aek)
                              (define fv (atomic aek sigma)) 
                              (define aevs (map (lambda (ae) (atomic ae sigma)) aes)) 
                              (define clos (filter (lambda (x) x) 
                                                   (map (lambda (e) (if (and (list? e) 
                                                                             (equal? (first e) 'lambda)
                                                                             (= (length (second e)) 1))
                                                                        e
                                                                        #f))
                                                        (set->list fv))))
                              (map (lambda (lam)
                                           `(,(third lam) 
                                             ,(store-join sigma (hash (hash-ref saved (first (second lam))) (delta op aevs)))))
                                   clos)]
                             [`(set!/k ,xl ,ae ,aek)
                              (define fv (atomic aek sigma))
                              (define aev (atomic ae sigma))
                              (define clos (filter (lambda (x) x) 
                                                   (map (lambda (e) (if (and (list? e) 
                                                                             (equal? (first e) 'lambda)
                                                                             (= (length (second e)) 1))
                                                                        e
                                                                        #f))
                                                        (set->list fv))))
                              (map (lambda (lam)
                                           `(,(third lam) 
                                             ,(store-join sigma (hash (hash-ref saved xl) aev (hash-ref saved (first (second lam))) (set 'VOID)))))
                                   clos)]
                             [`(if ,ae ,et ,ef)
                              (define aev (atomic ae sigma)) 
                              (foldl (lambda (pred el acc) 
                                             (if (pred aev)
                                                 (cons `(,el ,sigma) acc)
                                                 acc)) 
                                     '() 
                                     (list (lambda (aev) (> (set-count (set-subtract aev (set 'FALSE))) 0)) (lambda (aev) (set-member? aev 'FALSE)))
                                     `(,et ,ef))]
                             [`(,aef ,aes ...)
                              (define fv (atomic aef sigma)) 
                              (define aevs (map (lambda (ae) (atomic ae sigma)) aes))  
                              (define clos (filter (lambda (x) x)
                                                   (map (lambda (e) (if (and (list? e) 
                                                                             (equal? (first e) 'lambda))
                                                                        e
                                                                        #f))
                                                        (set->list fv))))
                              (map (lambda (lam)
                                           (let ([minarg (min (length (second lam)) (length aevs))])  ;; in the future we want to fix this on the GPU side to match only closures
                                           `(,(third lam)                                             ;;    which take the right number of parameters 
                                             ,(store-join sigma (foldl (lambda (xl v h) (hash-set h (hash-ref saved xl) v)) (hash) (take (second lam) minarg) (take aevs minarg))))))
                                   clos)])]))


(define (explore reachable sigma)
        ;(pretty-print `(explore ,reachable sigma))
        (let* ([updated (foldl (lambda (inc acc) `(,(set-union (first inc) (first acc)) ,(store-join (second inc) (second acc))))
                               `(,reachable ,sigma)
                               (map (lambda (next)
                                            (let* ([succs (successors `(,next ,sigma))]
                                                   [reachable+ (list->set (foldl (lambda (succ r+) (cons (first succ) r+)) '() succs))]
                                                   [sigma+ (foldl store-join sigma (foldl (lambda (succ stores) (cons (second succ) stores)) '() succs))])
                                                  `(,reachable+ ,sigma+)))
                                    (set->list reachable)))]
               [sigma+ (second updated)]
               [reachable+ (first updated)])
              (if (and (equal? reachable reachable+) (equal? sigma sigma+))
                  sigma
                  (explore reachable+ sigma+))))


(define sigma (explore (set root) (hash)))

;(pretty-print sigma)
;(exit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define msigma (hash))

(define (matrix-store in)
  (define row (read in))
  (when (not (eof-object? row))
    (define col (read in))
    
    ; should only print if row < (length X*T)
    (when (< row lenX)
      (define x (list-ref X row))
      (define value (list-ref V col))
      
      (set! msigma (store-join msigma (hash x (set (if (and (number? value) (let ([e (hash-ref saved value)]) (and (list? e) (member (first e) '(lambda))))) (hash-ref saved value) value))))))
    
    (matrix-store in)))

; read the matrix name and number of rows and columns
(define _ (read store-matrix))
(define store-num-rows (read store-matrix))
(define store-num-cols (read store-matrix))
(matrix-store store-matrix)


(call-with-output-file "worklist-output.txt" (lambda (port) (pretty-write (foldl (lambda (k h) (hash-set h k (set->list (hash-ref sigma k)))) 
                                                                                (hash) 
                                                                                (hash-keys sigma)) 
                                                           port)
                                                           (pretty-write (foldl (lambda (k h) (hash-set h k (set->list (hash-ref msigma k)))) 
                                                                                (hash) 
                                                                                (hash-keys msigma)) 
                                                           port))

                                            #:mode 'text #:exists 'replace)




