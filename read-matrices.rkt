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
(define lenL (length L)) 
(define lenA (length A)) 
(define lenS (length S)) 
(define lenX (length X)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(display "Labels: ")
(pretty-print saved)
(newline)

(define (print-store in)
  (define row (read in))
  (when (not (eof-object? row))
    (define col (read in))
    
    ; should only print if row < (length X*T)
    (when (< row lenX)
      (define x (list-ref X row))
      (define value (list-ref V col))
      
      (printf "~a ==> ~a ~n" x value))
    
    (print-store in)))

; read the matrix name and number of rows and columns
(define _ (read store-matrix))
(define store-num-rows (read store-matrix))
(define store-num-cols (read store-matrix))
(print-store store-matrix)
