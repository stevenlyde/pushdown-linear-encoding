; Adapted from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAboutR6.html



(define trace? #f)

(define (loop i l)
        (if (= i 0) l (loop (- i 1) (cons i l))))

(define (iota1 n)
    (loop n (list)))
    
(define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

(define (my-try x y z)
    (if (null? x)
      (if (null? y)
        (begin (if trace? (print z) (void)) 1)
        0)
      (+ (if (ok? (car x) 1 z)
           (my-try (append (cdr x) y) (list) (cons (car x) z))
           0)
         (my-try (cdr x) (cons (car x) y) z))))
           
(define (nqueens n)
  (my-try (iota1 n) (list) (list)))


      
(print (nqueens 7))



