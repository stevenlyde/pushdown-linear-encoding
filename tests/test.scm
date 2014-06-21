

; Home spun foldl which allows us to employ and test additional user-land higher-order functions
(define (myfold foldf acc lst)
        (if (null? lst)
            acc
            (myfold foldf (foldf (car lst) acc) (cdr lst))))


(define symbs '(1 2 a b 3))

(pretty-print (myfold (lambda (s lst) (if (equal? s 'a) (cons 'YAY lst) (cons s lst))) '() symbs))


