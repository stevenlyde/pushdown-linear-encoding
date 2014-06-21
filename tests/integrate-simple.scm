


; Home spun foldl which allows us to employ and test additional user-land higher-order functions
(define (myfold foldf acc lst)
        (if (null? lst)
            acc
            (myfold foldf (foldf (car lst) acc) (cdr lst))))

; A function for numerical integration over fun: Num -> Num between start and stop with delta precision/step using a quadrature rule
(define (integrate fun rule)
        (rule fun 77))

; a function taking a function, pair of points, and a delta, which returns the area of that slice by the midpoint rule
(define (midpoint_rule func points)
        0)


; All rules
(define rules (list midpoint_rule))


; Some functions to integrate over
(define funcs (list (lambda (x) (sqrt x))
                    ))


(myfold (lambda (rule na) (myfold (lambda (afunc na) (pretty-print (integrate afunc rule))) na funcs)) (void) rules)


