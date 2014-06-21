#lang racket

(require racket/flonum)

(define-syntax set!/k
    (syntax-rules ()
        [(_ x ae k)
         (k (set! x ae))]))


(define (prim op . aes)
        (let ([k (first (reverse aes))]
              [args (reverse (cdr (reverse aes)))])
             (k (apply op args))))
             
             
(define halt exit)
             






