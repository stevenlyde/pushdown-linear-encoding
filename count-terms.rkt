#lang racket


(define count 0)

(define (counter! e)
        (match e
               [`(set!/k ,aes ...) 
                 (set! count (+ 1 count))
                 (map counter! aes)]
               [`(prim ,aes ...) 
                 (set! count (+ 1 count))
                 (map counter! aes)]
               [`(if ,aes ...) 
                 (set! count (+ 1 count))
                 (map counter! aes)]
               [`(lambda ,args ,aes ...) 
                 (set! count (+ 1 count))
                 (map counter! aes)]
               [(? list?)
                (set! count (+ count 1))
                (map counter! e)]
               [else (void)]))

(void (counter! (read)))
(pretty-print count)




