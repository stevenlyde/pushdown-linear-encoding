#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define worklist-output (open-input-file "worklist-output.txt"))

(define wsigmal (read worklist-output))
(define msigmal (read worklist-output))

(define wsigma (foldl (lambda (k h) (hash-set h k (list->set (hash-ref wsigmal k)))) (hash) (hash-keys wsigmal)))
(define msigma (foldl (lambda (k h) (hash-set h k (list->set (hash-ref msigmal k)))) (hash) (hash-keys msigmal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Compare the two
(display "Differences listed below: ")
(newline)
(void (map (lambda (k) (define mset (hash-ref msigma k (lambda () (set))))
                       (define wset (hash-ref wsigma k (lambda () (set))))
                       (define inter (set-intersect mset wset))
                       (when (not (equal? mset wset))
                             (print `(intersect ,k ,inter)) (newline)
                             (print `(cpu-sigma ,k ,(set-subtract wset inter))) (newline)
                             (print `(gpu-sigma ,k ,(set-subtract mset inter))) (newline)
                             (newline) (newline)))
           (set->list (set-union (list->set (hash-keys msigma)) (list->set (hash-keys wsigma))))))



