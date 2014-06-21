; Adapted from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAboutR6.html


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(print (fib 20))



