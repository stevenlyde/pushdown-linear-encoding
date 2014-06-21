; Adapted from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAboutR6.html


(define (four1 data)
  (let ((n (vector-length data))
        (pi*2 6.28318530717959))
    
    (begin 
      (letrec ((loop1 (lambda (i j)
        (if (< i n)
          (begin
            (if (< i j)
              (begin
                (let ((temp (vector-ref data i)))
                  (begin (vector-set! data i (vector-ref data j))
                         (vector-set! data j temp)))
                (let ((temp (vector-ref data (+ i 1))))
                  (begin (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
                         (vector-set! data (+ j 1) temp))))
              (void))
            (letrec ((loop2 (lambda (m j)
                (if (and (>= m 2) (>= j m))
                  (loop2 (/ m 2) (- j m))
                  (loop1 (+ i 2) (+ j m))))))
              (loop2 (/ n 2) j)))
          (void)))))
        (loop1 0 0))

    (letrec ((loop3 (lambda (mmax)
      (if (< mmax n)
        (let ((theta
                (fl/ pi*2 (exact->inexact mmax))))
          (let ((wpr
                (letrec ((x (flsin (fl* 0.5 theta))))
                  (fl* -2.0 (fl* x x))))
               (wpi
                (flsin theta)))
          (begin
          (letrec ((loop4 (lambda (wr wi m)
            (if (< m mmax)
              (letrec ((loop5 (lambda (i)
                  (if (< i n)
                    (let ((j (+ i mmax)))
                      (let ((tempr
                             (fl-
                               (fl* wr (vector-ref data j))
                               (fl* wi (vector-ref data (+ j 1)))))
                            (tempi
                              (fl+
                                (fl* wr (vector-ref data (+ j 1)))
                                (fl* wi (vector-ref data j)))))
                      (begin
                      (vector-set! data j
                        (fl- (vector-ref data i) tempr))
                      (vector-set! data (+ j 1)
                        (fl- (vector-ref data (+ i 1)) tempi))
                      (vector-set! data i
                        (fl+ (vector-ref data i) tempr))
                      (vector-set! data (+ i 1)
                        (fl+ (vector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)))))
                (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
                       (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
                       (+ m 2))))))
                 (loop5 m))
          (void)))))
        (loop4 1.0 0.0 0))
      (loop3 (* mmax 2)))))
      (void)))))
    (loop3 2)))))

(define data
  (make-vector 1024 0.0))
 
(define (run data)
  (begin (four1 data)
         (vector-ref data 0)))

(print (run (make-vector 1024 0.5)))



