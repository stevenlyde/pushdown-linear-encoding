((lambda (true)
   ((lambda (false) 
      ((lambda (f1)
         (let ((d1 (f1 true)))
           (f1 false)))
       (lambda (x1)
         ((lambda (f2)
            (let ((d2 (f2 true)))
              (f2 false)))
          (lambda (x2)
            ((lambda (z)
               (let ((f (z x1)))
                 (f x2)))
             (lambda (y1)
               (lambda (y2) y1))))))))
    (lambda (ff) ff)))
 (lambda (tt) tt))