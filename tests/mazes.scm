; Adapted from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAboutR6.html (by Marc Feeley)


(define odd?
  (lambda (n)
    (= 1 (modulo n 2))))

(define even?
  (lambda (n)
    (= 0 (modulo n 2))))
    
(define map
  (lambda (f lst)
    (if (null? lst)
      lst
      (cons (f (car lst)) (map f (cdr lst))))))

(define foldr
  (lambda (f base lst)
    (letrec ((foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst)))))))
    (foldr-aux lst))))

(define foldl
  (lambda (f base lst)
    (letrec ((foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst))))))
    (foldl-aux base lst))))

(define for
  (lambda (lo hi f)
    (letrec ((for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            (list)))))
    (for-aux lo))))

(define concat
  (lambda (lists)
    (foldr (lambda (a b) (append a b)) (list) lists)))

(define list-read
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read (cdr lst) (- i 1)))))

(define list-write
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write (cdr lst) (- i 1) val)))))

(define list-remove-pos
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))

(define member-of
  (lambda (e lst)
    (if (null? lst)
      #f
      (if (equal? e (car lst))
        #t
        (member-of e (cdr lst))))))

(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member-of (car lst) (cdr lst))
            (duplicates? (cdr lst))))))

(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j)))))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map
  (lambda (f mat)
    (map (lambda (lst) (map f lst)) mat)))

(define initial-random 0)

(define next-random
  (lambda (current-random)
    (modulo (+ (* current-random 3581) 12751) 131072)))

(define neighboring-cavities
  (lambda (pos cave)
    (let ((size (matrix-size cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read cave (- i 1) j))
                      (list (cons (- i 1) j))
                      (list))
                  (if (and (< i (- n 1)) (matrix-read cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      (list))
                  (if (and (> j 0) (matrix-read cave i (- j 1)))
                      (list (cons i (- j 1)))
                      (list))
                  (if (and (< j (- m 1)) (matrix-read cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      (list))))))))

(define shuffle-aux
  (lambda (lst current-random)
    (if (null? lst)
        (list)
        (let ((new-random (next-random current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux (list-remove-pos lst i)
                               new-random)))))))

(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))

(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux c nc new-cavity-id old-cavity-id))
                   (matrix-write cave i j new-cavity-id)
                   (neighboring-cavities pos cave))
            cave)))))

(define change-cavity
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))

(define pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write cave i j pos))))

(define try-to-pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities pos cave)))
        (if (duplicates?
             (map (lambda (nc) (matrix-read cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define pierce-randomly
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly (cdr possible-holes)
                           (try-to-pierce hole cave))))))

(define make-maze
  (lambda (n m)
    (if (not (and (odd? n) (odd? m)))
        (print 'error)
        (let ((cave
               (make-matrix n m (lambda (i j)
                                  (if (and (even? i) (even? j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat
                (for 0 n (lambda (i)
                           (concat
                            (for 0 m (lambda (j)
                                       (if (equal? (even? i) (even? j))
                                           (list)
                                           (list (cons i j)))))))))))
          (pierce-randomly (shuffle possible-holes) cave)))))


(begin (pretty-print (make-maze 15 15))
       (pretty-print (make-maze 15 15))
       (pretty-print (make-maze 15 15))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5))
       (pretty-print (make-maze 5 5)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(define odd?2
  (lambda (n)
    (= 1 (modulo n 2))))

(define even?2
  (lambda (n)
    (= 0 (modulo n 2))))
    
(define map2
  (lambda (f lst)
    (if (null? lst)
      lst
      (cons (f (car lst)) (map2 f (cdr lst))))))

(define foldr2
  (lambda (f base lst)
    (letrec ((foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst)))))))
    (foldr-aux lst))))

(define foldl2
  (lambda (f base lst)
    (letrec ((foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst))))))
    (foldl-aux base lst))))

(define for2
  (lambda (lo hi f)
    (letrec ((for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            (list)))))
    (for-aux lo))))

(define concat2
  (lambda (lists)
    (foldr (lambda (a b) (append a b)) (list) lists)))

(define list-read2
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read2 (cdr lst) (- i 1)))))

(define list-write2
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write2 (cdr lst) (- i 1) val)))))

(define list-remove-pos2
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos2 (cdr lst) (- i 1))))))

(define member-of2
  (lambda (e lst)
    (if (null? lst)
      #f
      (if (equal? e (car lst))
        #t
        (member-of2 e (cdr lst))))))

(define duplicates?2
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member-of2 (car lst) (cdr lst))
            (duplicates?2 (cdr lst))))))

(define make-matrix2
  (lambda (n m init)
    (for2 0 n (lambda (i) (for2 0 m (lambda (j) (init i j)))))))

(define matrix-read2
  (lambda (mat i j)
    (list-read2 (list-read2 mat i) j)))

(define matrix-write2
  (lambda (mat i j val)
    (list-write2 mat i (list-write2 (list-read2 mat i) j val))))

(define matrix-size2
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map2
  (lambda (f mat)
    (map2 (lambda (lst) (map2 f lst)) mat)))

(define next-random2
  (lambda (current-random)
    (modulo (+ (* current-random 3581) 12751) 131072)))

(define neighboring-cavities2
  (lambda (pos cave)
    (let ((size (matrix-size2 cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read2 cave (- i 1) j))
                      (list (cons (- i 1) j))
                      (list))
                  (if (and (< i (- n 1)) (matrix-read2 cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      (list))
                  (if (and (> j 0) (matrix-read2 cave i (- j 1)))
                      (list (cons i (- j 1)))
                      (list))
                  (if (and (< j (- m 1)) (matrix-read2 cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      (list))))))))

(define shuffle-aux2
  (lambda (lst current-random)
    (if (null? lst)
        (list)
        (let ((new-random (next-random2 current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux2 (list-remove-pos lst i)
                                new-random)))))))

(define shuffle2
  (lambda (lst)
    (shuffle-aux2 lst initial-random)))

(define cave-to-maze2
  (lambda (cave)
    (matrix-map2 (lambda (x) (if x '_ '*)) cave)))

(define change-cavity-aux2
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read2 cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux2 c nc new-cavity-id old-cavity-id))
                   (matrix-write2 cave i j new-cavity-id)
                   (neighboring-cavities2 pos cave))
            cave)))))

(define change-cavity2
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux2 cave pos new-cavity-id (matrix-read2 cave i j)))))

(define pierce2
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write2 cave i j pos))))

(define try-to-pierce2
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities2 pos cave)))
        (if (duplicates?2
             (map2 (lambda (nc) (matrix-read2 cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl2 (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define pierce-randomly2
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly2 (cdr possible-holes)
                           (try-to-pierce2 hole cave))))))

(define make-maze2
  (lambda (n m)
    (if (not (and (odd?2 n) (odd?2 m)))
        (print 'error)
        (let ((cave
               (make-matrix2 n m (lambda (i j)
                                  (if (and (even?2 i) (even?2 j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat2
                (for2 0 n (lambda (i)
                           (concat2
                            (for2 0 m (lambda (j)
                                       (if (equal? (even?2 i) (even?2 j))
                                           (list)
                                           (list (cons i j)))))))))))
          (pierce-randomly2 (shuffle2 possible-holes) cave)))))


(begin (pretty-print (make-maze2 15 15))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4))
       (pretty-print (make-maze2 4 4)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(define odd?3
  (lambda (n)
    (= 1 (modulo n 2))))

(define even?3
  (lambda (n)
    (= 0 (modulo n 2))))
    
(define map3
  (lambda (f lst)
    (if (null? lst)
      lst
      (cons (f (car lst)) (map3 f (cdr lst))))))

(define foldr3
  (lambda (f base lst)
    (letrec ((foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst)))))))
    (foldr-aux lst))))

(define foldl3
  (lambda (f base lst)
    (letrec ((foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst))))))
    (foldl-aux base lst))))

(define for3
  (lambda (lo hi f)
    (letrec ((for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            (list)))))
    (for-aux lo))))

(define concat3
  (lambda (lists)
    (foldr (lambda (a b) (append a b)) (list) lists)))

(define list-read3
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read3 (cdr lst) (- i 1)))))

(define list-write3
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write3 (cdr lst) (- i 1) val)))))

(define list-remove-pos3
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos3 (cdr lst) (- i 1))))))

(define member-of3
  (lambda (e lst)
    (if (null? lst)
      #f
      (if (equal? e (car lst))
        #t
        (member-of3 e (cdr lst))))))

(define duplicates?3
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member-of3 (car lst) (cdr lst))
            (duplicates?3 (cdr lst))))))

(define make-matrix3
  (lambda (n m init)
    (for3 0 n (lambda (i) (for3 0 m (lambda (j) (init i j)))))))

(define matrix-read3
  (lambda (mat i j)
    (list-read3 (list-read3 mat i) j)))

(define matrix-write3
  (lambda (mat i j val)
    (list-write3 mat i (list-write3 (list-read3 mat i) j val))))

(define matrix-size3
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map3
  (lambda (f mat)
    (map3 (lambda (lst) (map3 f lst)) mat)))

(define next-random3
  (lambda (current-random)
    (modulo (+ (* current-random 3581) 12751) 131072)))

(define neighboring-cavities3
  (lambda (pos cave)
    (let ((size (matrix-size3 cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read3 cave (- i 1) j))
                      (list (cons (- i 1) j))
                      (list))
                  (if (and (< i (- n 1)) (matrix-read3 cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      (list))
                  (if (and (> j 0) (matrix-read3 cave i (- j 1)))
                      (list (cons i (- j 1)))
                      (list))
                  (if (and (< j (- m 1)) (matrix-read3 cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      (list))))))))

(define shuffle-aux3
  (lambda (lst current-random)
    (if (null? lst)
        (list)
        (let ((new-random (next-random3 current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux3 (list-remove-pos lst i)
                                new-random)))))))

(define shuffle3
  (lambda (lst)
    (shuffle-aux3 lst initial-random)))

(define cave-to-maze3
  (lambda (cave)
    (matrix-map3 (lambda (x) (if x '_ '*)) cave)))

(define change-cavity-aux3
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read3 cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux3 c nc new-cavity-id old-cavity-id))
                   (matrix-write3 cave i j new-cavity-id)
                   (neighboring-cavities3 pos cave))
            cave)))))

(define change-cavity3
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux3 cave pos new-cavity-id (matrix-read3 cave i j)))))

(define pierce3
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write3 cave i j pos))))

(define try-to-pierce3
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities3 pos cave)))
        (if (duplicates?3
             (map3 (lambda (nc) (matrix-read3 cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl3 (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define pierce-randomly3
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly3 (cdr possible-holes)
                           (try-to-pierce3 hole cave))))))

(define make-maze3
  (lambda (n m)
    (if (not (and (odd?3 n) (odd?3 m)))
        (print 'error)
        (let ((cave
               (make-matrix3 n m (lambda (i j)
                                  (if (and (even?3 i) (even?3 j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat3
                (for3 0 n (lambda (i)
                           (concat3
                            (for3 0 m (lambda (j)
                                       (if (equal? (even?3 i) (even?3 j))
                                           (list)
                                           (list (cons i j)))))))))))
          (pierce-randomly3 (shuffle3 possible-holes) cave)))))


(begin (pretty-print (make-maze3 15 15))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4))
       (pretty-print (make-maze3 4 4)))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define change-cavity-aux4
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read3 cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux3 c nc new-cavity-id old-cavity-id))
                   (matrix-write3 cave i j new-cavity-id)
                   (neighboring-cavities3 pos cave))
            cave)))))

(define change-cavity4
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux4 cave pos new-cavity-id (matrix-read3 cave i j)))))

(define pierce4
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write3 cave i j pos))))

(define try-to-pierce4
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities3 pos cave)))
        (if (duplicates?2
             (map2 (lambda (nc) (matrix-read3 cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl2 (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define pierce-randomly4
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly4 (cdr possible-holes)
                           (try-to-pierce4 hole cave))))))

(define make-maze4
  (lambda (n m)
    (if (not (and (odd?2 n) (odd?3 m)))
        (print 'error)
        (let ((cave
               (make-matrix2 n m (lambda (i j)
                                  (if (and (even?3 i) (even?3 j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat3
                (for3 0 n (lambda (i)
                           (concat3
                            (for2 0 m (lambda (j)
                                       (if (equal? (even?3 i) (even?3 j))
                                           (list)
                                           (list (cons i j)))))))))))
          (pierce-randomly3 (shuffle3 possible-holes) cave)))))


(begin (pretty-print (make-maze4 15 15))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4))
       (pretty-print (make-maze4 4 4)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(define odd?5
  (lambda (n)
    (= 1 (modulo n 2))))

(define even?5
  (lambda (n)
    (= 0 (modulo n 2))))
    
(define map5
  (lambda (f lst)
    (if (null? lst)
      lst
      (cons (f (car lst)) (map5 f (cdr lst))))))

(define foldr5
  (lambda (f base lst)
    (letrec ((foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst)))))))
    (foldr-aux lst))))

(define foldl5
  (lambda (f base lst)
    (letrec ((foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst))))))
    (foldl-aux base lst))))

(define for5
  (lambda (lo hi f)
    (letrec ((for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            (list)))))
    (for-aux lo))))

(define concat5
  (lambda (lists)
    (foldr (lambda (a b) (append a b)) (list) lists)))

(define list-read5
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read5 (cdr lst) (- i 1)))))

(define list-write5
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write5 (cdr lst) (- i 1) val)))))

(define list-remove-pos5
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos5 (cdr lst) (- i 1))))))

(define member-of5
  (lambda (e lst)
    (if (null? lst)
      #f
      (if (equal? e (car lst))
        #t
        (member-of5 e (cdr lst))))))

(define duplicates?5
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member-of5 (car lst) (cdr lst))
            (duplicates?5 (cdr lst))))))

(define make-matrix5
  (lambda (n m init)
    (for 0 n (lambda (i) (for5 0 m (lambda (j) (init i j)))))))

(define matrix-read5
  (lambda (mat i j)
    (list-read5 (list-read5 mat i) j)))

(define matrix-write5
  (lambda (mat i j val)
    (list-write5 mat i (list-write5 (list-read5 mat i) j val))))

(define matrix-size5
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map5
  (lambda (f mat)
    (map5 (lambda (lst) (map5 f lst)) mat)))

(define next-random5
  (lambda (current-random)
    (modulo (+ (* current-random 3581) 12751) 131072)))

(define neighboring-cavities5
  (lambda (pos cave)
    (let ((size (matrix-size5 cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read5 cave (- i 1) j))
                      (list (cons (- i 1) j))
                      (list))
                  (if (and (< i (- n 1)) (matrix-read5 cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      (list))
                  (if (and (> j 0) (matrix-read5 cave i (- j 1)))
                      (list (cons i (- j 1)))
                      (list))
                  (if (and (< j (- m 1)) (matrix-read5 cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      (list))))))))

(define shuffle-aux5
  (lambda (lst current-random)
    (if (null? lst)
        (list)
        (let ((new-random (next-random5 current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux5 (list-remove-pos lst i)
                                new-random)))))))

(define shuffle5
  (lambda (lst)
    (shuffle-aux5 lst initial-random)))

(define cave-to-maze5
  (lambda (cave)
    (matrix-map5 (lambda (x) (if x '_ '*)) cave)))

(define change-cavity-aux5
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read5 cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux5 c nc new-cavity-id old-cavity-id))
                   (matrix-write5 cave i j new-cavity-id)
                   (neighboring-cavities5 pos cave))
            cave)))))

(define change-cavity5
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux5 cave pos new-cavity-id (matrix-read5 cave i j)))))

(define pierce5
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write5 cave i j pos))))

(define try-to-pierce5
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities5 pos cave)))
        (if (duplicates?5
             (map5 (lambda (nc) (matrix-read5 cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl5 (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define pierce-randomly5
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly5 (cdr possible-holes)
                           (try-to-pierce5 hole cave))))))

(define make-maze5
  (lambda (n m)
    (if (not (and (odd?5 n) (odd?5 m)))
        (print 'error)
        (let ((cave
               (make-matrix5 n m (lambda (i j)
                                  (if (and (even?5 i) (even?5 j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat5
                (for5 0 n (lambda (i)
                           (concat5
                            (for5 0 m (lambda (j)
                                       (if (equal? (even?5 i) (even?5 j))
                                           (list)
                                           (list (cons i j)))))))))))
          (pierce-randomly5 (shuffle5 possible-holes) cave)))))


(begin (pretty-print (make-maze5 15 15))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4))
       (pretty-print (make-maze5 4 4)))













































