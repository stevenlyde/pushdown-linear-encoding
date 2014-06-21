


  (define even-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              '()
              (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

  (define odd-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-numbers (cdr (cdr l))))))))

  (define merge-lists1
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists1 (cdr l1) l2))
                  (cons (car l2) (merge-lists1 (cdr l2) l1)))))))

  (define merge-sort1
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists1
                (merge-sort1 (odd-numbers l))
                (merge-sort1 (even-numbers l)))))))

  (define merge-lists2
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists2 (cdr l1) l2))
                  (cons (car l2) (merge-lists2 (cdr l2) l1)))))))

  (define merge-sort2
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists2
                (merge-sort2 (odd-numbers l))
                (merge-sort2 (even-numbers l)))))))

  (define merge-lists3
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists3 (cdr l1) l2))
                  (cons (car l2) (merge-lists3 (cdr l2) l1)))))))

  (define merge-sort3
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists3
                (merge-sort3 (odd-numbers l))
                (merge-sort3 (even-numbers l)))))))





(pretty-print (merge-sort1 '(1 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort2 '(1 125 56 34 23 78 46 68 24 6 35 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort3 '(42 1 121 56 34 23 78 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort1 '(1 0 56 34 23 78 46 68 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort3 '(1 66 56 34 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort1 '(0 1 101 56 34 46 68 24 6 35 33 21 23 34 87 17 98 2 4 46 47 75 43 42 499 9 11 13 15 19)))
(pretty-print (merge-sort2 '(1 181 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 41 40 4 9 11 13 15 191)))
(pretty-print (merge-sort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))

(pretty-print (merge-sort3 '(42 1 121 56 34 23 78 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort1 '(1 0 56 34 23 78 46 68 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort3 '(1 66 56 34 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (merge-sort1 '(0 1 101 56 34 46 68 24 6 35 33 21 23 34 87 17 98 2 4 46 47 75 43 42 499 9 11 13 15 19)))
(pretty-print (merge-sort2 '(1 181 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 41 40 4 9 11 13 15 191)))
(pretty-print (merge-sort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))







(define pivot1 (lambda (l)
  (cond ((null? l) 999)
	((null? (cdr l)) 999)
        ((<= (car l) (car (cdr l))) (pivot1 (cdr l)))
	(#t (car l)))))

(define partition1 (lambda (piv l p1 p2)
  (if (null? l) 
      (list p1 p2)
       (if (< (car l) piv) 
           (partition1 piv (cdr l) (cons (car l) p1) p2)
	   (partition1 piv (cdr l) p1 (cons (car l) p2))))))

(define (quicksort1 l)
 (let ((piv (pivot1 l)))
      (if (equal? piv 999) 
          l
          (let ((parts (partition1 piv l '() '())))
               (append (quicksort1 (car parts)) 
                       (quicksort1 (car (cdr parts))))))))


(define pivot2 (lambda (l)
  (cond ((null? l) 999)
	((null? (cdr l)) 999)
        ((<= (car l) (car (cdr l))) (pivot2 (cdr l)))
	(#t (car l)))))

(define partition2 (lambda (piv l p1 p2)
  (if (null? l) 
      (list p1 p2)
       (if (< (car l) piv) 
           (partition2 piv (cdr l) (cons (car l) p1) p2)
	   (partition2 piv (cdr l) p1 (cons (car l) p2))))))

(define (quicksort2 l)
 (let ((piv (pivot2 l)))
      (if (equal? piv 999) 
          l
          (let ((parts (partition2 piv l '() '())))
               (append (quicksort2 (car parts)) 
                       (quicksort2 (car (cdr parts))))))))



(define pivot3 (lambda (l)
  (cond ((null? l) 999)
	((null? (cdr l)) 999)
        ((<= (car l) (car (cdr l))) (pivot3 (cdr l)))
	(#t (car l)))))

(define partition3 (lambda (piv l p1 p2)
  (if (null? l) 
      (list p1 p2)
       (if (< (car l) piv) 
           (partition3 piv (cdr l) (cons (car l) p1) p2)
	   (partition3 piv (cdr l) p1 (cons (car l) p2))))))

(define (quicksort3 l)
 (let ((piv (pivot3 l)))
      (if (equal? piv 999) 
          l
          (let ((parts (partition3 piv l '() '())))
               (append (quicksort3 (car parts)) 
                       (quicksort3 (car (cdr parts))))))))





(pretty-print (quicksort1 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort2 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort1 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort2 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))

(pretty-print (quicksort1 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort2 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort1 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort2 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))
(pretty-print (quicksort3 '(0 1 34 23 78 46 24 6 35 33 21 23 34 87 98 22 45 46 47 75 43 42 41 40 4 9 11 15 19)))








(define (insert-into1 l n)
        (if (null? l)
            (list n)
            (if (< n (car l))
                (cons n l)
                (cons (car l) (insert-into1 (cdr l) n)))))

(define (insertion-sort-aux1 l l+)
        (if (null? l)
            l+
            (insertion-sort-aux1 (cdr l) (insert-into1 l+ (car l)))))

(define (insertion-sort1 l)
   (if (or (null? l) (null? (cdr l)))
       l
       (insertion-sort-aux1 l '())))


(define (insert-into2 l n)
        (if (null? l)
            (list n)
            (if (< n (car l))
                (cons n l)
                (cons (car l) (insert-into2 (cdr l) n)))))

(define (insertion-sort-aux2 l l+)
        (if (null? l)
            l+
            (insertion-sort-aux2 (cdr l) (insert-into2 l+ (car l)))))

(define (insertion-sort2 l)
   (if (or (null? l) (null? (cdr l)))
       l
       (insertion-sort-aux2 l '())))



(define (insert-into3 l n)
        (if (null? l)
            (list n)
            (if (< n (car l))
                (cons n l)
                (cons (car l) (insert-into3 (cdr l) n)))))

(define (insertion-sort-aux3 l l+)
        (if (null? l)
            l+
            (insertion-sort-aux3 (cdr l) (insert-into3 l+ (car l)))))

(define (insertion-sort3 l)
   (if (or (null? l) (null? (cdr l)))
       l
       (insertion-sort-aux3 l '())))




(pretty-print (insertion-sort1 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort3 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort1 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort3 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))

(pretty-print (insertion-sort1 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort3 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort1 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort2 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))
(pretty-print (insertion-sort3 '(99 1 50 56 34 23 78 46 68 24 6 35 33 21 23 34 87 17 98 22 45 46 47 75 43 42 41 40 4 9 11 13 15 19)))




(define (bubble-sort1 x gt?)
  (letrec
    ((fix (lambda (f i)
       (if (equal? i (f i))
           i
           (fix f (f i)))))
     (sort-step (lambda (l)
        (if (or (null? l) (null? (cdr l)))
            l
            (if (gt? (car l) (car (cdr l)))
                (cons (car (cdr l)) (sort-step (cons (car l) (cdr (cdr l)))))
                (cons (car l) (sort-step (cdr l))))))))
 
  (fix sort-step x)))




(define (bubble-sort2 x gt?)
  (letrec
    ((fix (lambda (f i)
       (if (equal? i (f i))
           i
           (fix f (f i)))))
     (sort-step (lambda (l)
        (if (or (null? l) (null? (cdr l)))
            l
            (if (gt? (car l) (car (cdr l)))
                (cons (car (cdr l)) (sort-step (cons (car l) (cdr (cdr l)))))
                (cons (car l) (sort-step (cdr l))))))))
 
  (fix sort-step x)))




(define (bubble-sort3 x gt?)
  (letrec
    ((fix (lambda (f i)
       (if (equal? i (f i))
           i
           (fix f (f i)))))
     (sort-step (lambda (l)
        (if (or (null? l) (null? (cdr l)))
            l
            (if (gt? (car l) (car (cdr l)))
                (cons (car (cdr l)) (sort-step (cons (car l) (cdr (cdr l)))))
                (cons (car l) (sort-step (cdr l))))))))
 
  (fix sort-step x)))



(pretty-print (bubble-sort1 '(1 125 56 4 23 78 612  68 24 6 35 34 87 17 98 22 45 447 75 43 42 41 4  9 11 13 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort1 '(1 125 5634 23 7846 12 68 24 13 3 8717 98 22 45 447 75 43 42 41 0  9 11 13 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort1 '(1 125 5 34 23 7 46 68 24 635 34 8 17 98 11 22 45 647 75 43 42 4140  9 11 13 1519) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 12 5 34 23 7 46 68 24 635 34 8 17 8 22 45 647 75 43 42 4140  9 11 13 1519) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 15 6 34 23 8 46 68 24  35 347 1 98 22 21 44 47 75 43 42 4 40 4 9 11 13 5 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 2556 34 2378 46 68 246 35 3487 17 98 22 4 6 93 47 75 43 42 1 404 9 98 11 13 5 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1 15 56 34 23 78 46 68 4 6 35 4 87 17 98 245 46 47 75 43 4 41 0 4 9 11 3 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1 25 56 34 3 78 46 68 24 6 3534 87 17 98  45 46 47 75 43 2 4 40 4 9 1113 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1125 56 34 23 78 46 6824 6 35 34 87 17 9 2 45 46 47 75 4342 41 40 4 911 13 15 19) (lambda (a b) (> a b))))

(pretty-print (bubble-sort1 '(1 125 56 4 23 78 612  68 24 6 35 34 87 17 98 22 45 447 75 43 42 41 4  9 11 13 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort1 '(1 125 5634 23 7846 12 68 24 13 3 8717 98 22 45 447 75 43 42 41 0  9 11 13 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort1 '(1 125 5 34 23 7 46 68 24 635 34 8 17 98 11 22 45 647 75 43 42 4140  9 11 13 1519) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 12 5 34 23 7 46 68 24 635 34 8 17 8 22 45 647 75 43 42 4140  9 11 13 1519) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 15 6 34 23 8 46 68 24  35 347 1 98 22 21 44 47 75 43 42 4 40 4 9 11 13 5 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort2 '(1 2556 34 2378 46 68 246 35 3487 17 98 22 4 6 93 47 75 43 42 1 404 9 98 11 13 5 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1 15 56 34 23 78 46 68 4 6 35 4 87 17 98 245 46 47 75 43 4 41 0 4 9 11 3 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1 25 56 34 3 78 46 68 24 6 3534 87 17 98  45 46 47 75 43 2 4 40 4 9 1113 15 19) (lambda (a b) (> a b))))
(pretty-print (bubble-sort3 '(1125 56 34 23 78 46 6824 6 35 34 87 17 9 2 45 46 47 75 4342 41 40 4 911 13 15 19) (lambda (a b) (> a b))))




