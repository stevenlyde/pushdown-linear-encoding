#lang racket

(provide primlistop?
         primnumop?
         primboolop?
         primvoidop?)

(define primlistops 
  '(list
    append
    cons
    cdr
    car
    reverse
    memq
    member
    vector-ref
    make-vector))

(define primnumops 
  '(+
    -
    *
    /
    sqrt
    expt
    max
    min
    vector-length
    length
    exact->inexact
    fl+
    fl-
    fl/
    fl*
    flsin
    modulo))

(define primvoidops
  '(void
    print
    pretty-print
    vector-set!))

(define primboolops
  '(<
    >
    <=
    >=
    = equal?
    null?
    not
    and
    or
    fl>
    fl<
    list?
    number?
    boolean?))

(define (primlistop? op)
  (and (member op primlistops) #t))

(define (primnumop? op)
  (and (member op primnumops) #t))

(define (primboolop? op)
  (and (member op primboolops) #t))

(define (primvoidop? op)
  (and (member op primvoidops) #t))
