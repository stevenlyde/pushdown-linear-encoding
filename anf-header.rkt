#lang racket

(require racket/flonum)

(define (prim op . args)
  (apply op args))


