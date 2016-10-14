#lang racket
(define (square-list1 items)
  (if (null? items)
      empty
      (cons (sqr (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map sqr items))