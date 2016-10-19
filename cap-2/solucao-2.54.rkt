#lang racket
(define (equal? a b)
  (if (= (length a) (length b))
      (if (and (null? a) (null? b))
          true
          (if (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))
              false))
      false))