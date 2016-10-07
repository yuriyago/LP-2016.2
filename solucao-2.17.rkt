#lang racket
(define (last-pair list)
  (define (last-pair-iter list pair)
    (if (null? list)
        pair
        (last-pair-iter (cdr list) list)))
  (last-pair-iter list empty))