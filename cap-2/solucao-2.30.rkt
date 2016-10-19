#lang racket
(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (sqr x)))
       tree))