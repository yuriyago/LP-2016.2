#lang racket
(define (map-tree proc tree)
  (map (lambda (x)
         (if (pair? x)
             (map-tree proc x)
             (proc x)))
       tree))