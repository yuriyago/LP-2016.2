#lang racket
(define (subsets s)
  (if (null? s)
      (list empty)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))