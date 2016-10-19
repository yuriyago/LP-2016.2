#lang racket
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (not (pair? x))
                                   1
                                   (count-leaves x)))
                   t)))

(define (accumulate op initial list)
  (if (null? list)
      initial
      (op (car list)
          (accumulate op initial (cdr list)))))