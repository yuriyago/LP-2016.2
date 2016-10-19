#lang racket
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

(define (accumulate op initial list)
  (if (null? list)
      initial
      (op (car list)
          (accumulate op initial (cdr list)))))

(require racket/trace)
(trace accumulate)