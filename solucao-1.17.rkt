#lang racket

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fmult a b)
  (cond ((= b 0) 0)
        ((even? b)
         (double (fmult a (/ b 2))))
        (else
         (+ a (fmult a (- b 1))))))