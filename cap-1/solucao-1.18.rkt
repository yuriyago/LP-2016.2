#lang racket

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (mult a b)
  (mult-i a b 0))

(define (mult-i a b n)
  (cond ((= b 0) n)
        ((even? b)
         (mult-i (double a) (halve b) n))
        (else
         (mult-i a (- b 1) (+ n a)))))