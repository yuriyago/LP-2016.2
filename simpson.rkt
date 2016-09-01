#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* k h))))

    (define (term k)
      (* (cond ((= k 0) 1)
               ((= k n) 1)
               ((odd? k) 4)
               (else 2))
         (y k)))
  
    (/ (* h (sum term 0 (lambda (n) (+ n 1)) n)) 3)))
