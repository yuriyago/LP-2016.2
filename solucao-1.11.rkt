#lang racket
(require racket/trace)

(define (f0 n)
  (if (< n 3)
      n
      (+ (f0 (- n 1)) (* 2 (f0 (- n 2))) (* 3 (f0 (- n 3))))))

(define (f-iter a b c count)
   (if (< count 3)
       a
       (f-iter (+ a (* 2 b) (* 3 c))
               a
               b
               (- count 1))))

(define (f1 n)
   (if (< n 3)
       n
       (f-iter 2 1 0 n)))

(trace f-iter)
