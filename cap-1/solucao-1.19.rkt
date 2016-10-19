#lang racket

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n 0))

(define (fib-iter a b p q count steps)
  (cond ((= count 0) 
         (values b steps))
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 (* q p)) (square q)) 
                   (/ count 2)
                   (+ 1 steps)))
        (else 
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)
                   (+ 1 steps)))))

(let-values (((resp steps) (fib 2000)))
    (writeln resp)
    (writeln steps))