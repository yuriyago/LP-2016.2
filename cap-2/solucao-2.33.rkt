#lang racket
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) empty sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (accumulate op initial list)
  (if (null? list)
      initial
      (op (car list)
          (accumulate op initial (cdr list)))))
                    