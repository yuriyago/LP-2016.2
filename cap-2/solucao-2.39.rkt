#lang racket
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) empty sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y) x)) empty sequence))

(define (fold-right op initial list)
  (if (null? list)
      initial
      (op (car list)
          (fold-right op initial (cdr list)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))