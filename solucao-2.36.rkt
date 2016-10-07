#lang racket

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map (lambda (x) (car x))
                                     seqs))
            (accumulate-n op init (map (lambda (x) (cdr x))
                                     seqs)))))

(define (accumulate op initial list)
  (if (null? list)
      initial
      (op (car list)
          (accumulate op initial (cdr list)))))
                    