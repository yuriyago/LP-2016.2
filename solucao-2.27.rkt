#lang racket
(define (deep-reverse list)
  
  (define (dr-iter list out)
    (cond ((null? list) out)
          ((pair? list)
           (dr-iter (cdr list)
                    (cons (if (pair? (car list))
                              (dr-iter (car list) empty)
                              (car list))
                          out)))
          (else (dr-iter empty (cons list out)))))
  
  (dr-iter list empty))