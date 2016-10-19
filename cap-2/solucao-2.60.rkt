#lang racket
;Não muda
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;Fica mais simples
(define (adjoin-set x set)
  (cons x set))

;O original depende da ordem: o teste daria dois resultados diferentes

(define (intersection-set set1 set2)
  (define (aux set1 set2 dummy)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
  (if (check-duplicates set1)
      (aux set1 set2 0)
      (aux set2 set1 0)))
  

(define quatros (list 4 4 4 ))
(define pares (list 2 4 6 8))

(intersection-set quatros pares)
(intersection-set pares quatros)

