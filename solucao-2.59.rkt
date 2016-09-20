#lang racket

;Dados
(define (element-of-set? x set)
(cond ((null? set) false)
((equal? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
(if (element-of-set? x set)
set
(cons x set)))

(define (intersection-set set1 set2)
(cond ((or (null? set1) (null? set2)) '())
((element-of-set? (car set1) set2)
(cons (car set1)
(intersection-set (cdr set1) set2)))
(else (intersection-set (cdr set1) set2))))

; exercício-2.59

#|cond:
1)verificar se algum dos sets é vazio --> output: set não vazio
2)verificar se o primeiro elemento (car) do set1 está no set2 ---> output: aplicar o unio-set ao cdr do set1, somente
3)else: union-set do cdr do set1 com o cons do car do set1 e o set2.|#


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)(union-set (cdr set1) set2))
        ( union-set (cdr set1)(cons(car set1) set2))))

(define teste1 (list 1 2 3 4 5 6))
(define teste2 (list 1 2 3 4 5 6 7 8 9 10))
(union-set teste1 teste2)


