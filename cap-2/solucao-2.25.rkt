#lang racket

#|Exercise 2.25. Give combinations of cars and cdrs that will pick 7 from each of the following lists:
(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))|#

; extraindo o 7 no primeiro exemplo
(define first-ex (list 1 3 (list 5 7) 9))
first-ex
(car (cdr (car (cdr (cdr first-ex)))))

; extraindo o 7 no segundo exemplo
(define second-ex (list(list 7)))
second-ex
(car (car second-ex))

; extraindo o 7 no terceiro exemplo
(define third-ex (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
third-ex
(car (cdr (car (cdr ( car (cdr (car ( cdr ( car (cdr (car (cdr third-ex))))))))))))
