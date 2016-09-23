#lang racket

(define (adjoin-set x set)
  (cond
    ((empty? set) (list x))
    ((< x (car set)) (cons x set))
    ((= x (car set)) set)
    ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(define (adjoin-set-old x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (cond ((or (empty? set) (< x (car set)))
;;          (cons x set))
;; 	((= x (car set)) set) 
;; 	((> x (car set))(cons (car set) (adjoin-set x (cdr set))))))

