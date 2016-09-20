#lang racket
(require racket/trace)
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


(trace adjoin-set)
(trace adjoin-set-old)

;Os traces e o adjoin-set-old foram usados
;para mostrar a diminuição de passos
;na nova implementação.