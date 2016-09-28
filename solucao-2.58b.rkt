#lang racket

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (define (sum-iter x initial)
    (if (eq? x empty)
        #f
        (if (eq? (car x) '+)
            (list initial (cdr x))
            (sum-iter (cdr x) (cons (car x) initial)))))
  (sum-iter x empty))

(define (addend s)
  (if (and (pair? (car s)) (eq? (cdar s) empty))
      (caar s)
      (car s)))

(define (augend s)
  (if (and (pair? (cadr s)) (eq? (cdadr s) empty))
      (caadr s)
      (cadr s)))

(define (product? x)
  (and (pair? (cdr x)) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (not (pair? a1)) (not (pair? a2))) (list a1 '+ a2))
        ((pair? a1)
         (if (pair? a2) (append a1 '(+) a2) (append a1 '(+) (list a2))))
        (else (append (list a1) '(+) a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        ((not (pair? m1) (pair? 2)) (list m1 '* m2))
        ((pair? m1)
         (if (pair? m2) (append m1 '(*) m2) (append m1 '(*) (list m2))))
        (else (append (list m1) '(*) m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend (sum? exp)) var)
                   (deriv (augend (sum? exp)) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
