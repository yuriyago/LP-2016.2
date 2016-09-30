#lang racket
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leafs)
    (if (empty? (cdr leafs))
        (car leafs)
        (successive-merge (merge-lowers leafs))))

(define (merge-lowers leafs)
  (define (aux lower leafs)
    (let ((second-lower (lower-weight leafs)))
      (append
       (remove second-lower leafs)
       (list (make-code-tree second-lower lower)))))
  (aux (lower-weight leafs) (remove (lower-weight leafs) leafs)))

(define (lower-weight leafs)
  (define (aux list lower)
    (if (empty? list)
        lower
        (if (>= (weight (car list))
                (weight lower))
            (aux (cdr list) lower)
            (aux (cdr list) (car list)))))
  (aux (cdr leafs) (car leafs)))






(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

