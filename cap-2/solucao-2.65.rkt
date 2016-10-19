#lang racket

(define (union-set tree1 tree2)
  (define (union-set-l list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          ((let ((x1 (car list1)) (x2 (car list2)))
             (cond ((= x1 x2)
                    (cons x1 (union-set-l (cdr list1)
                                          (cdr list2))))
                   ((< x1 x2)
                    (cons x1 (union-set-l (cdr list1)
                                          list2)))
                   ((< x2 x1)
                    (cons x2 (union-set-l list1
                                          (cdr list2)))))))))
  (list->tree
   (union-set-l (tree->list-2 tree1)
                (tree->list-2 tree2))))


(define (intersection-set tree1 tree2)
  (define (intersection-set-l list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (let ((x1 (car list1)) (x2 (car list2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-set-l 
                           (cdr list1)
                           (cdr list2))))
                ((< x1 x2) (intersection-set-l
                            (cdr list1) 
                            list2))
                ((< x2 x1) (intersection-set-l 
                            list1 
                            (cdr list2)))))))
  (list->tree (intersection-set-l (tree->list-2 tree1)
                                  (tree->list-2 tree2))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))
		      