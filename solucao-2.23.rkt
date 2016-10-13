#lang racket

(define (for-each proc itens) 
   (let ((item (cdr itens))) 
     (proc (car itens)) 
     (if (not (null? item)) 
         (for-each proc item) 
         true))) 