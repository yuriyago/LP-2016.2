#lang racket

 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 
 (define (accumulate-n op init sequence)  
   (if (null? (car sequence)) 
       null 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence))))) 
  
  
 (define matriz (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
  
  
 (define (dot-product v1 v2) 
   (accumulate + 0 (map * v1 v2))) 


 (define (matrix-*-vector m v) 
   (map (lambda (m-linha) (dot-product m-linha v)) 
        m)) 


 (define (transpose m) 
   (accumulate-n cons null m)) 
  

 (define (matrix-*-matrix m n) 
   (let ((n-coluna (transpose n))) 
     (map (lambda (m-linha) (matrix-*-vector n-coluna m-linha)) 
          m))) 
  
 