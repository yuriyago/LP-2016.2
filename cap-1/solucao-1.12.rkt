#lang racket

(define (pascal line position)
  (if (or (= position 1)
          (= line position))
      1
      (+ (pascal (- line 1) (- position 1))
         (pascal (- line 1) position))))
