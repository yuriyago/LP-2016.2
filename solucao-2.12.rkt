#lang racket

(define (make-interval x y)
  (if (> y x) (cons x y) (cons y x)))

(define (lower-bound interv) (car interv))

(define (upper-bound interv) (cdr interv))

(define (make-center-percent center percent)
  (let ((tolerance (* center (/ percent 100.0))))
    (make-interval (+ center tolerance)
                   (- center tolerance))))

(define (center interv)
  (/ (+ (upper-bound interv)
        (lower-bound interv)) 2.0))

(define (percent interv)
  (let ((center (/ (+ (upper-bound interv)
                       (lower-bound interv)) 2.0))
        (width (/ (- (upper-bound interv)
                     (lower-bound interv)) 2.0)))
    (* (/ width center) 100.0)))