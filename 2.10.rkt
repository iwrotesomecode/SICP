#lang sicp

#| Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over
Alyssa’s shoulder and comments that it is not clear what it means to divide by
an interval that spans zero. Modify Alyssa’s code to check for this condition
and to signal an error if it occurs. |#


(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by zero (interval spans zero)." y)
      (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y))))))

;; helper functions
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (make-interval a b) (cons a b))


;; test
(define x (make-interval 3 5))
(define y (make-interval -2 1))
(div-interval x y)
