#lang sicp

#| Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the
difference of two intervals may be computed. Define a corresponding subtraction
procedure, called sub-interval. |#

(define (sub-interval x y)
  (let ((s1 (- (lower-bound x)
               (upper-bound y)))
        (s2 (- (upper-bound x) 
               (lower-bound y))))
    (make-interval s1 s2)))



(define x (make-interval 6.12 7.48))

(define y (make-interval 4.465 4.935))
(newline)
(display (sub-interval x y))

;; At first I thought it was (- lower lower), an example shows this is wrong
(newline)
(display (make-interval (- (lower-bound x)
                           (lower-bound y))
                        (- (upper-bound x) 
                           (upper-bound y))))

;; for completeness, this is just the opposite order of the interval
(newline)
(display (make-interval (- (upper-bound x)
                           (lower-bound y))
                        (- (lower-bound x) 
                           (upper-bound y))))


;; helper functions

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (make-interval a b) (cons a b))
