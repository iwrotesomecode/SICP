#lang sicp

#| Exercise 2.9: The width of an interval is half of the difference between its
upper and lower bounds. The width is a measure of the uncertainty of the number
specified by the interval. For some arithmetic operations the width of the
result of combining two intervals is a function only of the widths of the
argument intervals, whereas for others the width of the combination is not a
function of the widths of the argument intervals. Show that the width of the
sum (or difference) of two intervals is a function only of the widths of the
intervals being added (or subtracted). Give examples to show that this is not
true for multiplication or division. |#

; input two intervals
(define (get-width a b)
  (+ (/ (- (upper-bound b)
           (lower-bound b)) 2)
     (/ (- (upper-bound a)
           (lower-bound a)) 2)))

; input single interval
(define (get-simple-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define x (make-interval 6.12 7.48))
(define y (make-interval 4.465 4.935))



(define -xy (get-width x y))

(define sub-xy (sub-interval x y))
(define w-xy (get-simple-width sub-xy))

(define add-xy (add-interval x y))
(define a-xy (get-simple-width add-xy))

(define div-xy (div-interval x y))
(define d-xy (get-simple-width div-xy))
 
(define mul-xy (mul-interval x y))
(define m-xy (get-simple-width mul-xy))             

(display (equal? -xy w-xy))
(newline)
(display -xy)
(display "--Difference of individual widths")
(newline)
(display a-xy)
(display "--Width of add-interval")
(newline)
(display w-xy)
(display "--Width of sub-interval")
(newline)
(display d-xy)
(display "--Width of div-interval")
(newline)
(display m-xy)
(display "--Width of mul-interval")

;; helper functions
(define (sub-interval x y)
  (let ((s1 (- (lower-bound x)
               (upper-bound y)))
        (s2 (- (upper-bound x) 
               (lower-bound y))))
    (make-interval s1 s2)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

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


(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (make-interval a b) (cons a b))
