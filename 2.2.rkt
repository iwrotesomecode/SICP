#lang sicp

#| Exercise 2.2: Consider the problem of representing line segments in a plane.
Each segment is represented as a pair of points: a starting point and an ending
point. Define a constructor make-segment and selectors start-segment and
end-segment that define the representation of segments in terms of points.
Furthermore, a point can be represented as a pair of numbers: the x coordinate
and the y coordinate. Accordingly, specify a constructor make-point and
selectors x-point and y-point that define this representation. Finally, using
your selectors and constructors, define a procedure midpoint-segment that takes
a line segment as argument and returns its midpoint (the point whose coordinates)
are the average of the coordinates of the endpoints.
|#

(define (make-point x y)
  (cons x y))

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (midpoint-segment line-segment)
  (let ((a (start-segment line-segment))
        (b (end-segment line-segment)))
    (make-point (average (x-point a) (x-point b))
              (average (y-point a) (y-point b)))))

(define (start-segment segment) (car segment)) 
(define (end-segment segment) (cdr segment)) 

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (x-point x) (car x))
(define (y-point x) (cdr x))
(define (average x y) (/ (+ x y) 2))

; Test
(define p1 (make-point 1 -1))
(define p2 (make-point 5 8))
(define line (make-segment p1 p2))
(define midpoint (midpoint-segment line))
(print-point p1)
(print-point p2)
(print-point midpoint)


#|Exercise 2.3: Implement a representation for rectangles in a plane. (Hint: You)
may want to make use of Exercise 2.2. In terms of your constructors and
selectors, create procedures that compute the perimeter and the area of a given
rectangle. Now implement a different representation for rectangles. Can you
design your system with suitable abstraction barriers, so that the same
perimeter and area procedures will work using either representation? |#

; First method, input line segments from left and bottom
(define (make-rectangle left bottom)
  (cons left bottom))

; Second method, input 3 points
(define (make-rectangle2 upperleft bottomleft bottomright)
  (let ((left (make-segment upperleft bottomleft))
        (bottom (make-segment bottomleft bottomright)))
    (cons left bottom)))

; input rectangle
(define (area x)
  (let ((height (distance (car x)))
        (length (distance (cdr x))))
    (* height length)))

; input rectangle
(define (perimeter rectangle)
   (let ((height (distance (car rectangle)))
        (length (distance (cdr rectangle))))
    (* 2 (+ height length)))) 


(define (distance segment)
  (let ((a (make-point (x-point (start-segment segment))
                       (y-point (start-segment segment))))
        (b (make-point (x-point (end-segment segment))
                       (y-point (end-segment segment)))))
    (sqrt (+ (square
              (- (x-point a) (x-point b)))
             (square
              (- (y-point a) (y-point b)))))))

(define (square x) (* x x))
; There were several sqrt methods explored in Chapter 1
; Heron of Alexandria's method here
; see also fixed point + average damping
; see also fixed point + Newton's method
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; Test
;; Construct points
(define a (make-point 0 0))
(define b (make-point 0 4))
(define c (make-point 5 0))
;; Construct segments
(define left (make-segment a b))
(define bottom (make-segment a c))
;; Construct rectangle
(define rect (make-rectangle left bottom))
(define rect2 (make-rectangle2 b a c))
(newline)
(display (area rect))
(newline)
(display (area rect2))
(newline)
(display (perimeter rect))
(newline)
(display (perimeter rect2))
