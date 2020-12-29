#lang sicp

#| Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the
system on a variety of arithmetic expressions. Make some intervals A and B , and
use them in computing the expressions A / A and A / B . You will get the most
insight by using intervals whose width is a small percentage of the center
value. Examine the results of the computation in center-percent form (see)
Exercise 2.12. |#

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))


;; helper functions
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by zero (interval spans zero)." y)
      (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

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

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

;; Testing

(define b1 (make-center-percent 5 5))
(define b2 (make-center-percent 10 2))

(display "Set 1")
(newline)
(display (par1 b1 b1))
(newline)
(display (par2 b1 b1))
(newline)
(display "Set 2")
(newline)
(display (par1 b1 b2))
(newline)
(display (par2 b1 b2))


#| Exercise 2.15: Eva Lu Ator, another user, has also noticed the different
intervals computed by different but algebraically equivalent expressions. She
says that a formula to compute with intervals using Alyssa’s system will produce
tighter error bounds if it can be written in such a form that no variable that
represents an uncertain number is repeated. Thus, she says, par2 is a “better”
program for parallel resistances than par1. Is she right? Why? |#


;; Interval arithmetic doesn't necessarily have same alebraic properties, as
;; we've seen with cases for multiplication and sign. Here we are trying to say
;; they are equivalent, when they are not.

#| Exercise 2.16: Explain, in general, why equivalent algebraic expressions may
lead to different answers. Can you devise an interval-arithmetic package that
does not have this shortcoming, or is this task impossible? (Warning: This)
problem is very difficult. |#

;; One example: https://en.wikipedia.org/wiki/Allen%27s_interval_algebra new
;; relations need to be described by intervals that are outside standard algebra
