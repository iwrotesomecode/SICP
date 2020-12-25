#lang sicp

#| Exercise 2.5: Show that we can represent pairs of nonnegative integers using
only numbers and arithmetic operations if we represent the pair a and b as the
integer that is the product 2^a 3^b . Give the corresponding definitions of the
procedures cons, car, and cdr. |#


; Construct new pair -- This works, but doesn't quite fit problem
;(define (new-cons a b)
;  (lambda (m) (m (expt 2 a) (expt 3 b)))

(define (new-cons a b)
  (* (expt 2 a) (expt 3 b)))

; Get a
;(define (new-car x)
;  (x (lambda (p q) (logb 2 p)))) ; first method
(define (new-car x)
  (define (iter y)
    (if (divides? 3 y)
      (iter (/ y 3))
      (logb 2 y)))
  (iter x))

;Get b
;(define (new-cdr x)
;  (x (lambda (p q) (logb 3 q)))) ; first method
(define (new-cdr x)
  (define (iter y)
    (if (divides? 2 y)
        (iter (/ y 2))
        (logb 3 y)))
  (iter x))


;; helper functions
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Calculate log with base b
(define (logb b x) (/ (log x) (log b)))

; Euclid's Algorithm 
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (divides? divisor x)
  (= 0 (remainder x divisor)))

;; test
(define x (new-cons 5 8))
(display "x: ")
;(display (x *)) ; uses first attempt def new-cons
(display x)
(newline)
(display "a: ")
(display (new-car x))
(newline)
(display "b: ")
(display (new-cdr x))

