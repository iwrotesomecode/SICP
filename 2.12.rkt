#lang sicp

#| Exercise 2.12: Define a constructor make-center-percent that takes a center
and a percentage tolerance and produces the desired interval. You must also
define a selector percent that produces the percentage tolerance for a given
interval. The center selector is the same as the one shown above. |#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))


(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

(define (percent i)
  (let ((upper (upper-bound i))
        (center (center i)))
    (* (/ (- upper center) center) 100)))

;; helper functions
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))



;; test
(display (make-center-percent 10 20))
(display (percent (make-center-percent 10 20)))
