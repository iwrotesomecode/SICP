#lang sicp

#| Exercise 2.28: Write a procedure fringe that takes as argument a
tree (represented as a list) and returns a list whose elements are all the
leaves of the tree arranged in left-to-right order. |#

;; first attempt
;(define (fringe lst)
;  (cond ((null? lst) nil)
;        ((not (pair? lst)) (λ (x) cons lst x))
;        (else (fringe (car lst))
;              (fringe (cdr lst))

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) (list lst))
        (else (append (fringe (car lst))
                      (fringe (cdr lst))))))

; This may be more performant, not using 'append'
(define (fringe2 lst)
  (define (iter x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (iter (car x)
                      (iter (cdr x) result)))))

  (iter lst nil))
(define x
  (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))

(fringe (list (list 1 2 (list 3 4)) 5 (list 6) (list 7 8 9)))

(fringe2 (list (list 1 2 (list 3 4)) 5 (list 6) (list 7 8 9)))
