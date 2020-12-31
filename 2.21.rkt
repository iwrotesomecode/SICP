#lang sicp


#| Exercise 2.21: The procedure square-list takes a list of numbers as argument
and returns a list of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)

Here are two different definitions of square-list. Complete both of them by
filling in the missing expressions: |#


(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (square x) (* x x))

; Map procedure over a list
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))



(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
