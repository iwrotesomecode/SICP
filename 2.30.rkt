#lang sicp

#|Exercise 2.30: Define a procedure square-tree analogous to the square-list
procedure of Exercise 2.21. That is, square-tree should behave as follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(1 (4 (9 16) 25) (36 49))

Define square-tree both directly (i.e., without using any higher-order)
procedures and also by using map and recursion. |#


; direct definition
(define (square-tree-d tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree tree))
        (else
         (cons (square-tree-d (car tree))
               (square-tree-d (cdr tree))))))
; map definition
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))

; examples
(define (scale-tree-d tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree-d (car tree)
                           factor)
               (scale-tree-d (cdr tree)
                           factor)))))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
