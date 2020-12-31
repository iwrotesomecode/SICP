#lang sicp

#| Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3
4))). Give the result printed by the interpreter, the corresponding
box-and-pointer structure, and the interpretation of this as a tree (as in
Figure 2.6). |#

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 4)))) ;4
(length (list 1 (list 2 (list 3 4)))) ;2


#|
(1 (2 (3 4)))
    /   \
   1   (2 (3 4))
        /   \
       2   (3 4)
            / \
           3   4
|#
