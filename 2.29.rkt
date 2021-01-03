#lang sicp

#| Write the corresponding selectors left-branch and right-branch, which return
the branches of a mobile, and branch-length and branch-structure, which return
total-weight that returns the total weight of a mobile. A mobile is said to be
the components of a branch. Using your selectors, define a procedure balanced if
the torque applied by its top-left branch is equal to that applied by its
top-right branch (that is, if the length of the left rod multiplied by the)
weight hanging from that rod is equal to the corresponding product for the right
side and if each of the submobiles hanging off its branches is balanced. Design
a predicate that tests whether a binary mobile is balanced. Suppose we change
the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert to the new
representation? |#

(define (make-mobile left right) ; left/right are branch
  (list left right))


(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length mobile)
  (car mobile))

(define (branch-structure mobile)
  (car (cdr mobile)))

 (define (total-weight mobile)
   (cond ((null? mobile) 0)
         ((not (pair? mobile)) mobile)
         (else (+ (total-weight (branch-structure (left-branch mobile)))
                  (total-weight (branch-structure (right-branch mobile)))))))

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (if (pair? (branch-structure left)) (balanced? left) #t)
         (if (pair? (branch-structure right)) (balanced? right) #t))))


(define w (make-mobile (make-branch 2 4) (make-branch 1 8)))

(display w)
(newline)
(newline)
(total-weight w)
(newline)
(torque (left-branch w))
(newline)
(torque (right-branch w))
(newline)
(balanced? w)
