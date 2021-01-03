#lang sicp

#| Exercise 2.32: We can represent a set as a list of distinct elements, and we
can represent the set of all subsets of the set as a list of lists. For example,
if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1))
3 (1 2) (1 2 3). Complete the following definition of a procedure that
generates the set of subsets of a set and give a clear explanation of why it
works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map ⟨??⟩ rest)))))|#

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


(subsets (list 1 2 3))

; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


#|

(subsets '(1 2))

(append (subsets '(2))
        (map cons 1 (subsets '(2))))


(append (append (subsets '())
                (map cons 2 (subsets '())))
        (map cons 1 (append (subsets '())
                        (map cons 2 (subsets '())))))

(append (append '()
                '(2))
        (map cons 1 (append '()
                        '(2))))

(append '('(), '(2))
        (map cons 1 '('(),'(2))))

(append '('(), '(2))
        ('(1) '(1 2)))

'('() '(2) '(1) '(1 2))


|#
