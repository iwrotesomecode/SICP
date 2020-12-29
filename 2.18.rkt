#lang sicp

#| Exercise 2.18: Define a procedure reverse that takes a list as argument and
returns a list of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)|#

; recursive reverse
(define (reverse list-r)
  (if (null? (cdr list-r))
      list-r
      (append (reverse (cdr list-r))
              (list (car list-r)))))

; iterative reverse
(define (reverse-iter list0)
  (define (iter intermediate final)
    (if (null? intermediate)
        final
        (iter (cdr intermediate)
              (cons (car intermediate)
                    final))))
  (iter list0 (list)))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(reverse (list 1 4 9 16 25))

(reverse-iter (list 1 3 5 7 9))
