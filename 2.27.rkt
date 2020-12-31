#lang sicp

#| Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a
deep-reverse procedure that takes a list as argument and returns as its value
the list with its elements reversed and with all sublists deep-reversed as well.
For example,

    (define x
      (list (list 1 2) (list 3 4)))

    x
    ((1 2) (3 4))

    (reverse x)
    ((3 4) (1 2))

    (deep-reverse x)
    ((4 3) (2 1))

|#


(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((pair? (car lst))
         (append (deep-reverse (cdr lst))
                 (list (deep-reverse (car lst)))))
        (else (append (deep-reverse (cdr lst))
                      (list (car lst))))))

(define (reverse lst)
  (if (null? (cdr lst))
      lst
      (append (reverse (cdr lst))
              (list (car lst)))))

(define x (list (list 1 2) (list 3 4)))

(reverse x)

(deep-reverse x)


;; This is a nice one from the community scheme wiki
(define (deep-reverse2 t)
   (if (pair? t)
       (reverse (map deep-reverse2 t))
       t))

(deep-reverse2 x)

;; This is a better implementation of my approach
;; Don't need to check (pair? (car lst)), just check pair directly
(define (deep-reverse3 li)
   (cond ((null? li) '())
         ((not (pair? li)) li)
         (else (append (deep-reverse3 (cdr li))
                       (list (deep-reverse3 (car li)))))))
  
(deep-reverse3 x)
