#lang sicp

#| Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

    (define (count-leaves t)
      (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))|#


(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                   t)))

; Helper function
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves-orig x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-orig (car x))
                 (count-leaves-orig (cdr x))))))

; test
(count-leaves '((1 2) (3 (4 5)) ((6 7) 8 9)))
