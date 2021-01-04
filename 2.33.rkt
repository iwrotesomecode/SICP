#lang sicp

#| Exercise 2.33: Fill in the missing expressions to complete the following
definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) ⟨??⟩)
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons ⟨??⟩ ⟨??⟩))

(define (length sequence)
  (accumulate ⟨??⟩ 0 sequence))|#

;; these mostly involved looking at what the accumulate procedure does and
;; thinking about original procedure
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; Helper Functions

(define (map-orig proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map-orig proc (cdr items)))))

(define (append-orig list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append-orig (cdr list1)
                    list2))))

(define (length-orig items)
  (if (null? items)
      0
      (+ 1 (length-orig (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (square x) (* x x))


(map square '(1 2 3 4))

(append '(1 2) '(3 4))

(length '(4 5 (6 (7 (8 9 10)))))

