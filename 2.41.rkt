#lang sicp

#| Exercise 2.41: Write a procedure to find all ordered triples of distinct
positive integers i , j , and k less than or equal to a given integer n that sum
to a given integer s . |#

; Helper Functions

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ 1 a) b))))

; Problem

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                       (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (flatsum triple)
  (+ (car triple) (cadr triple) (caddr triple)))

(define (triple-sum x sum)
  (filter (lambda (triple) (= sum (flatsum triple))) (unique-triples x)))

; Test
(triple-sum 4 8) ; '((4 3 1))
(triple-sum 6 12) ; '((5 4 3) (6 4 2) (6 5 1))
