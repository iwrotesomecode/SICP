#lang sicp

#| Exercise 2.37: Suppose we represent vectors v = ( v i ) as sequences of
numbers, and matrices m = ( m i j ) as sequences of vectors (the rows of the)
matrix. For example, the matrix
( 1 2 3 4 4 5 6 6 6 7 8 9 ) is represented as the sequence ((1 2 3 4) (4 5 6))
6 (6 7 8 9). With this representation, we can use sequence operations to
concisely express the basic matrix and vector operations. These
operations (which are described in any book on matrix algebra) are the
following:
(dot-product v w) returns the sum Σ i v i w i ; (matrix-*-vector m v) returns the vector t , where t i = Σ j m i j v j ; (matrix-*-matrix m n) returns the matrix p , where p i j = Σ k m i k n k j ; (transpose m) returns the matrix n , where n i j = m j i .
We can define the dot product as83

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2.36.)

(define (matrix-*-vector m v)
  (map ⟨??⟩ m))

(define (transpose mat)
  (accumulate-n ⟨??⟩ ⟨??⟩ mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map ⟨??⟩ m)))|#

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))


; Helper functions

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-orig proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map-orig proc (cdr items)))))

; Test
(define v '(1 2 3))
(define u '(4 5 6))
(dot-product v u) ; '(32)
; The following doesn't work.
;(dot-product (transpose u) v) ; '((4 8 12) (5 10 15) (6 12 18))


(define m '((1 2 3) (4 5 6)))
(define n '((7 8) (9 10) (11 12)))

(matrix-*-vector m u) ; '(32 77)
(transpose m)         ; '((1 4) (2 5) (3 6))
(matrix-*-matrix m n) ; '((58 64) (139 154))
