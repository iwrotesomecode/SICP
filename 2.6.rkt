#lang sicp

#| Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling
enough, consider that, in a language that can manipulate procedures, we can get
by without numbers (at least insofar as nonnegative integers are concerned) by
implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the λ-calculus.

Define one and two directly (not in terms of zero and add-1). (Hint: Use)
substitution to evaluate (add-1 zero). Give a direct definition of the addition
procedure + (not in terms of repeated application of add-1). |#

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Substitute for (add-1 zero)
(add-1 zero)
;; add-1 takes one argument
(lambda (f) (lambda (x) (f ((zero f) x))))
;; zero takes one argument f and returns another procedure
(lambda (f) (lambda (x) (f ((lambda (x) x)) x)))
;; zero's returned procedure also takes one arg, returns x
;; zero plus one should be one, so this is the def of one
(lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

;; Substitute for (add-1 one) to get two
(add-1 one)

(lambda (f) (lambda (x) (f ((one f) x))))
;; one takes a single argument and return (f x)
(lambda (f) (lambda (x) (f ((lambda (x) (f x))) x)))
;; two is then a composition of functions
(lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; We know we must compose the functions, so work backwards
;; (f_m (f_m..(f_m x)..))) with m+n f's
;; (m f) will result in a procedure will all the m f's
;; then its argument x_m must be all the n f's=> (f_n (f_n..(f_n x)..))
;; so x_m = ((n f) x)
(define (add-church m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))


;; another way to look at it
(define (compose f g)
    (lambda (x) (f (g x))))

(define (church-add m n)
  (lambda (f) (compose (m f) (n f)))) 
