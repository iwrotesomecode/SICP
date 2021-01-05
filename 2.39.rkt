#lang sicp

#| Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18)
in terms of fold-right and fold-left from Exercise 2.38:

(define (reverse sequence)
  (fold-right
   (lambda (x y) ⟨??⟩) nil sequence))

(define (reverse sequence)
  (fold-left
   (lambda (x y) ⟨??⟩) nil sequence))|#

(define (reverse-right sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))


(define (reverse-left sequence)
  (fold-left
   (lambda (x y) (append (list y) x)) nil sequence))


; helper function
(define (reverse list-r)
  (if (null? (cdr list-r))
      list-r
      (append (reverse (cdr list-r))
              (list (car list-r)))))

; Note that this is tail recursive In tail recursion, you perform your
; calculations first, and then you execute the recursive call, passing the
; results of your current step to the next recursive step. This results in the
; last statement being in the form of (return (recursive-function params)).
; Basically, the return value of any given recursive step is the same as the
; return value of the next recursive call. The consequence of this is that once
; you are ready to perform your next recursive step, you don't need the current
; stack frame any more. This allows for some optimization. In fact, with an
; appropriately written compiler, you should never have a stack overflow snicker
; with a tail recursive call. Simply reuse the current stack frame for the next
; recursive step.
;
; It is converted to iterative with O(1) memory complexity.
;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Traditional recursion: perform recursive calls first
; then take return value of recursive call to return result
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


; test
(fold-left cons '() '(1 2 3 4 5))

(fold-right cons '() '(1 2 3 4 5))

(fold-left - 0 '(1 2 3)) ; (- (- (- 0 1) 2) 3)
(fold-right - 0 '(1 2 3)) ; (- 1 (- 2 (- 3 0)))
(reverse-right (list 1 2 3)) ;
(reverse-left (list 1 2 3)) ;
