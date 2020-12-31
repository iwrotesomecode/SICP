#lang sicp

#| Exercise 2.20: The procedures +, *, and list take arbitrary numbers of
arguments. One way to define such procedures is to use define with dotted-tail
notation. In a procedure definition, a parameter list that has a dot before the
last parameter name indicates that, when the procedure is called, the initial
parameters (if any) will have as values the initial arguments, as usual, but the
final parameter’s value will be a list of any remaining arguments. Use this
notation to write a procedure same-parity that takes one or more integers and
returns a list of all the arguments that have the same even-odd parity as the
first argument. |#


(define (same-parity x . z)
  (define (iter original final)
    (if (null? original)
        final
        (if (= (remainder x 2) (remainder (car original) 2))
            ; if parity on x matches first item in remaining list
            ; cons this to final list
            (iter (cdr original) (cons (car original) final))
            ; otherwise iterate without cons'ing final list
            (iter (cdr original) final))))
  (reverse (iter z (list x))))

;; helper function
(define (reverse list-r)
  (if (null? (cdr list-r))
      list-r
      (append (reverse (cdr list-r))
              (list (car list-r)))))

;; test
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
