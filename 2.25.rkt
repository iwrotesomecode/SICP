#lang sicp

#| Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each
of the following lists:

(1 3 (5 7) 9)
((7))
(1 (2 (3 (4 (5 (6 7))))))
|#

(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr a)))))
(car (car b))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
; (cadr x) is (car (cdr x))
(cadr (cadr (cadr (cadr (cadr (cadr c))))))


#|
note that:
(car '(1 2)) -> 1
(cdr '(1 2)) -> (2)
(cadr '(1 2)) -> 2
(caar '((1 2) 3)) -> 1
(cadar '((1 2) 3)) -> 2


(caar x)        (car (car x))
(cadr x)        (car (cdr x))
(cdar x)        (cdr (car x))
(cddr x)        (cdr (cdr x))
(caaar x)       (car (car (car x)))
(caadr x)       (car (car (cdr x)))
(cadar x)       (car (cdr (car x)))
(caddr x)       (car (cdr (cdr x)))
(cdaar x)       (cdr (car (car x)))
(cdadr x)       (cdr (car (cdr x)))
(cddar x)       (cdr (cdr (car x)))
(cdddr x)       (cdr (cdr (cdr x)))
(caaaar x)      (car (car (car (car x))))
(caaadr x)      (car (car (car (cdr x))))
(caadar x)      (car (car (cdr (car x))))
(caaddr x)      (car (car (cdr (cdr x))))
(cadaar x)      (car (cdr (car (car x))))
(cadadr x)      (car (cdr (car (cdr x))))
(caddar x)      (car (cdr (cdr (car x))))
(cadddr x)      (car (cdr (cdr (cdr x))))
(cdaaar x)      (cdr (car (car (car x))))
(cdaadr x)      (cdr (car (car (cdr x))))
(cdadar x)      (cdr (car (cdr (car x))))
(cdaddr x)      (cdr (car (cdr (cdr x))))
(cddaar x)      (cdr (cdr (car (car x))))
(cddadr x)      (cdr (cdr (car (cdr x))))
(cdddar x)      (cdr (cdr (cdr (car x))))
(cddddr x)      (cdr (cdr (cdr (cdr x))))

|#
