#lang sicp

#|Exercise 2.11: In passing, Ben also cryptically comments: “By
testing the signs of the endpoints of the intervals, it is possible to break
mul-interval into nine cases, only one of which requires more than two
multiplications.” Rewrite this procedure using Ben’s suggestion.|#

;There are the 9 signed cases in order xl(x_lower), yl, xu(x_upper),yu
;xl,yl,xu,yu
;1 ++++ 
;2 ++-+ 
;3 -+++
;4 -+-+ This case needs to test min
;5 --++
;6 ++--
;7 -+--
;8 ---+
;9 ----
(define (mul-interval x y)
  (define (neg? x) (< x 0))
  (define (pos? x) (>= x 0))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (pos? xl) (pos? yl)) ;1 ++++ 
           (make-interval (* xl yl) (* xu yu)))
          ((and (pos? xl) (neg? yl) (pos? yu)) ;2 ++-+
           (make-interval (* xu yl) (* xu yu)))
          ((and (neg? xl) (pos? yl) (pos? xu)) ;3 -+++
           (make-interval (* xl yu) (* xu yu)))
          ((and (neg? xl) (neg? yl) (pos? xu) (pos? yu)) ;4 -+-+
           (let ((p1 (min (* xl yu) (* yl xu)))
                 (p2 (max (* xu yu) (* xl yl))))
             (make-interval p1 p2)))
          ((and (neg? xl) (neg? xu) (pos? yl)) ;5 --++
           (make-interval (* xl yu) (* xu yl)))
          ((and (pos? xl) (neg? yu)) ;6 ++--
           (make-interval (* xl yu) (* xu yl)))
          ((and (neg? xl) (pos? xu) (neg? yl)) ;7 -+--
           (make-interval (* xu yu) (* xl yu)))
          ((and (neg? xu) (neg? yl) (pos? yu)) ;8 ---+
           (make-interval (* xl yu) (* xl yl)))
          ((and (neg? xu) (neg? yu)) ;9 ----
           (make-interval (* xl yl) (* xu yu))))))


;; original implementation
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; Helper functions
;; 
; ensure car always is lower-bound
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))


;; tests from http://community.schemewiki.org/?sicp-ex-2.11
;;
 (define (eql-interval? a b) 
   (and (= (upper-bound a) (upper-bound b)) 
        (= (lower-bound a) (lower-bound b)))) 
  
 ;; Fails if the new mult doesn't return the same answer as the old 
 ;; naive mult. 
 (define (ensure-mult-works aH aL bH bL) 
   (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
    (if (eql-interval? (mul-interval-old a b) 
                      (mul-interval a b)) 
       1 
       (error "new mult returns different value!"  
              a  
              b  
              (mul-interval-old a b) 
              (mul-interval a b)))))
  
  
 ;; The following is overkill, but it found some errors in my 
 ;; work.  The first two #s are the endpoints of one interval, the last 
 ;; two are the other's.  There are 3 possible layouts (both pos, both 
 ;; neg, one pos one neg), with 0's added for edge cases (pos-0, 0-0, 
 ;; 0-neg). 
  
 (ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10)
  
  
  
