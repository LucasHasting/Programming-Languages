#lang racket

;racket practice
;(* 5 (- 8 2))
;( / (* 2 3 4 5) 7)
;(+ (- 10 (* 6 3)) 7)
;(- 3 4 5 6)

;problem 1
(define (area b h) (/ (* b h) 2))
(area 4 5)

;problem 2
(define (areaHeron a b c) (sqrt (* (/ (+ a b c) 2) (- (/ (+ a b c) 2) a) (- (/ (+ a b c) 2) b) (- (/ (+ a b c) 2) c))))

(areaHeron 4 5 6)

;problem 3
(define (weekly_pay h r)
  (if (> h 40)
      (+ (* 40 r) (/ (* (- h 40) r 3) 2))
      (* h r)))
(weekly_pay 41 5)

;problem 4
(define (f start stop left right)
  (if (= start stop)
      (+ left right)
      (f (+ start 1) stop right (+ left right))))
(define (fib n) (f 1 n 1 1))
(fib 6)