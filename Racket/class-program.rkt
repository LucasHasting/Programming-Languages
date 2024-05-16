#lang racket
(define (bad a b c)
  (define m (max a b c))
  (define d (- (+ a b c) m))
  (> m d))

(define (heron a b c)
  (define s (/ (+ a b c) 2))
  (if (bad a b c)
      "bad"
  (sqrt (* s (- s a) (- s b) (- s c)))
  )
  )

;(heron 7 4 9)
;(heron 3 4 5)
;(heron 7 2 10)

;CAR return first element of a list and removes it from the list
;CDR returns the rest of the list
;CONS takes 2 lists A and B, returns ((A) B)
;' is used to denote data
;LIST? is a list?
;NULL? is null?
;#T - true
;#F - falses

(define (ctr lst)
  (cond
    ((null? lst) 0)
    ((+ 1 (ctr (cdr lst))))
    )
  )

(ctr '(a b c))

(define (append list1 list2)
  (cond
    ((null? list1) list2)
    (else (cons (car list1) (append (cdr list1) list2)))
  )
)

;(append '(a b c) '(x y z))

(define (member atm a_list)
  (cond
    ((null? a_list) #F)
    ((eq? atm (car a_list)) #T)
    (else (member atm (cdr a_list)))
    )
  )

;(member 'c '(a b cx d))