#lang racket

;Name: Lucas Hasting
;Date: 4/3/2024

;References: Official Racket Documentation and class notes
;Offical Racket Documentation Used:
;https://docs.racket-lang.org/reference/reader.html
;https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._length%29%29
;https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldl%29%29
;https://docs.racket-lang.org/reference/Equality.html
;https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._append%29%29

;function to get the average of a list of numbers
(define (average lst)
  (/ (foldl + 0 lst) (length lst))
)

;function test
(average '(88 55 44 66 77 99))

;function to count the numbers greater than the average in a list of numbers
(define (greater_than_average lst)
  
  ;helper function to recurse through the list
  (define (helper lst avg)
      (cond
        ((null? lst) 0) ;if list is null, return 0
        ((> (car lst) avg) (+ 1 (helper (cdr lst) avg))) ;if first index is > average, add 1 and recurse
        (else (helper (cdr lst) avg)) ;otherwise recurse
      )
   )
  
  (helper lst (average lst)) ;call the helper function
)

;function test
(greater_than_average '(88 55 44 66 77 99))

;count the roots in a quadratic equation
(define (number_of_quadratic a b c)
  (cond
    ((equal? (- (* b b) (* 4 a c)) 0) 1) ;if sqrt is (0) return 1
    ((< (- (* b b) (* 4 a c)) 0) 0) ;if sqrt is (-) return 0
    (else 2) ;otherwise return 2
  )
 )

;function tests
(number_of_quadratic 1 -1 -6)
(number_of_quadratic 2 3 5)
(number_of_quadratic 1 6 9)

;find the roots in a quadratic equation
(define (quadratic a b c)
  (cond
    ((equal? (- (* b b) (* 4 a c)) 0) (/ (- 0 b) (* 2 a))) ;if sqrt is (0)
    ((< (- (* b b) (* 4 a c)) 0) "No real roots") ;if sqrt is (-)
    (else (list (/ (+ (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) (/ (- (- 0 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))) ;otherwise
  )
 )

;function tests
(quadratic 1 -1 -6)
(quadratic 2 3 5)
(quadratic 1 6 9)