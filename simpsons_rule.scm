#lang racket

(define pi 3.1415929)

(define (square x) (* x x))

(define (sum fn a b)
  (if (> a b)
      0
      (+ (fn a) (sum fn (+ a 1) b))
  )
)

(define (coeff i n)
  (cond ((= 0 i) 1)
        ((= n i) 1)
        ((even? i) 2)
        ((odd? i) 4)
  )
)

(define (simp fn a b n)
  (simp-iter fn a b n 0)
)

(define (simp-iter fn a b n k)
  (define h (/ (- b a) n))
  (if (> k n)
      0
      (+ (* (/ h 3) (fn (+ a (* k h))) (coeff k n))
         (simp-iter fn a b n (+ k 1))
      )
  )
)



