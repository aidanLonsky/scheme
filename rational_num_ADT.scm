#lang racket

(define (abs x)
  (if (> 0 x)
      (- x)
      x))

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((rn (/ n g))
      (rd (/ d g)))
      (cond ((and (> 0 rn) (> 0 rd)) (cons (abs rn) (abs rd)))
            ((or (> 0 rn) (> 0 rd)) (cons (- (abs rn)) (abs rd)))
            (else (cons rn rd))))
  )
)

(define (numer x) (car x)) ; what if you gave this function a different object? i think these are just names to simplify design

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  ;(newline)
  (display (numer x))
  (display "/")
  (display (denom x)))