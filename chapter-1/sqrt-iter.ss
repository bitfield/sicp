#lang scheme
(require (planet schematics/schemeunit:3))

(define (sqrt-iter guess x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess) x)))

(define (mysqrt x)
  (sqrt-iter 1.00 x))

(define-simple-check (check-sqrt x)
  (let ((result (mysqrt x)))
    (check-= (* result result) x 0.0001)))

(check-sqrt 2)
(check-sqrt 0)
(check-sqrt 100)
(check-sqrt 8.6)