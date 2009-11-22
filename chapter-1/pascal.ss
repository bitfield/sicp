#lang scheme
(require (planet schematics/schemeunit:3))

; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;
; The numbers at the edge of the triangle are all 1, and each number inside the 
; triangle is the sum of the two numbers above it.35 Write a procedure that 
; computes elements of Pascal's triangle by means of a recursive process. 

(define (sum-each-pair s)
  (if (= (length s) 2)
      (list (+ (car s) (cadr s)))
      (cons (+ (car s) (cadr s))
            (sum-each-pair (cdr s)))))

(check-equal? (sum-each-pair '(1 2 3)) '(3 5))
(check-equal? (sum-each-pair '(1 2 3 4 12 8 0)) '(3 5 7 16 20 8))

(define (pascals-triangle-next-row s)
  (sum-each-pair (append '(0) 
                         (append s '(0)))))

(check-equal? (pascals-triangle-next-row '(1)) '(1 1))
(check-equal? (pascals-triangle-next-row '(1 1)) '(1 2 1))
(check-equal? (pascals-triangle-next-row '(1 2 1)) '(1 3 3 1))
(check-equal? (pascals-triangle-next-row '(1 3 3 1)) '(1 4 6 4 1))

(define (pascals-triangle-nth-row n)
  (if (= n 1)
      '(1)
      (pascals-triangle-next-row (pascals-triangle-nth-row (- n 1)))))

(check-equal? (pascals-triangle-nth-row 4) '(1 3 3 1))
(check-equal? (pascals-triangle-nth-row 5) '(1 4 6 4 1))

; The exercise asks for a recursive solution, not an iterative one.
; This is given above, but it's nice to print the triangle out as
; well as just computing its nth row. That's better done iteratively.

(define (print-pascals-triangle-from current-row num-rows)
  (display current-row)
  (newline)
  (if (= num-rows 1)
      #t
      (print-pascals-triangle-from (pascals-triangle-next-row current-row) (- num-rows 1))))

(define (print-pascals-triangle num-rows)
  (print-pascals-triangle-from '(1) num-rows))

(print-pascals-triangle 10)