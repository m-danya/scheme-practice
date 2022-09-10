#lang racket

(define (get_length x y z) (sqrt (+ (* x x) (* y y) (* z z))))

(define (normalize_vector x y z)
  (
   let
    ((length (get_length x y z)))
    (list (/ x length) (/ y length) (/ z length))))

(define (codirectional? x0 y0 z0 x1 y1 z1)
  (
   equal?
   (normalize_vector x0 y0 z0)
   (normalize_vector x1 y1 z1)))

; (codirectional? 1 1 0 2 2 0)
; (codirectional? 2 3 4 1 2 3)
; (codirectional? 1 1 1 -2 -2 -2)