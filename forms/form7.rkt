#lang racket

; 1. реализовать это через функции нельзя, т.к. во время вызова произойдет связывание
; локального для кадра имени с фактическим значением аргумента, поэтому мы не сможем изменить
; связывание того имени, которого хотим

(define-syntax left-rot!
    (syntax-rules (first-value-is-carried) ; специальный символ!
    ((left-rot! first-value-is-carried first-value m0) (
        begin
            (set! m0 first-value)
    ))
    ((left-rot! first-value-is-carried first-value m0 m1 m2 ...) (
        begin
            (set! m0 m1)
            (left-rot! first-value-is-carried first-value m1 m2 ...)
    ))
    ((left-rot! m0) (void))
    ((left-rot! m0 m1 m2 ...) (
        let ((first-value m0)) (
            begin
            (set! m0 m1)
            (left-rot! first-value-is-carried first-value m1 m2 ...)
        )
    ))
))

(define a 1)
(define b 2)
(define c 3)
(define d 4)
(println a)
(println b)
(println c)
(println d)
(println '->)
(left-rot! a b c d)
(println a)
(println b)
(println c)
(println d)
(println 'done)
; 2 -> 2
(left-rot! a)
(println a)
(println 'done)
; 2 3 -> 3 2
(left-rot! a b)
(println a)
(println b)
(println 'done)