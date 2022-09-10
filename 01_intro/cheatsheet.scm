#lang scheme

(define size 2)
(define double_size (+ size size))

size
(println size)

; quoting code
(quote (+ 1 size))
'(+ 1 size)

; defining a function
(define (square x) (* x x))
(square 12)
(procedure? square)

; cond
; Вычисляем предикаты по порядку, начиная с 1-го, до тех пор пока не получим не #f
; В заключительной ветви полезно вместо предиката писать else
(cond (#f 11) (0 21) (1 31) (1 41))

; if
(define (abs x) (if (< x 0) (- x) x))
(abs -5)
(abs 5)
; if круче cond, потому что вычисляет выражение только если они нужны

; case
(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite)
)

; begin
; (begin (println "Input N:") (read))

; * of 2 max numbers 
(define (two-of-three x y z)
  (cond ((or (<= x y z) (<= x z y)) (* y z))
        ((or (<= y x z) (y z x)) (* x z))
        (else (* x y))
  )
)

(two-of-three 1 2 3)

(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

(factorial 5)

; lists
(define lst '(1 2 3 4 5 6))
; взять голову
(car lst)
; взять всё остальное
(cdr lst)
; посчитать длину за линейное время
(length lst)
(reverse lst)
; найти вхождение
(member 2 lst)

; apply
; рассматривает второй аргумент как список аргументов для функции
(apply + lst)