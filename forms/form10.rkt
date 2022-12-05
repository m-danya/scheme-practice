#lang racket

(define Y
    (lambda (f) (
        (lambda (x) (x x))
        (lambda (g) (f (lambda args (apply (g g) args)))))
    )
)

(define n!!! (lambda (n)
    (
        (Y (lambda (f) (lambda (i result) (
        cond
            ((= i 0) (* result 1)) ; N_0 = 1
            ((= i 1) (* result 1)) ; N_1 = 1
            ((= i 2) (* result 2)) ; N_2 = 2
            (else    (f (- i 3) (* i result)))
        ))))
        n
        1
    )
))

(n!!! 0)
(n!!! 1)
(n!!! 2)
(n!!! 3)
(n!!! 4)
(n!!! 5)
(n!!! 6)
(n!!! 7)
(n!!! 8)
