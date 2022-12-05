#lang racket

(define-syntax when
    (syntax-rules ()
        ((when test) test)
        ((when test expr ...)
            (if test (begin expr ...) #f)
        )
    )
)

(when #t)
(when #f)
(when #t (print 'a) (print 'b) (println 'c))
(when #f (print 'a) (print 'b) (println 'c))

(define (filter1 f lst)
  (reverse (foldl (lambda (x y) (
    if (f x) (cons x y) y 
  )
  ) null lst)))

(define (filter2 f lst)
  (foldr (lambda (x y) (if (f x) (append (list x) y) y)) null lst))

(filter1 even? '(1 2 3 4 5))
(filter2 even? '(1 2 3 4 5))

; ЗАДАНИЕ 2.3


; (λa. (λc. ((λb. (c a)) ((λb. b b) (λb. b b a))))) y z 
; (λa. (λc. ((λb. (c a)) ((λd. d d) (λe. e e a))))) y z 
; (λc. ((λb. (c y)) ((λd. d d) (λe. e e y)))) z 
; ((λb. (z y)) ((λd. d d) (λe. e e y)))
; (z y) -- нормальная форма

; 1, 3, 9, .. -> 1, else -> 0


(define (nthbit n) (
    let loop ((x 1)) (
        if (= x n) 1 (
            if (> x n) 0 (
                loop (* x 3)
            )
        )
    )
))

(nthbit 1)
(nthbit 2)
(nthbit 3)
(nthbit 4)
(nthbit 5)
(nthbit 6)
(nthbit 7)
(nthbit 8)
(nthbit 9)
(nthbit 10)
