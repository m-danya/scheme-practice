#lang racket

; copypaste from lecture (only for output comparison)
; (define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2))))))    (define (accumulate combiner null-val term a next b) (let loop ((a a) (result null-val)) (if (> a b) result (loop (next a) (combiner (term a) result)))) )(define (enumerate-interval a b) (accumulate (lambda (x y) (append y x)) '() list a add1 b))   (define (list-fib-squares-from-lecture n) (map (lambda (x) (let ((temp (fib x))) (* temp temp))) (enumerate-interval 1 n)))

; linear-iterative
(define (list-fib-squares-a n)
  (let loop ((k 1) (result '()) (prev-1 0) (prev-2 1))
    (let* (
           (current (+ prev-1 prev-2))
           )
      (if (> k n)
         (reverse (map (lambda (x) (* x x)) result))
         (
          if (= k 1)
            (loop (+ k 1) (cons current result) 0 1)
            (loop (+ k 1) (cons current result) prev-2 current)
         )
         )
      )
    )
  )

; linear-iterative using mapping+folding only
(define (list-fib-squares-b n) (
    reverse (
      map
        (lambda (x) (* x x))
        (foldl 
          (
              lambda (x result) (
                  if (or (equal? x 0) (equal? x 1))
                  (cons 1 result)
                  (
                      let (
                          (prev-1 (car (cdr result)))
                          (prev-2 (car result))
                      ) (
                          cons (+ prev-1 prev-2) result
                      )
                  )
              )
          )
          '() (build-list n values)
        )
    )
))

(define (process lst) (
    let* 
    (
      (first-elem-mult (foldl * 1 (car lst)))
      (filter-function (lambda (some-list) (
        > (foldl + 0 some-list) first-elem-mult
      )))
    )
    (filter filter-function lst)
  )
)

; (list-fib-squares-from-lecture 10)
; (list-fib-squares-a 10)
; (list-fib-squares-b 10)

; (process '((5) (1 2) () (3 4) (2 3) (2 3 4)))