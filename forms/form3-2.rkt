#lang racket

(define (task-03 lst) (
    let (
        (lengths (
            map (lambda (x) (
            sqrt (foldl + 0 (
                map (lambda (t) (* t t)) x
            )))) lst
        ))
    )
    (exact->inexact (/ (foldl + 0 lengths) (length lengths)))
))

; (task-03 (list (list 3 4) (list 2 2 1) (list 6 8) (list 4320 3240)))