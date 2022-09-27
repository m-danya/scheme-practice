#lang racket

; definitions from the lecture
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (merge-lists-from-list lst1 lst2) (
    ; failed
    let loop ((x lst1) (y lst2) (result '())) (
        if (and (empty? x) (empty? y)) 
            result
            (
                loop
                (if (empty? x) '() (cdr x))
                (if (empty? y) '() (cdr y))
                (append
                    result 
                    (if (empty? x) '() (car x))
                    (if (empty? y) '() (car y))
            )
        )
    )
))

(define (print-tree-by-level-asc tree) (
    if (empty-tree? tree) '(()) (
        append 
            (list (list(tree-data tree)))
            (merge-lists-from-list 
                (print-tree-by-level-asc (tree-right tree))
                (print-tree-by-level-asc (tree-left tree))
            )
    )
))

(merge-lists-from-list '((1 1) (2)) '((3)))
; (print-tree-by-level-asc #())
; (print-tree-by-level-asc #(1 #() #()))
; (print-tree-by-level-asc #(10 #(21 #() #()) #(22 #() #())))
; (print-tree-by-level-asc #(10 #(21 #(31 #() #()) #()) #(22 #(33 #() #()) #(34 #() #()))))
; (merge-lists-from-list '((1 1 1) (2 2)) '((5) (6 6 6)))