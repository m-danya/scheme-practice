#lang racket

; definitions from the lecture
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

; IMPLEMENTATION WITHOUT CC

(define (max-tree-level-without-cc tree) (
    if (empty-tree? tree) 0 (
        + 1 (
            max (max-tree-level-without-cc (tree-left tree)) (max-tree-level-without-cc (tree-right tree))
        )
    )
))

(define (print-level-x-without-cc tree x) (
    let loop ((t tree) (level 0)) (
        if (or (empty-tree? t) (> level x)) '() (
            if (= level x)
                (begin 
                    (print (tree-data t))
                    (write-char #\space)
                )
                (
                    begin 
                       (loop (tree-right t) (+ level 1)) 
                       (loop (tree-left t) (+ level 1)) 
                    
                )
        )
    )
))

(define (print-tree-by-level-asc-without-cc tree) (
    let ((max-level (max-tree-level-without-cc tree))) (
        let loop ((tree tree)(x 0)) (
            if (> x max-level) (void) (
                begin 
                ; (print "printing lvl ")
                ; (println x)
                (print-level-x-without-cc tree x)
                (newline)
                (loop tree (+ x 1))
            )
        )
    )
))

; IMPLEMENTATION WITH CC (continuation-passing style)

(define (max-tree-level-cps tree cc) (
    if (empty-tree? tree) (cc 0) (
        max-tree-level-cps 
                ; tree arg
                (tree-left tree)
                ; cc arg
                (lambda (y) (
                        max-tree-level-cps 
                            ; tree arg
                            (tree-right tree)
                            ; cc arg
                            (lambda (z) (
                                cc (
                                    + 1 (max y z)
                                )
                            )) 
                    )
                )
    )
))


(define (print-level-x-cps tree x) (
    let loop ((t tree) (level 0) (cc (lambda () (void)))) (
        if (or (empty-tree? t) (> level x)) (cc) (
            if (= level x)
                (begin 
                    (print (tree-data t))
                    (write-char #\space)
                    (cc)
                )
                (loop (tree-right t) (+ level 1) (
                    lambda () (
                        loop (tree-left t) (+ level 1) cc
                    )
                ))
        )
    )
))


(define (print-tree-by-level-asc tree) (
    ; no need to use cps here: the recursion is already "iterable"
    let ((max-level (max-tree-level-cps tree (lambda (x) x)))) (
        let loop ((tree tree)(x 0)) (
            if (> x max-level) (void) (
                begin 
                ; (print "printing lvl ")
                ; (println x)
                (print-level-x-cps tree x)
                (newline)
                (loop tree (+ x 1))
            )
        )
    )
))


; (print-tree-by-level-asc #())
; (print-tree-by-level-asc #(1 #() #()))
; (print-tree-by-level-asc #(10 #(21 #() #()) #(22 #() #())))
; (print-tree-by-level-asc #(10 #(21 #(31 #() #()) #()) #(22 #(33 #() #()) #(34 #() #()))))
