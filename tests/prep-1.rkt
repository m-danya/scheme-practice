#lang racket

(define (taskI lst) (
    let* (
        (build-structure (lambda (curr-index curr-min indices) (list curr-index curr-min indices)))
        (get-curr-index (lambda (x) (car x)))
        (get-curr-min (lambda (x) (cadr x)))
        (get-indices (lambda (x) (caddr x)))
        (f (lambda (lst) (
            foldl 
                (lambda (x result) (
                    let (
                            (curr-min (get-curr-min result))
                            (curr-index (get-curr-index result))
                            (curr-indices (get-indices result))
                        ) (
                            cond 
                                ((< x curr-min) (
                                    build-structure 
                                        (+ curr-index 1)
                                        x
                                        (list curr-index)
                                ))
                                ((> x curr-min) (
                                    build-structure
                                        (+ curr-index 1)
                                        curr-min
                                        curr-indices
                                ))
                                (
                                    else
                                    (
                                        build-structure
                                            (+ curr-index 1)
                                            curr-min
                                            (cons curr-index curr-indices)
                                    )
                                )
                    )
                    )
                )
                (build-structure 0 +inf.0 '())
                lst
        )))
    )
    (
        get-indices (f lst)
    )
)
)

(define (taskII t s) (
    cond 
        ((equal? t 1) s)
        ((equal? t 0) 0)
        (else 
            (let 
                ((s_quarter (/ s 4)))
                (
                    +
                    (taskII (vector-ref t 0) s_quarter)
                    (taskII (vector-ref t 1) s_quarter)
                    (taskII (vector-ref t 2) s_quarter)
                    (taskII (vector-ref t 3) s_quarter)
                )
            )
        )
))

(define (taskIII t) (
    call/cc (
        lambda (cc-exit) (
            begin 
                (define dfs 
                    (lambda (t s) (
                        cond
                        ((equal? t 1) s)
                        ((equal? t 0) 0)
                        (else 
                            (let 
                                (
                                    (s_quarter (/ s 4))
                                    (s_half (/ s 2))
                                )
                                (
                                    foldl
                                    (lambda (x result) (
                                        ; result = (list sum quarters_left)
                                        let* (
                                            (sum (car result))
                                            (quarters_left (cadr result))
                                            (new_sum (+ (dfs x s_quarter) sum))
                                            (max_sum (+ new_sum (* quarters_left s_quarter)))
                                            (min_sum new_sum)
                                        )
                                        (
                                            cond 
                                                ((<= max_sum s_half) (cc-exit #f))
                                                ((> min_sum s_half) (cc-exit #t))
                                                (else (list new_sum (- quarters_left 1)))
                                        )
                                    ))
                                    (list 0 3)
                                    (
                                        list
                                        (vector-ref t 0)
                                        (vector-ref t 1)
                                        (vector-ref t 2)
                                        (vector-ref t 3)
                                    )
                                )
                            ))
                        ))
                )                

            (let (
                (dfs-result (dfs t 1))
            )
            (
                cond
                ((= dfs-result 1) #t)
                ((= dfs-result 0) #f)
                (else dfs-result)
            ))
    )
    )
))

(define (taskIV-сс t s cc) (
    cond 
        ((equal? t 1) (cc s))
        ((equal? t 0) (cc 0))
        (else
            (let
                ((s_quarter (/ s 4)))
                (taskIV-сс 
                    (vector-ref t 0) 
                    s_quarter 
                    (lambda (x) (
                        taskIV-сс
                            (vector-ref t 1)
                            s_quarter
                            (lambda (y) (
                                taskIV-сс
                                    (vector-ref t 2)
                                    s_quarter
                                    (lambda (z) (
                                        taskIV-сс
                                            (vector-ref t 3)
                                            s_quarter
                                            (lambda (t) (cc (+ t z y x)))
                                    )
                                    )
                            )
                        ))
                    )
                )
            )
        )
))

(define (taskV . functions-list) (
    cond 
        ((empty? (cdr functions-list)) (car functions-list))
        (else (
            apply
                taskV
                (cons
                    (lambda (x) (
                        ; f1(f2(x))
                        (car functions-list); f1
                        (
                            (cadr functions-list); f2
                            x
                        )
                    ))
                    (cddr functions-list)
                )
        )
        )
))

; Tests

(taskI (list))
(taskI (list 2 -1 2 3))
(taskI (list -1 0 1 -1 0 1 -1))
(taskII #(1 0 0 #(1 1 1 0)) 16)
(taskIII #(1 1 0 #(1 1 1 0)))
(taskIII #(0 0 0 #(1 0 1 1)))
(taskIV-сс #(1 0 0 #(1 1 1 0)) 16 (lambda (x) x))
((taskV (lambda (x) (+ x 5)) (lambda (x) (* x 2)) (lambda (x) (* x 3))) 1) ; 1 * 3 * 2 + 5 = 11
