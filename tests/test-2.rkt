#lang racket

; функции для работы с деревьями на векторах из лекций
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))


(define (fun2-i tree)
    (call/cc (lambda (cc-exit) (
        let inner-loop ((tree tree) (return-bool #t) (min_value -inf.0) (max_value +inf.0)) (
            if (empty-tree? tree)
            (if return-bool #t 0)
            (
                if (and (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree))) 
                    (if (and (>= (tree-data tree) min_value) (<= (tree-data tree) max_value)) (
                        if return-bool #t 1
                    ) (
                        cc-exit #f
                    ))
                    (
                        let* (
                            (h1 (inner-loop (tree-left tree) #f (min min_value (tree-data tree)) max_value))
                            (h2 (inner-loop (tree-right tree) #f min_value (max max_value (tree-data tree))))
                            (h_min (min h1 h2))
                            (h_max (max h1 h2))
                        ) (
                            if (equal? (- h_max h_min) 1) (
                                if return-bool #t (+ 1 h_max)
                            ) (
                                cc-exit #f
                            )
                        )
                    )
            )
        )
    )))
)

(fun2-i #())
(fun2-i (vector 0 (vector -1 #() #()) (vector 4 (vector 3 #() #()) #())))
(fun2-i #(7 #(5 #(4 #() #()) #(6 #() #())) #()))
(fun2-i #(7 #(6 #(3 #() #()) #(9 #() #())) #(9 #() #())))


(define (f-2-ii lst) 
    (car (
        foldl (lambda (x so-far) (
            let ((unique_elems_n (car so-far)) (unique_elems (cadr so-far))) (
                if (member x unique_elems) so-far (
                    list (+ unique_elems_n 1) (cons x unique_elems)
                )
            )
        )) (list 0 '()) lst
    ))
)

(f-2-ii (list 1 2 1 3 1 4))

; порождающая функция
(define (stream2-3-f i x1 x2 x3 x4 x5) (
    if (equal? i (+ x1 x2 x3 x4 x5)) (
        stream2-3-f (+ i 1) x2 x3 x4 x5 (+ x1 x2 x3 x4 x5)
    ) (
        stream-cons i (stream2-3-f (+ i 1) x1 x2 x3 x4 x5)
    )
))

(define stream2-3 (
    stream2-3-f 1 0 0 0 0 1
))

(stream->list (stream-take stream2-3 12))

(define (fun-2-iv f_)
    (define counter 0)
    (define f f_)
    (lambda vals
        (cond 
            ((equal? vals '(get-counter)) counter)
            ((equal? vals '(zero-counter)) (set! counter 0))
            (else (
                begin
                (set! counter (+ 1 counter))
                (apply f vals)
            ))
        )
    )
)

(define new-f (fun-2-iv +))
(println 'get-counter)
(new-f 'get-counter)
(new-f)
(println '(calling f))
(new-f 1)
(new-f 1 2)
(new-f 1 2 3)
(println 'get-counter)
(new-f 'get-counter)
(println '(zero-counter and get-counter))
(new-f 'zero-counter)
(new-f 'get-counter)

(define (fun2-v-cps tree cc) (
    call/cc (lambda (cc-exit) (
            let inner-loop ((tree tree) (return-bool #t) (min_value -inf.0) (max_value +inf.0) (cc cc)) (
                if (empty-tree? tree)
                (if return-bool (cc #t) (cc 0))
                (
                    if (and (empty-tree? (tree-left tree)) (empty-tree? (tree-right tree)))
                        (if (and (>= (tree-data tree) min_value) (<= (tree-data tree) max_value)) (
                            if return-bool 
                            (cc #t)
                            (cc 1)
                        ) (
                            cc-exit #f
                        ))
                        (
                            inner-loop
                            (tree-left tree)
                            #f
                            (min min_value (tree-data tree))
                            max_value
                            ; cc arg:
                            (lambda (h1) (
                                inner-loop
                                    (tree-right tree)
                                    #f
                                    min_value
                                    (max max_value (tree-data tree))
                                    ; cc arg:
                                    (lambda (h2) (
                                        let (
                                            (h_min (min h1 h2))
                                            (h_max (max h1 h2))
                                        ) (
                                            if (equal? (- h_max h_min) 1) (
                                                if return-bool (cc #t) (cc (+ 1 h_max))
                                            ) (
                                                cc-exit #f
                                            )
                                        )
                                    ))
                                )
                            )
                        )
                )
            )
        ))
))

(println '(testing fun2-v-cps))
(fun2-v-cps #() (lambda (x) x))
(fun2-v-cps (vector 0 (vector -1 #() #()) (vector 4 (vector 3 #() #()) #())) (lambda (x) x))
(fun2-v-cps #(7 #(5 #(4 #() #()) #(6 #() #())) #()) (lambda (x) x))
(fun2-v-cps #(7 #(6 #(3 #() #()) #(9 #() #())) #(9 #() #())) (lambda (x) x))


; задание 2.VI
; в процессе обнаружилось, что была лишняя закрывающая
; скобка, исправил

; (λx. ((λz. (λy. (z x))) x ((λy. y y) (λx. x x)))) y z
; ((λz. (λy_0. (z y))) y ((λy. y y) (λx. x x))) z
; ((λy_0. (y y)) ((λy. y y) (λx. x x))) z
; (y y) z -- нормальная форма
