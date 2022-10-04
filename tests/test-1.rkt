#lang scheme/base
(require math/number-theory)

(define (vector-fold-left func init-val vctr) (
    ; `func` has 3 arguments: index, val, el
    ; `val` is the current value/result of folding
    let ((length-of-vector (vector-length vctr))) ( ; calculated once => it's efficient
        let loop ((func func) (val init-val) (vctr vctr) (index 0)) (
            let ((new-index (+ index 1))) (
                if (equal? index length-of-vector) val (
                    ; we don't copy the vector, just pass the new index
                    loop func (func index val (vector-ref vctr index)) vctr new-index
                )
            )
        )
    )
))


(define (fun2a n) ( ; generates a recursive process
        let loop ((x 2)) (
            if (> x n) '() (
                if (and (divides? x n) (not (prime? x)))
                    (cons x (loop (+ x 1)))
                    (loop (+ x 1))
            )
        )
))


(define (fun2b n) ( ; generates an iterative process
        reverse ( ; O(n) after the iterative process
                let loop ((x 2) (result '())) ( ; get (reversed) list
                if (> x n) result (
                    if (and (divides? x n) (not (prime? x)))
                        (loop (+ x 1) (cons x result))
                        (loop (+ x 1) result)
                )
            )
        )
))


(define (fun3 n) (
    let loop ((x 2) (n n) (result 1)) (
        if (= n 0) result (
            if (prime? x)
                (loop (+ x 1) n result)
                (loop (+ x 1) (- n 1) (* result x))
        )
    )
))


(define (fun4-recursive tree r1 r2) ( ; non-cps, preparing for cps..
    let (
        (get-data (lambda (tree) (vector-ref tree 0)))
        (get-left (lambda (tree) (vector-ref tree 1))) ; get left subtree
        (get-right (lambda (tree) (vector-ref tree 2))) ; get right subtree
    ) (
        let loop ((tree tree) (r1 (min r1 r2)) (r2 (max r1 r2)) (r 0)) (
            if (equal? tree #()) 0 (
                if (and (positive? (get-data tree)) (>= r r1) (<= r r2))
                    (
                        + 1 (loop (get-left tree) r1 r2 (+ r 1)) (loop (get-right tree) r1 r2 (+ r 1))
                    ) 
                    (
                        + 0 (loop (get-left tree) r1 r2 (+ r 1)) (loop (get-right tree) r1 r2 (+ r 1))
                    )
            )
        )
    )
))

(define (fun4 tree r1 r2) ( ; cps => tail recursion
    let (
        (get-data (lambda (tree) (vector-ref tree 0)))
        (get-left (lambda (tree) (vector-ref tree 1))) ; get left subtree
        (get-right (lambda (tree) (vector-ref tree 2))) ; get right subtree

    ) (
        let loop-cps ((tree tree) (r1 (min r1 r2)) (r2 (max r1 r2)) (r 0) (cc (lambda (x) x))) (
            if (equal? tree #()) (cc 0) (
                loop-cps (get-left tree) r1 r2 (+ r 1) (lambda (x) (
                    loop-cps (get-right tree) r1 r2 (+ r 1) (lambda (y) (
                        cc (
                            +
                                (if (and (positive? (get-data tree)) (>= r r1) (<= r r2)) 1 0)
                                x
                                y
                            )
                    )))
                )
            )
        )
    )
))

(define (fun5 tree) (
    begin (
        define (get-tree-height tree) ( ; calculate tree height
                if (equal? tree #()) 0 (
                    +
                        1
                        (
                            max
                            (get-tree-height (vector-ref tree 1))
                            (get-tree-height (vector-ref tree 2))
                        )
                )
            )
    )
    (
        let (
            (get-data (lambda (tree) (vector-ref tree 0)))
            (get-left (lambda (tree) (vector-ref tree 1))) ; get left subtree
            (get-right (lambda (tree) (vector-ref tree 2))) ; get right subtree
        ) (
            call/cc (
                lambda (cc-exit) (
                    let is-heap ((tree tree) (parent-data +inf.0)) (
                        if (equal? tree #()) #t (
                            if (
                                and 
                                (>= parent-data (get-data tree))
                                (<= (abs (- (get-tree-height (get-left tree)) (get-tree-height (get-right tree)))) 1)
                            )
                            (
                                and
                                    (is-heap (get-left tree) (min parent-data (get-data tree)))
                                    (is-heap (get-right tree) (min parent-data (get-data tree)))
                            )
                            (cc-exit #f)
                        )
                    )
                )
            )
        )
    )
))

; TESTS

(vector-fold-left (lambda (i val el) (add1 val)) 0 #())
(vector-fold-left (lambda (i val el) (add1 val)) 0 #(10 20 30))

(fun2a 1)
(fun2a 2)
(fun2a 12)

(fun2b 1)
(fun2b 2)
(fun2b 12)

(fun3 0)
(fun3 4)
(fun3 10)

(fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1)
(fun4 #() 1 10)
(fun4 #(10 #() #()) 0 0)

(fun5 #())
(fun5 #(10 #() #()))
(fun5 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())))
(fun5 #(1 #(-1 #(-3 #() #()) #(-3 #() #())) #(-2 #() #())))
