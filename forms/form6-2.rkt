#lang racket

(require math/number-theory)

(define (get-x-dividers x) (
    let loop ((k 1) (result '())) (
        cond
            ((> k x) result)
            ((divides? k x) (
                loop (+ k 1) (cons k result)
            ))
            (else 
                (loop (+ k 1) result)
            )
    )
))

(define (sum-of-dividers x) (
    foldl + 0 (get-x-dividers x)
))

(define (is-abundant? x) (
    > (sum-of-dividers x) (* 2 x)
))

(define (odd-abundant n) (
    let loop ((found 0) (k 1)) (
        if (is-abundant? k)
            (
                let ((new-found (+ found 1))) (
                    if (equal? new-found n)
                        k
                        (loop new-found (+ k 2))
                )
            )
            (
                loop found (+ k 2)
            )

    )
))


(define hash-table (make-hash '())) ; global

(define (memo-odd-abundant n) 
    (define (get-max-useful-index-for-n n) (
        let ((max-of-keys (foldl max 0 (hash-keys hash-table)))) (
            min n max-of-keys
        )
    ))
    (define (get-nth n) (
        let* (
            (idx (get-max-useful-index-for-n n))
            (k (hash-ref hash-table idx 1))
        ) (
            let loop ((found (max 0 (- idx 1))) (k k)) (
                if (is-abundant? k)
                    (
                        let ((new-found (+ found 1))) (
                            begin
                            (hash-set! hash-table new-found k) 
                            (if (equal? new-found n)
                                    k
                                    (loop new-found (+ k 2)))
                        )
                    )
                    (
                        loop found (+ k 2)
                    )
            )
        )
    ))
    (get-nth n)
)

(println 'non-memo)
(odd-abundant 30)
(odd-abundant 30)
(odd-abundant 30)
(odd-abundant 30)
(odd-abundant 30)

(println 'memo)
(memo-odd-abundant 30)
(memo-odd-abundant 30)
(memo-odd-abundant 30)
(memo-odd-abundant 30)
(memo-odd-abundant 30)
(memo-odd-abundant 31)
(memo-odd-abundant 32)
(memo-odd-abundant 33)
(memo-odd-abundant 34)
(memo-odd-abundant 35)
(memo-odd-abundant 5)

; Ответы на вопросы

; Мемоизованная версия даёт возможность не перевычислять (1..n-1) элементы последовательности
; при последовательных вызовах для разных n, а сразу искать последующие члены, если это необходимо.
; Для одного вызова данная реализация не даёт преимущества по сравнению с обычной версией,
; т.к. сохраняет в таблицу результаты для разных n. 