#lang racket

(define (make-queue) (
    ; ('queue . (null . null))
    mcons 'queue (mcons null null)
))

(define (queue? q) (
    and (mpair? q) (eq? 'queue (mcar q))
))

(define (front-queue q) (
    mcar (mcdr (mcdr q))
))

(define (empty-queue? q) (
    and (queue? q) (
        null? (mcdr (mcdr q))
    )
))

(define (insert-queue! q e) (
    let ((inserted-value-elem (mcons e null))) (
        if (empty-queue? q)
            (
                begin
                (set-mcar! (mcdr q) inserted-value-elem)
                (set-mcdr! (mcdr q) inserted-value-elem)
            )
            (
                begin
                (set-mcdr! (mcar (mcdr q)) inserted-value-elem) ; link from old end to new elem
                (set-mcar! (mcdr q) inserted-value-elem)
            )
    )
))

(define (delete-queue q) (
    set-mcdr! (mcdr q) (mcdr(mcdr (mcdr q)))
))

(define q (make-queue))

(insert-queue! q 1)
(insert-queue! q 2)
(insert-queue! q 3)

(front-queue q)
(delete-queue q)
(front-queue q)
(delete-queue q)
(front-queue q)
(delete-queue q)

(empty-queue? q)

(insert-queue! q 4)
(insert-queue! q 5)

(front-queue q)
(delete-queue q)
(front-queue q)
(delete-queue q)