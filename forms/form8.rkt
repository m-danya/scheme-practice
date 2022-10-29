#lang racket

; (require racket/promise)

(define ones (stream-cons 1 ones))

(define (stream-scale s t) ; multiply stream elements by t
    (stream-map (lambda (x) (* x t)) s)
)

(define (powers x)
    (stream-cons 1 (stream-scale (powers x) x))
)

(define powers-of-3-and-7 (
    let gen
            ((s3 (powers 3)) (s7 (stream-rest (powers 7)))) ; avoid two ones
                (
                    let ((s3-head (stream-first s3)) (s7-head (stream-first s7))) (
                        if (< s3-head s7-head)
                            (stream-cons s3-head (gen (stream-rest s3) s7))
                            (stream-cons s7-head (gen s3 (stream-rest s7)))
                    )
                )
))


(stream->list (stream-take powers-of-3-and-7 20))