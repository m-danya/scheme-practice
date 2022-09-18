#lang racket

(define (is_natural? x)
  (and (integer? x) (> x 0))
)

(define
  (2n-1!-list-recursive n)
  (if (is_natural? n) 
    (
      if (= n 1)
      '(1)
      (let* (
          (prev_list (2n-1!-list-recursive (- n 1)))
          (prev_list_last_elem (list-ref prev_list (- n 2)))
          (2n-2 (- (* 2 n) 2))
          (2n-1 (- (* 2 n) 1))
        )
      (append prev_list (list (* prev_list_last_elem 2n-1 2n-2)))
      )
    )
    '())
)

(define 
  (2n-1!-list-iterative n)
  (if (is_natural? n)
    (
      let loop ((k 1) (result '()))
      (
        let
        (          
          (2k-2 (- (* 2 k) 2))
          (2k-1 (- (* 2 k) 1))
        )
        (
          if (> k n) 
          result
          (
            loop (+ k 1) (
              if (= k 1)
              '(1)
              (append result (list (* (list-ref result (- k 2)) 2k-1 2k-2)))
            )
          )
          )
      )
    )
    '()
  )
)


; (2n-1!-list-recursive 'abc)
; (2n-1!-list-recursive -5)
; (2n-1!-list-recursive 0)
; (2n-1!-list-recursive 1)
; (2n-1!-list-recursive 2)
; (2n-1!-list-recursive 3)
; (2n-1!-list-recursive 4)
; (2n-1!-list-recursive 10)

; (2n-1!-list-iterative 'abc)
; (2n-1!-list-iterative -5)
; (2n-1!-list-iterative 0)
; (2n-1!-list-iterative 1)
; (2n-1!-list-iterative 2)
; (2n-1!-list-iterative 3)
; (2n-1!-list-iterative 4)
; (2n-1!-list-iterative 10)