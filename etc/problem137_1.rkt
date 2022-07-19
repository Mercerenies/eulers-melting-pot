#lang racket

;; From the numbers produces by problem137.rkt, it's OEIS A081018
;;
;; https://oeis.org/A081018

(require memo)

(define/memoize (fibo n)
  (if (<= n 2)
      1
      (+ (fibo (- n 2)) (fibo (- n 1)))))

(let ([target 15]
      [sum 0])
  (for ([i target])
    (set! sum (+ sum (fibo (+ (* 4 i) 3)))))
  (println sum))
