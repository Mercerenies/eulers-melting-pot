#lang racket

;; From the numbers produces by problem137.rkt, it's OEIS A081018
;;
;; https://oeis.org/A081018

(require memo)

(define/memoize (fibo n)
  (if (<= n 2)
      1
      (+ (fibo (- n 2)) (fibo (- n 1)))))

(println (* (fibo 30) (fibo 31)))
