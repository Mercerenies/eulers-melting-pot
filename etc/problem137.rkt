#lang racket

;; We start with the formula for the Fibonacci sequence:
;;
;; Fₙ = (ϕⁿ - ψⁿ) / √5
;;
;; ϕ = (1 + √5) / 2
;;
;; ψ = (1 - √5) / 2
;;
;; Write our A_F(x) out explicitly using the above formula. We get a
;; sum involving x, ϕ, and ψ. Simplify and split into two sums. The
;; first sum will be a geometric series in terms of (ϕ x)ⁿ and the
;; second will be a geometric series in terms of (ψ x)ⁿ. Use geometric
;; series formula to write those out explicitly. Simplify again, and
;; rationalize the denominator. You'll end up with
;;
;; A_F(x) = x / (1 - x - x²)
;;
;; Set s = A_F(x). Then
;;
;; s = x / (1 - x - x²)
;;
;; Solve for x to get
;;
;; x = ((s + 1) ± √(5 s² + 2 s + 1)) / (- 2 s)
;;
;; This is rational if and only if the discriminant is a perfect
;; square. So we want s values for which 5 s² + 2 s + 1 is a perfect
;; square. Write
;;
;; w = 5 s² + 2 s + 1
;;
;; Then solve for s to get
;;
;; s = (- 1 ± √(5 w - 4)) / 5
;;
;; So try perfect squares w in order, and if s is an integer then we
;; have a solution. This algorithm is monotonic and will produce
;; solutions in strictly ascending order.

;; ... ... ... too slow

(let ([wsqrt 2]
      [s 0]
      [found 0])
  (let loop ()
    (let ([discr (- (* 5 wsqrt wsqrt) 4)])
      (let-values ([(sqrt-discr remainder) (integer-sqrt/remainder discr)])
        (set! s (/ (- sqrt-discr 1) 5))
        (when (and (= remainder 0)
                   (integer? s))
          (println s)
          (set! found (+ found 1)))))
    (when (< found 15)
      (set! wsqrt (+ wsqrt 1))
      (loop)))
  (println s))
