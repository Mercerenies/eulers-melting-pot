
#lang racket

;; A truly absurd number of Conway topograph diagrams later, we find
;; the following.
;;
;; The infinite polynomial series in the problem can be written as
;;
;; k = (x + 3x²) / (1 - x - x²)
;;
;; Simplify into a polynomial to find
;;
;; 0 = (k + 3) x² + (k + 1) x - k
;;
;; Quadratic formula gives us a discriminant of
;;
;; Δ = (k + 1)² + 4 (k + 1) k = 5 k² + 14 k + 1
;;
;; x is rational exactly when the discriminant is a perfect square, so
;; we want, for some w,
;;
;; w² = 5 k² + 14 k + 1
;;
;; Complete the square, multiply by common fraction, and simplify to get
;;
;; 44 = (5 k + 7)² - 5 w²
;;
;; Substitute D = 5 k + 7, and we have the Pell-type equation
;;
;; 44 = D² - 5 w²
;;
;; (Insert massive Conway topograph here)
;;
;; It turns out it has a whopping *six* fundamental solutions: (7, 1),
;; (17, 7), (8, -2), (8, 2), (13, 5), (43, 19).
;;
;; The recurrence, which can be gotten from the Conway topograph or
;; from the wonderful Wikipedia page on Pell equations, is
;; A=((9,20),(4,9)).
;;
;; We only care about cases where k is an integer, i.e. where D = 2
;; (mod 5). That's every two iterations, with some of them on the even
;; iteration of the fundamental solution and some on the odd.
;; Accounting for the solutions we actually want to start at (and
;; skipping (7,1), which corresponds to k=0, the trivial answer), our
;; fundamental solutions are
;;
;; (17,7), (32,14), (112,50), (217,97), (767,343), (1487,665)
;;
;; And the recurrence is A²=((161,360),(72,161)). We have six
;; fundamental solutions. We want the first 30. Iterate five times,
;; take a sum, and go get some ice cream, because if you actually read
;; this entire comment, you deserve it.

(define (next-solution vec)
  (let ([x (first vec)]
        [y (second vec)])
    (list (+ (* 161 x) (* 360 y)) (+ (* 72 x) (* 161 y)))))

(define (d->k d)
  (/ (- d 7) 5))

(let ([total 0]
      [vectors '((17 7) (32 14) (112 50) (217 97) (767 343) (1487 665))])
  (for ([i 5])
    ;; Update the total
    (for/list ([vec vectors])
      (set! total (+ total (d->k (first vec)))))
    ;; Update the vectors
    (set! vectors (map next-solution vectors)))
  (printf "~a~%" total))
