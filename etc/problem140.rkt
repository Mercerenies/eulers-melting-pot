
#lang racket

(let ([total 0]
      [solutions-found 0]
      [qsqrt 2]) ; Note: q=1 gives us trivial solution of k=0, which we exclude.
  (let loop ()
    (let ([q (* qsqrt qsqrt)])
      (let-values ([(isqrt rem) (integer-sqrt/remainder (+ 44 (* 5 q)))])
        (let ([k5 (- isqrt 7)])
          (when (and (= (modulo k5 5) 0) (= rem 0))
            (let ([k (quotient k5 5)])
              (set! total (+ k total))
              (set! solutions-found (+ solutions-found 1))
              (printf "~a ~a~%" q k))))))
    (set! qsqrt (+ qsqrt 1))
    (when (< solutions-found 30)
      (loop)))
  (printf "~a~%" total))
