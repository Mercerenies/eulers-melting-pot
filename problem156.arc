
;; Don't have integer division built in, so we reconstruct it from
;; modulo :)
(def div (x y)
  (/ (- x (mod x y)) y))

(def ^ (x y)
  (if (is y 0)
    1
    (* x (^ x (- y 1)))))

(def to-num (xs)
  (if (cdr xs)
    (reduce (fn (a b) (+ (* 10 a) b)) xs)
    (if xs
      (car xs)
      0)))

(def from-num (n)
  (rev (from-num-acc n)))

(def from-num-acc (n)
  (if (is n 0)
    nil
    (cons (mod n 10) (from-num-acc (div n 10)))))

(def f-list (ns d)
  (if ns
    (let k (- (len ns) 1)
      (let below (if (is k 0) 0 (* (car ns) k (^ 10 (- k 1))))
        (let current (if (is (car ns) d)
                       (+ 1 (to-num (cdr ns)))
                       (if (> (car ns) d)
                         (^ 10 k)
                         0))
          (let recursive (f-list (cdr ns) d)
            (+ below current recursive)))))
    0))

(def f (n d)
  (f-list (from-num n) d))

(def find-all-fixed-points (lower upper d)
  (if (>= lower upper)
    0
    (if (is lower (- upper 1))
      (if (is lower (f lower d)) lower 0)
      (let midpoint (div (+ upper lower) 2)
        (let f-lower (f lower d)
          (let f-upper (f upper d)
            (let f-midpoint (f midpoint d)
              (let lower-sum (if (or (> f-lower midpoint) (< f-midpoint lower)) 0 (find-all-fixed-points lower midpoint d))
                (let upper-sum (if (or (> f-midpoint upper) (< f-upper midpoint)) 0 (find-all-fixed-points midpoint upper d))
                  (+ lower-sum upper-sum))))))))))

(def upper-limit () (^ 10 11))

(write (sum (fn (d) (find-all-fixed-points 0 (upper-limit) d)) (range 1 9)))
