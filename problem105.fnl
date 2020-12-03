;; -*- Scheme -*-

(fn split [s]
    (let [fields {}]
      (s:gsub "[^,]+" (fn [c] (table.insert fields c)))
      fields))

(fn parse-line [s]
    (let [nums {}]
      (each [_ v (ipairs (split s))]
            (table.insert nums (tonumber v)))
      nums))

(fn shallow-copy [x]
    (let [y {}]
      (each [k v (pairs x)]
            (tset y k v))
      y))

(fn condition1 [nums]
    (var result true)
    (let [sums {0 true}]
      (each [_ x (ipairs nums)]
            (let [sums1 {}]
              (each [y _ (pairs sums)]
                    (table.insert sums1 (+ x y)))
              (each [_ z (ipairs sums1)]
                    (if (. sums z)
                        (set result false)
                        (tset sums z true))))))
    result)

(fn condition2 [nums]
    (var result true)
    (let [nums (shallow-copy nums)]
      (table.sort nums)
      (var a (. nums 1))
      (var b 0)
      (for [i 1 (/ (length nums) 2)]
           (set a (+ a (. nums (+ i 1))))
           (set b (+ b (. nums (- (length nums) i -1))))
           (when (<= a b)
             (set result false))))
    result)

(fn sum [nums]
    (var count 0)
    (each [_ x (ipairs nums)]
          (set count (+ count x)))
    count)

(var total)
(each [line (io.lines "./files/p105_sets.txt")]
      (let [nums (parse-line line)]
        (when (and (condition1 nums) (condition2 nums))
          (set total (+ total (sum nums))))))
(print total)
