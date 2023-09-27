
(import fractions [Fraction])
(import dataclasses [dataclass])
(import itertools [islice])

(defclass [(dataclass :order True :frozen True)] Point []
  #^ Fraction x
  #^ Fraction y)

(defclass [dataclass] LineSegment []
  #^ Point first
  #^ Point second

  (defn __init__ [self first second]
    (if (> first second)
        (setv self.first second self.second first)
        (setv self.first first self.second second)))

  (defn [property] vertical? [self]
    (= self.first.x self.second.x))

  ;; Precondition: self is not a vertical line.
  (defn [property] slope [self]
    (/ (- self.second.y self.first.y) (- self.second.x self.first.x)))

  (defn f [self x]
    (let [m self.slope]
      (+ (* m (- x self.first.x)) self.first.y))))

(defn intersection-point [line1 line2]
  (cond
    (and line1.vertical? line2.vertical?) None
    line2.vertical? (intersection-point-vertical line2 line1)
    line1.vertical? (intersection-point-vertical line1 line2)
    True (intersection-point-general line1 line2)))

;; Precondition: line1 is vertical and line2 is not.
(defn intersection-point-vertical [line1 line2]
  (let [x line1.first.x
        y (line2.f x)]
    (when (and (< line2.first.x x line2.second.x) (< line1.first.y y line1.second.y))
      (Point x y))))

;; Precondition: Neither line is vertical.
(defn intersection-point-general [line1 line2]
  (let [m1 line1.slope
        m2 line2.slope]
    (when (!= m1 m2)
      (let [x (/ (+ line2.first.y (- line1.first.y) (* m1 line1.first.x) (- (* m2 line2.first.x))) (- m1 m2))
            y (line1.f x)]
        (when (and (< line1.first.x x line1.second.x) (< line2.first.x x line2.second.x))
          (Point x y))))))

(defn blum-blum-shub []
  (let [value 290797]
    (while True
      (setv value (% (* value value) 50515093))
      (yield (% value 500)))))

(defn numbers-to-segments [numbers]
  (try
   (let [numbers (iter numbers)]
     (while True
       (let [x1 (Fraction (next numbers))
             y1 (Fraction (next numbers))
             x2 (Fraction (next numbers))
             y2 (Fraction (next numbers))]
         (yield (LineSegment (Point x1 y1) (Point x2 y2))))))
   (except [StopIteration])))

(let [lines (list (numbers-to-segments (islice (blum-blum-shub) 20000)))
      intersection-points (set)]
  (lines.sort :key (fn [x] x.first))
  (for [i (range 0 (len lines))]
    (for [j (range (+ i 1) (len lines))]
      (let [line1 (. lines [i])
            line2 (. lines [j])]
        (when (> line2.first.x line1.second.x)
          (break))
        (let [point (intersection-point line1 line2)]
          (when point
            (intersection-points.add point))))))
  (print (len intersection-points)))
