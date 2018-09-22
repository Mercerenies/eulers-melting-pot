\version "2.18.2"

#(define (triangular n) (/ (* n (+ n 1)) 2))
#(define (square n) (* n n))
#(define (pentagonal n) (/ (* n (- (* 3 n) 1)) 2))
#(define (hexagonal n) (* n (- (* 2 n) 1)))
#(define (heptagonal n) (/ (* n (- (* 5 n) 3)) 2))
#(define (octagonal n) (* n (- (* 3 n) 2)))

#(define (generate-fours f n)
   (let ((curr (f n)))
     (cond
      ((> curr 9999) '())
      ((> curr  999) (cons curr (generate-fours f (+ n 1))))
      (#t            (generate-fours f (+ n 1))))))

#(define (split n)
   (let ((str (number->string n)))
     (cons (string->number (substring str 0 2))
           (string->number (substring str 2 4)))))

#(define (unsplit n)
   (string->number
    (string-append (number->string (car n)) (number->string (cdr n)))))

#(define (check-chain first chain)
   (cond
    ((null? chain) #t)
    ((null? (cdr chain)) (eqv? (caar chain) (cdr first)))
    ((eqv? (caar chain) (cdadr chain)) (check-chain first (cdr chain)))))

#(define (iterate visited full curr rest chain)
   (cond
    ((and (null? visited) (null? rest) (null? full))
     (if (check-chain (car chain) chain)
         chain
         #f))
    ((null? curr)
     (if (null? rest)
         #f
         (iterate (if (null? full) visited (cons full visited))
                  (car rest)
                  (car rest)
                  (cdr rest)
                  chain)))
    ((or (null? chain) (eqv? (cdar chain) (caar curr)))
     (or (iterate '() '() '() (append visited rest) (cons (car curr) chain))
         (iterate visited full (cdr curr) rest chain)))
    (#t
     (iterate visited full (cdr curr) rest chain))))

triangles = #(map split (generate-fours triangular 1))
squares = #(map split (generate-fours square 1))
pents = #(map split (generate-fours pentagonal 1))
hexes = #(map split (generate-fours hexagonal 1))
hepts = #(map split (generate-fours heptagonal 1))
octs = #(map split (generate-fours octagonal 1))

solution = #(iterate '() '() '() (list octs hepts hexes pents squares triangles) '())

lyric = \lyricmode {
  #(number->string (fold + 0 (map unsplit solution)))
}

\score {
  <<
    \new Voice = "one" {
      e'
    }
    \new Lyrics \lyricsto "one" {
      \lyric
    }
  >>
}
