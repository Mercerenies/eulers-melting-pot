
#lang racket

;; Generator script for Burlesque programs.

(define (seq . args)
  (string-join args))

(define (proc name . body)
  (format "proc ~a { ~a }"
          name
          (apply seq body)))

(define (call procedure-name)
  (format "call ~a" procedure-name))

(define (set n)
  (format "s~a" n))

(define (get n)
  (format "g~a" n))

(define (pushconst n)
  (~a n))

(define (block . body)
  (format "{ ~a }" (apply seq body)))

(define (if-else i e)
  (format "{ ~a } \\/ { ~a } \\/ ie" i e))

(define add ".+")
(define sub ".-")
(define mul ".*")
(define div "./")
(define mod ".%")
(define pow "**")
(define factors "fc")
(define llength "L[")
(define lrange "r@")
(define lmap "m[")
(define sum "++")
(define pop "vv")
(define lnull "nu")
(define swap "\\/")
(define while "w!")

;; Variables (we use single-digit names so we can use the easy
;; assignment syntax). None of our code is recursive so nothing needs
;; to be re-entrant.
(define a 0)
(define b 1)
(define twos 2)
(define fives 3)
(define p 4)
(define n 5)
(define 10^n 6)
(define t 7) ; Also used for outer a and b in the main loop.
(define f 8) ; Used for inner a and b in the main loop.

(displayln (seq
            (proc 'count_for ; Takes ten_n, a, b as args
                  (set b)
                  (set a)
                  (get a) (get b) add mul
                  (get a) (get b) mul div
                  (set p)
                  (pushconst 0) (set twos)
                  (pushconst 0) (set fives)
                  (block (get p) (pushconst 2) div (set p) (get twos) (pushconst 1) add (set twos)) (block (get p) (pushconst 2) mod (pushconst 0) "==") while
                  (block (get p) (pushconst 5) div (set p) (get fives) (pushconst 1) add (set fives)) (block (get p) (pushconst 5) mod (pushconst 0) "==") while
                  (get p) factors llength (get twos) (pushconst 1) add mul (get fives) (pushconst 1) add mul)
            (proc 'count_solutions ; Takes n
                  (set n)
                  (pushconst 10) (get n) pow (set 10^n)
                  (pushconst 0) (get n) lrange
                  (block (pushconst 2) swap pow
                         (set t)
                         (pushconst 0) (get n) lrange
                         (block (pushconst 5) swap pow
                                (get 10^n) swap (pushconst 1) swap (get t) mul
                                (call 'count_for))
                         lmap sum)
                  lmap sum
                  (pushconst 1) (get n) lrange
                  (block (pushconst 2) swap pow
                         (set t)
                         (pushconst 0) (get n) lrange
                         (block (pushconst 5) swap pow
                                (set f)
                                (get t) (get f) "<=" (if-else (seq (get 10^n) (get t) (get f) (call 'count_for))
                                                              (seq (pushconst 0))))
                         lmap sum)
                  lmap sum
                  (pushconst 1) (get n) lrange
                  (block (pushconst 5) swap pow
                         (set t)
                         (pushconst 0) (get n) lrange
                         (block (pushconst 2) swap pow
                                (set f)
                                (get t) (get f) "<=" (if-else (seq (get 10^n) (get t) (get f) (call 'count_for))
                                                              (seq (pushconst 0))))
                         lmap sum)
                  lmap sum
                  add add)
            (pushconst 1) (pushconst 9) lrange
            (block (call 'count_solutions)) lmap sum))
