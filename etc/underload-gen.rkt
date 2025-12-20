
;; Generator script for the Underload programming language
;; (https://esolangs.org/wiki/Underload)
;;
;; Attempting to solve Project Euler Problem 206.

#lang racket

(define program string-append)

(define (swap) "~")
(define (dup) ":")
(define (discard) "!")
(define (cat) "*")
(define (enclose) "a")
(define (eval) "^")
(define (output) "S")

(define (quoted . s)
  (format "(~a)" (apply string-append s)))

(define (nop) (program (quoted "") (discard)))

;; Small numerals
(define *0 (program (discard) (quoted "")))
(define *1 (program (nop)))
(define *2 (program (dup) (cat)))
(define *3 (program (dup) (dup) (cat) (cat)))
(define *4 (program *2 *2))
(define *5 (program (dup) (dup) (cat) (dup) (cat) (cat)))
(define *6 (program *3 *2))
(define *7 (program (dup) (dup) (dup) (cat) (cat) (dup) (cat) (cat)))
(define *8 (program *4 *2))
(define *9 (program *3 *3))

(define (dip . inner)
  (let ([inner (apply string-append inner)])
    (program (enclose)
             (quoted inner)
             (swap)
             (cat)
             (eval))))

(define (2dip . inner)
  (dip (apply dip inner)))

(define (mul) (cat))
(define (exp) (eval))

(define euler206
  (program (quoted *2)
           (dip (quoted "hello!\n"))
           (eval)
           (output)))

(displayln euler206)
