
;; Generator script for the Underload programming language
;; (https://esolangs.org/wiki/Underload)
;;
;; Attempting to solve Project Euler Problem 206.

#lang racket

(require (for-syntax racket/list))
(require (for-syntax racket/list/grouping))
(require (for-syntax threading))

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

;; Flat fried quotations, top level only. No nesting. Parameters are
;; read in FIFO order, so the first fry uses the top of the stack
;; (this is opposite of Factor's convention).
;;
;; fry produces runnable code that immediately pops N values off the
;; stack and pushes a single quotation.

(define (compile-fry args)
  (define (calc-init args)
    (if (or (null? args) (eq? (car args) '_))
        (values "" args)
        (values (car args) (cdr args))))

  (let-values ([(init args) (calc-init args)])
    (apply string-append
           (quoted init)
           (for/list ([arg args])
             (if (eq? arg '_)
                 (program (swap) (cat))
                 (program (quoted arg) (cat)))))))

(define-syntax (fry stx)
  (define (underscore? a)
    (and (syntax? a)
         (eq? (syntax-e a) '_)))

  (define (by-is-underscore a b)
    (eq? (string? (syntax-e a)) (string? (syntax-e b))))

  (define (concatenate-if-not-underscores lst)
    (if (andmap (lambda (x) (not (underscore? x))) lst)
        #`(string-append #,@lst)
        lst))

  (define (protect-syntax a)
    (if (underscore? a) #'(quote _) a))

  (let ([args (~> (cdr (syntax->list stx))
                  (slice-by by-is-underscore _)
                  (map concatenate-if-not-underscores _)
                  flatten
                  (map protect-syntax _))])
    #`(compile-fry (list #,@args))))

;; Church Booleans

;; ( x y -- x )
(define true (program (discard)))

;; ( x y -- y )
(define false (program (swap) (discard)))

(define (choose-by-bool true-val false-val)
  (program (quoted true-val) (swap) (quoted false-val) (swap) (eval)))

(define (if-stmt true-case false-case)
  (program (choose-by-bool true-case false-case)
           (eval)))

;; Church Pairs.
;;
;; We encode pairs as simply programs that push both values to the
;; stack. This is easier than the "usual" Church encoding since we're
;; in a stack-based language and can easily "return" two values.

;; ( x y -- pair )
(define (pair)
  (program (enclose)
           (swap)
           (enclose)
           (fry _ _)))

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

(define (add) (fry (dup) _ (swap) _ (cat)))

(define euler206
  (program (quoted "first\n") (quoted "second\n")
           (pair)
           (eval) (output) (output)
           (quoted true)
           (if-stmt (quoted "true\n") (quoted "false\n"))
           (output)
           (quoted false)
           (if-stmt (quoted "true\n") (quoted "false\n"))
           (output)
           (quoted *2)
           (quoted *3)
           (add)
           (quoted "hello!\n")
           (swap)
           (eval)
           (output)))

(displayln euler206)
