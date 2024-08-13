
#lang racket

;; Generator for Minus programs. See http://www.golfscript.com/minus/basic.html

;; Analysis of variables:
;;
;; There are 26 lowercase letters of the alphabet. Of these, the core
;; language uses "c", "p", "o", and "i" for special variables. The
;; standard extensions additionally reserve "j", "q", and "r". That
;; leaves 19 variables free for us to use as we please:
;; abdefghklmnstuvwxyz. Note that vwxyz start with nonzero initial
;; values, so that requires a bit of care to initialize these
;; variables if we want to use them for something other than the
;; default.
;;
;; Default values for predefined vars:
;; z = -1, y = -2, x = -10, w = -32, v = -(max value of p)

(struct source-code (text lines-count))

(define (comment text)
  (source-code (format "# ~a\n" text) 0))

(define (-= a b)
  (when (and (number? b) (< b 0)) ; Just a sanity check; I'm sure I'll make this mistake a few times.
    (error "Cannot use negative number literals! Found ~a" b))
  (source-code (format "~a -= ~a\n" a b) 1))

;; Sets the variable to zero. Uses 1 line of code.
(define (=0 a)
  (-= a a))

;; Sets the variable to the negative of the given value. Uses 2 lines
;; of code.
(define (mov-negated a b)
  (prog (=0 a)
        (-= a b)))

;; Sets the variable to the given variable or literal. Uses a third
;; variable as temporary storage. Uses 4 lines of code.
(define (mov a b #:tmp tmp)
  (prog (mov-negated tmp b)
        (mov-negated a tmp)))

;; Jumps to a relative position earlier in the source code
;; unconditionally. Uses one line of code.
(define (jmp-backward rel)
  (when (<= rel 0)
    (error "Expected positive rel, found ~a" rel))
  (-= pointer (+ rel 1))) ; + 1 to include this instruction.

;; Jumps to a relative position later on in the source code
;; unconditionally. Uses three lines of code.
(define (jmp-forward rel #:tmp tmp)
  (when (<= rel 0)
    (error "Expected positive rel, found ~a" rel))
  (prog (mov-negated tmp rel)
        (-= pointer tmp)))

(define (infinite-loop . body)
  (let ([inner-len (code-length body)])
    (prog body
          (jmp-backward inner-len))))

;; do-while loop that runs at least once, then as long as the variable
;; is less than or equal to zero. The variable MUST NOT be `p`. `p` is
;; clobbered at the beginning and end of each loop iteration.
(define (do-while-non-positive var . body)
  (define (loop-body inner-code-len)
    (prog body
          (mov-negated 'p 'v)
          (mov 'Z inner-code-len #:tmp 'Y)
          (=0 'Y)
          (=0 'p)
          (-= 'p var)
          (-= 'p 'v)
          (-= 'c 'Z)))
  (let* ([mock-loop-body (loop-body 1)]
         [mock-loop-len (code-length mock-loop-body)])
    (loop-body mock-loop-len)))

;; Runs for each integer from the negative of the limit up to zero
;; (inclusive). The limit must be a positive constant, NOT a variable.
;;
;; var must not be `p`. Clobbers `p`.
(define (do-for var limit . body)
  (prog (mov-negated var limit)
        (do-while-non-positive var
            body
            (-= var 'z))))

;; Runs the condition body IF the variable is positive. var must not
;; be `p`. Clobbers `p`.
(define (do-if-positive var . body)
  (let ([body-len (code-length body)])
    (prog (mov-negated 'p 'v)
          (mov-negated 'Z body-len)
          (=0 'Y)
          (=0 'p)
          (-= 'p var)
          (-= 'p 'v)
          (-= 'c 'Z)
          body)))

;; Instruction pointer
(define pointer 'c)

(define array-index 'p)

(define (rel-index index)
  (let* ([capital-a (char->integer #\A)]
         [offset (- index capital-a)])
    (if (<= 0 offset 25)
        (integer->char (+ capital-a offset))
        (error "Index out of range"))))

(define (output-neg var)
  (-= 'q var))

(define (output-newline)
  (-= 'o 246))

(define (debug)
  (-= 'a '_))

(define (prog . code)
  code)

;; Accepts arbitrarily-nested lists of source code.
(define (code-text code)
  (cond
    [(list? code) (string-append* (map code-text code))]
    [(source-code? code) (source-code-text code)]
    [else (error "Invalid type for code-text: ~a" code)]))

(define (code-length code)
  (cond
    [(list? code) (apply + (map code-length code))]
    [(source-code? code) (source-code-lines-count code)]
    [else (error "Invalid type for code-length: ~a" code)]))

(define (print-code code)
  (display (code-text code)))

(define project-euler-187
  (prog (mov-negated 'e 5)
        (do-for 'd 100
          (do-if-positive 'e
            (output-neg 'e)
            (output-newline))
          (-= 'e 'z))
        (output-neg 7)
        (output-newline)))

(print-code project-euler-187)
