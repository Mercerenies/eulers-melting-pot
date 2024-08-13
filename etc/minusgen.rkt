
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
;;
;; I'm definitely using z and v for control flow. DO NOT touch these
;; vars. That's 17 variables left.
;;
;; Free-use variables:
;;
;; * k, m, and n for indexing and loops
;;
;; * t for miscellaneous temporary storage during movs
;;
;; * s for the current value of an array index
;;
;; * d for deltas in loops
;;
;; * u is the total number of primes we find
;;
;; Unused with no initial values: abefghl

(struct source-code (text lines-count))

(define (comment text)
  (source-code (format "# ~a\n" text) 0))

(define (code-newline)
  (source-code "\n" 0))

(define (-= a b)
  (when (and (number? b) (< b 0)) ; Just a sanity check; I'm sure I'll make this mistake a few times.
    (error "Cannot use negative number literals! Found ~a" b))
  (source-code (format "~a -= ~a\n" a b) 1))

(define (+= a b #:tmp [tmp 't])
  (prog (mov-negated tmp b)
        (-= a tmp)))

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
(define (mov a b #:tmp [tmp 't])
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
(define (jmp-forward rel #:tmp [tmp 't])
  (when (<= rel 0)
    (error "Expected positive rel, found ~a" rel))
  (prog (mov-negated tmp rel)
        (-= pointer tmp)))

(define (infinite-loop . body)
  (let ([inner-len (code-length body)])
    (prog (comment "Infinite loop")
          (indent 2 (prog body
                          (jmp-backward inner-len))))))

;; do-while loop that runs at least once, then as long as the variable
;; is less than or equal to zero. The variable MUST NOT be `p`. `p` is
;; clobbered at the beginning and end of each loop iteration.
;;
;; If the breaker variable is given, then that variable can be used
;; with the `do-last` special form inside the loop to abort future
;; loop iterations.
(define (do-while-non-positive var
                               #:breaker [breaker-var #f]
                               #:comment [loop-comment (format "Do while ~a" var)]
                               . body)
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
    (prog (comment loop-comment)
          (indent 2 (if breaker-var
                        (prog (mov breaker-var mock-loop-len #:tmp 'p)
                              (loop-body breaker-var))
                        (loop-body mock-loop-len))))))

;; Runs for each integer from the negative of the limit up to zero
;; (inclusive). The limit must be a positive constant, NOT a variable.
;;
;; var must not be `p`. Clobbers `p`.
(define (do-for var limit #:breaker [breaker-var #f] . body)
  (prog (mov-negated var limit)
        (do-while-non-positive var #:breaker breaker-var #:comment (format "Do for ~a from ~a to 0" var limit)
            body
            (-= var 'z))))

;; Break out of a loop using a breaker-var. Note that this does NOT
;; cancel the current loop iteration but merely stops the loop from
;; running again.
(define (do-last breaker-var)
  (=0 breaker-var))

;; Runs the condition body IF the variable is positive. var must not
;; be `p`. Clobbers `p`.
(define (do-if-positive var #:comment [code-comment (format "Do if ~a" var)] . body)
  (let ([body-len (code-length body)])
    (prog (comment code-comment)
          (indent 2 (prog (mov-negated 'p 'v)
                          (mov-negated 'Z body-len)
                          (=0 'Y)
                          (=0 'p)
                          (-= 'p var)
                          (-= 'p 'v)
                          (-= 'c 'Z)
                          body)))))

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

;; Indent the code, for pretty-printing purposes.
(define (indent amount code)
  (cond
    [(list? code) (map (lambda (x) (indent amount x)) code)]
    [(source-code? code) (struct-copy source-code code
                                      [text (format "~a~a" (make-string amount #\space) (source-code-text code))])]
    [else (error "Invalid type for indent: ~a" code)]))

(define (print-code code)
  (display (code-text code)))

;; Sieve of Eratosthenes ranges from -50,000,000 up to (and excluding) 0.
(define sieve-length 50000000)

;; We start storing prime numbers at index 50,000,000.
(define primes-start-index 50000000)

(define sieve-of-eratosthenes
  (prog (comment "Sieve of Eratosthenes")
        (mov-negated 'n primes-start-index) ; Negative of index into primes array
        (do-for 'k (- sieve-length 2)
          (mov 'p 'k)
          (mov-negated 'A 'z))
        (do-for 'k (- sieve-length 2)
          (mov 'p 'k)
          (mov 's 'A)
          (do-if-positive 's #:comment "If current index is prime"
            ;; d is the negative of the prime number we're working with right now
            (mov-negated 'd 'k)
            (-= 'd sieve-length)
            ;; Store the prime in the positive side of our array
            (mov-negated 'p 'n)
            (mov-negated 'A 'd)
            (-= 'n 1)
            ;; Cross out multiples of the index
            (mov 'm 'k)
            (do-while-non-positive 'm #:comment "Mark multiples of the index as non-prime"
              (-= 'm 'd)
              (mov 'p 'm)
              (=0 'A))))
        (comment "Set total prime count")
        (-= 'u 'n)
        (-= 'u primes-start-index)
        (code-newline)))

(define project-euler-187
  (prog
    sieve-of-eratosthenes
    (mov 'p primes-start-index)
    (debug)))


(print-code project-euler-187)
