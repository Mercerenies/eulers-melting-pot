
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
(define (output-prim) "S")

;; Convenience wrapper that can output constant text.
;;
;; With no argument: ( x -- )
;;
;; With Racket-side argument: ( -- )
(define (output [text #f])
  (if text
      (program (quoted text) (output-prim))
      (output-prim)))

(define (quoted . s)
  (format "(~a)" (apply string-append s)))

(define (nop) (program (quoted "") (discard)))

(define (comment text)
  (program (quoted text) (discard)))

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
(define *10 (program *5 *2))

;; Flat fried quotations, top level only. No nesting. Parameters are
;; read in FIFO order, so the first fry uses the top of the stack
;; (this is opposite of Factor's convention).
;;
;; fry produces runnable code that immediately pops N values off the
;; stack and pushes a single quotation.

(define (compile-fry args)
  (define (calc-init args)
    (if (or (null? args) (memq (car args) '(_ <_>)))
        (values "" args)
        (values (car args) (cdr args))))

  (let-values ([(init args) (calc-init args)])
    (apply string-append
           (quoted init)
           (for/list ([arg args])
             (cond
               [(eq? arg '_) (program (swap) (cat))]
               [(eq? arg '<_>) (program (swap) (enclose) (cat))]
               [else (program (quoted arg) (cat))])))))

(define-for-syntax (count-fry-args args)
  (define (underscore? a)
    (memq a '(_ <_>)))

  (let ([args (flatten (syntax->datum args))])
    (count underscore? args)))

(define-syntax (fry stx)
  (define (preprocess-nested-fry a)
    (let ([a-expr (syntax-e a)])
      (if (and (pair? a-expr) (syntax? (car a-expr)) (eq? (syntax-e (car a-expr)) 'quoted))
          (let ([fry-count (count-fry-args a)])
            (append (make-list fry-count #'<_>) (list #`(fry #,@(cdr a-expr)))))
          a)))

  (define (underscore? a)
    (and (syntax? a)
         (not (eq? #f (memq (syntax-e a) '(_ <_>))))))

  (define (by-is-underscore a b)
    (eq? (underscore? a) (underscore? b)))

  (define (concatenate-if-not-underscores lst)
    (if (andmap (lambda (x) (not (underscore? x))) lst)
        #`(string-append #,@lst)
        lst))

  (define (protect-syntax a)
    (if (underscore? a) #`(quote #,(syntax-e a)) a))

  (let ([args (~> (cdr (syntax->list stx))
                  (map preprocess-nested-fry _)
                  flatten
                  (slice-by by-is-underscore _)
                  (map concatenate-if-not-underscores _)
                  flatten
                  (map protect-syntax _))])
    #`(compile-fry (list #,@args))))

(define (dip . inner)
  (let ([inner (apply string-append inner)])
    (program (enclose)
             (quoted inner)
             (swap)
             (cat)
             (eval))))

(define (2dip . inner)
  (dip (apply dip inner)))

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

(define (do-while . body)
  (let ([body (apply program body)])
    (program (quoted (dip body)
                     (swap) (quoted (discard)) (swap)
                     (eval) (dup) (eval))
             (dup) (eval))))

(define (while . body)
  (let ([body (apply program body)])
    (if-stmt (do-while body) (program))))

;; ( ? ? -- ? )
(define (and-v)
  (program (quoted false) (swap)
           (eval)))

;; ( ? ? -- ? )
(define (or-v)
  (program (dip (quoted true) (swap))
           (eval)))

;; ( ? -- )
(define (output-bool)
  (program (choose-by-bool "true\n" "false\n") (output)))

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

;; ( xy -- x )
(define (pair-first)
  (program (eval)
           (discard)))

;; ( xy -- y )
(define (pair-second)
  (program (eval)
           (swap)
           (discard)))

(define (mul) (cat))
(define (exp) (eval))

(define (add) (fry (dup) _ (swap) _ (cat)))

(define (succ) (fry (dup) _ (cat)))

;; Predecessor function ( n -- n' ): Returns max(n - 1, 0)
;;
;; See Wikipedia for the magic sauce
;; https://en.wikipedia.org/wiki/Church_encoding#Predecessor_function
(define (pred)
  (fry ;; Create the function that we'll apply (this is fried against
       ;; the string we're catenating)
       (fry (swap) (discard) (dup) <_> (cat))
       ;; Repeat operation
       _
       ;; Create accumulator strings and run
       (quoted) (swap) (quoted) (swap) (eval)
       ;; Drop the "top" value
       (discard)))

;; ( x y -- x x y )
(define (dupd)
  (dip (dup)))

;; ( x y -- x y x y )
(define (2dup)
  (program (dupd)
           (dup)
           (dip (swap))))

;; ( x y z -- x y z x y z )
(define (3dup)
  (program (dip (2dup))
           (dup)
           (dip (swap) (dip (swap)))))

;; ( x y -- x y x)
(define (over)
  (program (dupd)
           (swap)))

;; ( x y z -- y z x )
(define (rot-up)
  (program (dip (swap))
           (swap)))

;; ( x y z - z x y )
(define (rot-down)
  (program (swap)
           (dip (swap))))

;; ( x y -- x-y )
(define (monus)
  (program (quoted (pred))
           (swap)
           (eval)
           (eval)))

;; ( x -- ? )
(define (==0)
  (program (quoted true) (swap)
           (quoted (discard) (quoted false)) (swap)
           (eval) (eval)))

;; ( x -- ? )
(define (!=0)
  (program (quoted false) (swap)
           (quoted (discard) (quoted true)) (swap)
           (eval) (eval)))

;; ( x y -- ? )
(define (op>)
  (program (monus) (!=0)))

;; ( x y -- ? )
(define (op<=)
  (program (monus) (==0)))

;; ( x y -- ? )
(define (op>=)
  (program (swap) (op<=)))

;; ( x y -- ? )
(define (op<)
  (program (swap) (op>)))

;; ( x y -- ? )
(define (op==)
  (program (2dup)
           (op<=)
           (dip (op>=))
           (and-v)))

;; ( x y -- floor(x/y) x%y )
(define (divmod)
  (program
   ;; Stack is going to be ( x/y x%y y )
   (2dip (quoted *0))
   (2dup) (op>=)
   (while (dup) (dip (monus)) (2dip (succ)) (2dup) (op>=))
   (discard)))


;; ( x y -- x%y )
(define (mod)
  (program
   (2dup) (op>=)
   (while (dup) (dip (monus)) (2dup) (op>=))
   (discard)))

;; ( x y -- floor(x/y) )
(define (div)
  (program (divmod) (discard)))


;; ( x -- ... )
;;
;; The first N cases are for zero, then one, then two, etc. The
;; else-case, if provided, is for any other numeral.
;;
;; Example: If we get 3 arguments, we push `else-case id 2case 2drop
;; 1case 2drop 0case 2drop`. Then we run `dup eval` N times and exec
;; whatever happens to end up one down on the stack after that. Each
;; case has instructions to nuke everything under it.
(define (branch-on-small-numeral #:else [else-case (program)] . cases)
  (define (2drop) (program (discard) (discard)))

  (define (id) (program))

  (program (dip (quoted else-case)
                (quoted (id))
                (apply string-append (for/list ([case (reverse cases)]
                                                [pops-needed (in-range 2 +inf.0 2)])
                                       (program (quoted (string-append (apply string-append (make-list pops-needed (discard)))
                                                                       case))
                                                (quoted (2drop)))))
                (quoted (dup) (eval)))
           (eval) (eval) (discard) (eval)))

(define (output-digit)
  (program (branch-on-small-numeral (quoted "0") (quoted "1") (quoted "2") (quoted "3") (quoted "4")
                                    (quoted "5") (quoted "6") (quoted "7") (quoted "8") (quoted "9")
                                    #:else (quoted "#"))
           (output)))

;; Church lists...ish, we're using the Underload method:
;; https://esolangs.org/wiki/Underload#Lists_and_tuples
;;
;; (x)~^(y)~^(z)~^...

(define (push-list . elems)
  (define (list-elem x) (program (quoted x) (swap) (eval)))

  (apply quoted (flatten (map list-elem elems))))

;; ( x -- lst )
(define (singleton)
  (program (enclose) (quoted (swap) (eval)) (cat)))

;; ( x lst -- lst' )
(define (lcons)
  (program (swap) (singleton) (swap) (cat)))

;; ( lst x -- lst' )
(define (lsnoc)
  (program (singleton) (cat)))

;; ( lst -- ... )
;;
;; Inner function sees ( elt -- ... )
(define (foreach . body)
  (let ([body (apply program body)])
    (program
     ;; Set up a self-reproducing payload. Payload sees ( payload elt -- ... payload payload )
     (quoted (swap) (dip body) (dup))
     (swap) (dip (dup))
     ;; Stack is now ( payload payload lst )
     (eval)
     ;; Stack is now ( ... payload payload )
     (discard) (discard))))

;; ( lst -- lst )
(define (reverse-list)
  (program
   (quoted) (swap)
   (foreach (swap) (lcons))))

(define (output-list-of-digits)
  (program (foreach (output-digit)) (output "\n")))

;; ( lst -- ... )
;;
;; Inner function sees ( n elt -- ... )
(define (foreach-with-index . body)
  (let ([body (apply program body)])
    (program
     ;; Index counter
     (quoted *0) (swap)
     (foreach
      ;; Stack is ( index elt ) right now
      (over) (dip body) (succ))
     ;; Stack is now ( ... index )
     (discard))))

;; ( lst -- n )
(define (llength)
  (program (quoted *0) (swap)
           (foreach (discard) (succ))))

;; ( n lst -- elem )
(define (lindex #:default [default (program)])
  (program (2dip (quoted default))
           ;; Stack is ( default n lst )
           (foreach-with-index
            ;; ( default n i elem )
            (dip (over) (op==))
            ;; (default n is-equal elem )
            (swap) (if-stmt (program (2dip (discard)) (swap))
                            (program (discard))))
           ;; ( default n )
           (discard)))

;; ( n -- ... )
;;
;; Inner function sees ( i -- ... )
(define (for-range . body)
  (let ([body (apply program body)])
    (program
     (quoted *0) (swap) (2dup) (op<)
     ;; Stack: ( 0 n 0<n )
     (while ;; Stack: ( i n )
            (dip (dup) (dip body) (succ))
            (2dup) (op<))
     (discard) (discard))))

;; ( n m -- k )
(define (max-v)
  (program (2dup) (op>=) (if-stmt (discard) (program (swap) (discard)))))

;; ( lst1 lst2 -- ... )
;;
;; Inner function sees ( x y -- ... )
;;
;; Zips to longest, padding is used if out of bounds.
(define (foreach-zipped #:padding padding . body)
  (let ([body (apply program body)])
    (program
     (2dup) (llength) (swap) (llength) (max-v)
     ;; Stack is ( lst1 lst2 longlen )
     (for-range
      ;; Stack is ( lst1 lst2 i )
      (rot-down) (2dup)
      (2dip
       ;; Stack is ( i lst1 lst2 ) with ( lst1 lst2 ) stored in dipspace
       (fry (dup) <_> (lindex #:default padding) (swap) <_> (lindex #:default padding) (swap)) (eval)
       ;; Stack is ( x y ) with ( lst1 lst2 ) stored in dipspace
       body))
     ;; Stack is ( lst1 lst2 )
     (discard) (discard))))

(define (two-digit-num a b)
  (program (quoted a) (quoted b)))

;; ( x y -- tens ones )
(define (mul-from-table)
  (branch-on-small-numeral
   ;; 0
   (program (discard) (two-digit-num *0 *0))
   ;; 1
   (program (quoted *0) (swap))
   ;; 2
   (branch-on-small-numeral
    ;; 2 * 0
    (two-digit-num *0 *0)
    ;; 2 * 1
    (two-digit-num *0 *2)
    ;; 2 * 2
    (two-digit-num *0 *4)
    ;; 2 * 3
    (two-digit-num *0 *6)
    ;; 2 * 4
    (two-digit-num *0 *8)
    ;; 2 * 5
    (two-digit-num *1 *0)
    ;; 2 * 6
    (two-digit-num *1 *2)
    ;; 2 * 7
    (two-digit-num *1 *4)
    ;; 2 * 8
    (two-digit-num *1 *6)
    ;; 2 * 9
    (two-digit-num *1 *8))
   ;; 3
   (branch-on-small-numeral
    ;; 3 * 0
    (two-digit-num *0 *0)
    ;; 3 * 1
    (two-digit-num *0 *3)
    ;; 3 * 2
    (two-digit-num *0 *6)
    ;; 3 * 3
    (two-digit-num *0 *9)
    ;; 3 * 4
    (two-digit-num *1 *2)
    ;; 3 * 5
    (two-digit-num *1 *5)
    ;; 3 * 6
    (two-digit-num *1 *8)
    ;; 3 * 7
    (two-digit-num *2 *1)
    ;; 3 * 8
    (two-digit-num *2 *4)
    ;; 3 * 9
    (two-digit-num *2 *7))
   ;; 4
   (branch-on-small-numeral
    ;; 4 * 0
    (two-digit-num *0 *0)
    ;; 4 * 1
    (two-digit-num *0 *4)
    ;; 4 * 2
    (two-digit-num *0 *8)
    ;; 4 * 3
    (two-digit-num *1 *2)
    ;; 4 * 4
    (two-digit-num *1 *6)
    ;; 4 * 5
    (two-digit-num *2 *0)
    ;; 4 * 6
    (two-digit-num *2 *4)
    ;; 4 * 7
    (two-digit-num *2 *8)
    ;; 4 * 8
    (two-digit-num *3 *2)
    ;; 4 * 9
    (two-digit-num *3 *6))
   ;; 5
   (branch-on-small-numeral
    ;; 5 * 0
    (two-digit-num *0 *0)
    ;; 5 * 1
    (two-digit-num *0 *5)
    ;; 5 * 2
    (two-digit-num *1 *0)
    ;; 5 * 3
    (two-digit-num *1 *5)
    ;; 5 * 4
    (two-digit-num *2 *0)
    ;; 5 * 5
    (two-digit-num *2 *5)
    ;; 5 * 6
    (two-digit-num *3 *0)
    ;; 5 * 7
    (two-digit-num *3 *5)
    ;; 5 * 8
    (two-digit-num *4 *0)
    ;; 5 * 9
    (two-digit-num *4 *5))
   ;; 6
   (branch-on-small-numeral
    ;; 6 * 0
    (two-digit-num *0 *0)
    ;; 6 * 1
    (two-digit-num *0 *6)
    ;; 6 * 2
    (two-digit-num *1 *2)
    ;; 6 * 3
    (two-digit-num *1 *8)
    ;; 6 * 4
    (two-digit-num *2 *4)
    ;; 6 * 5
    (two-digit-num *3 *0)
    ;; 6 * 6
    (two-digit-num *3 *6)
    ;; 6 * 7
    (two-digit-num *4 *2)
    ;; 6 * 8
    (two-digit-num *4 *8)
    ;; 6 * 9
    (two-digit-num *5 *4))
   ;; 7
   (branch-on-small-numeral
    ;; 7 * 0
    (two-digit-num *0 *0)
    ;; 7 * 1
    (two-digit-num *0 *7)
    ;; 7 * 2
    (two-digit-num *1 *4)
    ;; 7 * 3
    (two-digit-num *2 *1)
    ;; 7 * 4
    (two-digit-num *2 *8)
    ;; 7 * 5
    (two-digit-num *3 *5)
    ;; 7 * 6
    (two-digit-num *4 *2)
    ;; 7 * 7
    (two-digit-num *4 *9)
    ;; 7 * 8
    (two-digit-num *5 *6)
    ;; 7 * 9
    (two-digit-num *6 *3))
   ;; 8
   (branch-on-small-numeral
    ;; 8 * 0
    (two-digit-num *0 *0)
    ;; 8 * 1
    (two-digit-num *0 *8)
    ;; 8 * 2
    (two-digit-num *1 *6)
    ;; 8 * 3
    (two-digit-num *2 *4)
    ;; 8 * 4
    (two-digit-num *3 *2)
    ;; 8 * 5
    (two-digit-num *4 *0)
    ;; 8 * 6
    (two-digit-num *4 *8)
    ;; 8 * 7
    (two-digit-num *5 *6)
    ;; 8 * 8
    (two-digit-num *6 *4)
    ;; 8 * 9
    (two-digit-num *7 *2))
   ;; 9
   (branch-on-small-numeral
    ;; 9 * 0
    (two-digit-num *0 *0)
    ;; 9 * 1
    (two-digit-num *0 *9)
    ;; 9 * 2
    (two-digit-num *1 *8)
    ;; 9 * 3
    (two-digit-num *2 *7)
    ;; 9 * 4
    (two-digit-num *3 *6)
    ;; 9 * 5
    (two-digit-num *4 *5)
    ;; 9 * 6
    (two-digit-num *5 *4)
    ;; 9 * 7
    (two-digit-num *6 *3)
    ;; 9 * 8
    (two-digit-num *7 *2)
    ;; 9 * 9
    (two-digit-num *8 *1))))



;; ( a b -- a+b )
;;
;; Adds two numbers, represented as lists of base 10 Church-encoded
;; numerals.
(define (add-numerals)
  (program (reverse-list) (swap) (reverse-list)
           (2dip
            (quoted *0) ;; Carry bit
            (push-list)) ;; Sum digits
           ;; Stack is ( carry result b a )
           (foreach-zipped #:padding *0
             ;; Stack is ( carry result bdigit adigit )
             (add)
             (dip (swap))
             (add)
             ;; Stack is ( result carry+bd+ad )
             (quoted *10) (divmod) (dip (swap))
             ;; Stack is ( carry result nextdigit )
             (swap) (lcons))
           ;; Stack is ( carry result )
           (swap) (dup) (==0) (if-stmt (discard) (program (swap) (lcons)))))

;; ( a-list b-digit -- a*b-list )
;;
;; Multiplies a list-of-numerals number by a single digit.
(define (mul-by-digit)
  (program (swap) (2dip (push-list) (quoted *0)) (reverse-list)
           ;; Stack is ( result carry b-digit a-list )
           (foreach
            ;; Stack is ( result carry b-digit a-digit )
            (over)
            (dip (mul-from-table) (dip (swap)) (add) (quoted *10) (divmod) (dip (add))
                 ;; Stack is ( result carry next-digit ), dipspace is ( b-digit )
                 (rot-down) (dip (lcons))))
           ;; Stack is ( result carry b-digit )
           (discard)
           (dup) (==0) (if-stmt (discard) (program (swap) (lcons)))))

;; ( a-list b-list -- a*b-list )
;;
;; Multiplies two lists of numerals.
(define (mul-numerals)
  (program (2dip (quoted *0))
           ;; Stack is ( accum a-list b-list )
           (foreach
            ;; Stack is ( accum a-list b-digit )
            (over)
            (dip ;; Stack is ( accum a-list b-digit ), dipspace is ( a-list )
                 (mul-by-digit)
                 (swap) (quoted *0) (lsnoc) (add-numerals)))
           ;; Stack is ( accum a-list )
           (discard)))


(define practice
  (program (output "*****\n") ; *****
           (quoted *3) (quoted *4) (mul-from-table) (output-digit) (output-digit) (output "\n")
           (push-list *8 *1 *2 *3 *6)
           (push-list *2 *5 *5 *4 *4)
           (add-numerals)
           (output-list-of-digits) ; 106780

           (push-list *1 *2 *5) (quoted *2)
           (mul-by-digit)
           (output-list-of-digits) ; 250

           (push-list *5 *2 *5) (quoted *5)
           (mul-by-digit)
           (output-list-of-digits) ; 2625

           (push-list *3 *7 *9) (quoted *8)
           (mul-by-digit)
           (output-list-of-digits) ; 3032

           (push-list *3 *7 *9) (push-list *1 *8 *9 *2)
           (mul-numerals)
           (output-list-of-digits) ; 717068

           (quoted "a\n") (quoted "b") (quoted "c") (quoted "d") (quoted "e") (quoted (discard)) (fry (quoted (swap) _) (dup) (cat) (eval)) (eval)
           (output) (output) (output) ; eba
           (quoted "1\n") (quoted "2\n") (quoted (discard)) (fry (swap) _) (eval) (output) ; 2
           (quoted "\n") (quoted "7") (quoted (discard)) (fry (swap) <_>) (eval) (swap) (cat) (cat) (output) ; 7!
           (quoted "first\n") (quoted "second\n")
           (pair)
           (eval) (output) (output) ; second first
           (quoted true)
           (if-stmt (quoted "true\n") (quoted "false\n")) ; true
           (output)
           (quoted false)
           (if-stmt (quoted "true\n") (quoted "false\n")) ; false
           (output)

           (quoted *4)
           (do-while (output "loop") (pred) (dup) (!=0))
           (output "\n") ; looplooplooploop

           (push-list *7 *7 *7 *7 *7)
           (llength) (output-digit) (output "\n") ; 5

           (quoted *4)
           (quoted true)
           (while (output "w") (pred) (dup) (!=0))
           (output "\n") ; wwww

           (push-list *5 *6 *7 *8)
           (foreach-with-index (swap) (output-digit) (output-digit)) (output "\n") ; 05162738

           (quoted "XXXX\n") (output) ; XXXX
           (quoted *2)
           (push-list *5 *6 *7 *8)
           (lindex #:default *9) (output-digit) (output "\n") ; 7

           (quoted *4)
           (push-list *5 *6 *7 *8)
           (lindex #:default *9) (output-digit) (output "\n") ; 9

           (quoted *3)
           (quoted *2)
           (mul)
           (output-digit)
           (output "\n") ; 6

           (quoted "----\n") (output) ; ----
           (push-list *1 *2 *3)
           (push-list *4 *5 *6 *7 *8)
           (foreach-zipped (output-digit) (output-digit) #:padding *0) ; 4152637080
           (output "\n")

           (comment "LIST START\n")
           (push-list *1 *2 *3 *4)
           (foreach (output-digit) (output "\n")) ; 1 2 3 4

           (quoted *5) (succ) (output-digit) (output "\n") ; 6

           (quoted *7) (quoted *7) (mul)
           (quoted *10) (mod)
           (output-digit) (output "\n") ; 9

           (quoted *7) (quoted *7) (mul)
           (quoted *10) (div)
           (output-digit) (output "\n") ; 4

           (quoted *2)
           (quoted *3)
           (add)
           (quoted "hello!\n")
           (swap)
           (eval)
           (output) ; hello 5 times
           (quoted "hi\n")
           (quoted *7)
           (quoted *4)
           (monus)
           (eval)
           (output) ; hi 3 times

           (push-list *3 *4 *5 *6)
           (output-list-of-digits) ; 3456

           (push-list *3 *4 *5 *6)
           (reverse-list)
           (output-list-of-digits) ; 6543

           (quoted true) (quoted false) (and-v) (output-bool) ; false
           (quoted false) (quoted true) (and-v) (output-bool) ; false
           (quoted true) (quoted true) (and-v) (output-bool) ; true
           (quoted false) (quoted false) (and-v) (output-bool) ; false
           (quoted true) (quoted false) (or-v) (output-bool) ; true
           (quoted false) (quoted true) (or-v) (output-bool) ; true
           (quoted true) (quoted true) (or-v) (output-bool) ; true
           (quoted false) (quoted false) (or-v) (output-bool) ; false
           (output "--\n") ;; --
           (quoted *3) (quoted *3) (op==) (output-bool) ; true
           (quoted *3) (quoted *4) (op==) (output-bool) ; false
           (quoted *4) (quoted *3) (op==) (output-bool) ; false
           (quoted *3) (==0) (if-stmt (quoted "3==0\n") (quoted "3!=0\n")) (output) ; 3!=0
           (quoted *3) (!=0) (if-stmt (quoted "3!=0\n") (quoted "3==0\n")) (output))) ; 3!=0

(displayln practice)
;(displayln (pred))
;(displayln (count-fry-args #'(_ a b (d _ e <_>) c _)))

;(println (program (fry "A" "B" _ "C" "D" <_> "E" "F")))

(displayln (mul-numerals))
