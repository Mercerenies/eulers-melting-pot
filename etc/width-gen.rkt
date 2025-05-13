
;; Generator script for the Width programming language.
;; (https://github.com/stestoltz/Width/tree/master)
;;
;; Attempting to solve Project Euler Problem 202.

#lang racket

(define (random-from-vec vec)
  (let ([n (vector-length vec)])
    (vector-ref vec (random n))))

(define (digits n)
  (digits-impl n null))

(define (digits-impl n acc)
  (if (= n 0)
      acc
      (digits-impl (floor (/ n 10)) (cons (modulo n 10) acc))))

(define chars
  #hash((0 . #(#\i #\j #\l))
        (1 . #(#\f #\r #\t #\I))
        (2 . #(#\c #\k #\s #\v #\x #\y #\z #\J))
        (3 . #(#\a #\b #\d #\e #\g #\h #\n #\o #\p #\q #\u #\L))
        (4 . #(#\F #\T)) ; Note: I arbitrarily reserve #\Z for integer literals.
        (5 . #(#\A #\B #\E #\K #\P #\S #\V #\X #\Y))
        (6 . #(#\w #\C #\D #\H #\N #\R #\U))
        (7 . #(#\G #\O #\Q))
        (8 . #(#\m #\M))
        (9 . #(#\W))))

(define (ch d)
  (random-from-vec (hash-ref chars d)))

(define (prim-literal-char ch)
  `(prim-literal-char ,ch))

(define (cmd-regular . digits)
  ;; Regular command
  `(cmd-regular ,@digits))

(define (to-extended-digit n)
  (case n
    [(0) 2]
    [(1) 5]
    [(2) 6]))

(define (cmd-extended . digits)
  `(cmd-extended ,@digits))

(define (block . cmds)
  `(block ,@cmds))

(define-syntax for/block
  (syntax-rules ()
    [(_ clauses . body) (apply block (for/list clauses . body))]))

(define-syntax for*/block
  (syntax-rules ()
    [(_ clauses . body) (apply block (for*/list clauses . body))]))

(define (compile-single term #:was-extended was-extended)
  (case (first term)
    [(prim-literal-char) (values (string (second term)) #f)]
    [(cmd-regular) (values (apply string (map ch (rest term))) #f)]
    [(cmd-extended) (let ([ext (apply string (map (compose ch to-extended-digit) (rest term)))])
                      (if was-extended
                          ;; Need to insert a no-op in the middle of the juxtaposition
                          (values (string-append (string (ch 3)) ext) #t)
                          (values ext #t)))]
    [(block) (compile-block (rest term) #:was-extended was-extended)]))

(define (compile-block lst #:was-extended was-extended)
  (if (null? lst)
      (values "" was-extended)
      (let*-values ([(fst was-extended-1) (compile-single (first lst) #:was-extended was-extended)]
                    [(snd was-extended-2) (compile-block (rest lst) #:was-extended was-extended-1)])
        (values (string-append fst snd) was-extended-2))))

(define (compile-code term)
  (let-values ([(code _) (compile-single term #:was-extended #f)])
    code))

;;; Primitives

(define prim-begin-while (cmd-regular 0))

(define prim-begin-if (cmd-regular 8))

(define prim-else (cmd-regular 9))

(define prim-end-block (cmd-regular 1))

(define nop (cmd-regular 3))

(define (push-literal literal)
  (if (= literal 0)
      (block (prim-literal-char #\Z)
             (cmd-regular 0)
             (prim-literal-char #\Z))
      (block (prim-literal-char #\Z)
             (apply block (map cmd-regular (digits literal)))
             (prim-literal-char #\Z))))

;; If top-of-stack is true
(define (if-tos true-case [false-case null])
  (block prim-begin-if
         true-case
         (apply block (if false-case (list prim-else false-case) null))
         prim-end-block))

;; While counter is true
(define (while-counter &rest body)
  (block prim-begin-while
         (apply block body)
         prim-end-block))

(define cmd-dup (cmd-extended 0))
(define cmd-swap (cmd-extended 1))
(define cmd-pop (cmd-extended 2))

(define cmd-set-counter-to-tos (cmd-extended 0 0))
(define cmd-push-stack-height (cmd-extended 0 1))
(define cmd-input-string (cmd-extended 0 2))
(define cmd-input-number (cmd-extended 1 0))
(define cmd-to-string (cmd-extended 1 1))
(define cmd-to-int (cmd-extended 1 2))
(define cmd-inc-counter (cmd-extended 2 0))
(define cmd-dec-counter (cmd-extended 2 1))
(define cmd-print (cmd-extended 2 2))

(define cmd-add (cmd-extended 0 1 0))
(define cmd-sub (cmd-extended 0 1 1))
(define cmd-int-div (cmd-extended 0 2 0))
(define cmd-mul (cmd-extended 0 2 1))

(define factors #(5 11 17 23 29 41 47))

(define limit 6008819575)

;; Stack effect: ( -- n )
(define (do-multiples-up-to k #:offset offset)
  (block (push-literal limit)
         (push-literal k)
         cmd-int-div
         (push-literal offset)
         cmd-add
         (push-literal 3)
         cmd-int-div))

(define project-euler-202
  (block (push-literal 0)
         ;; Add all candidates
         (do-multiples-up-to 1 #:offset 1)
         cmd-add
         ;; Subtract singles
         (for*/block ([a (in-range 0 7)])
           (block (do-multiples-up-to (vector-ref factors a)
                                      #:offset 2)
                  cmd-sub))
         ;; Add doubles
         (for*/block ([a (in-range 0 7)]
                      [b (in-range (+ a 1) 7)])
           (block (do-multiples-up-to (* (vector-ref factors a)
                                         (vector-ref factors b))
                                      #:offset 1)
                  cmd-add))
         ;; Subtract triples
         (for*/block ([a (in-range 0 7)]
                      [b (in-range (+ a 1) 7)]
                      [c (in-range (+ b 1) 7)])
           (block (do-multiples-up-to (* (vector-ref factors a)
                                         (vector-ref factors b)
                                         (vector-ref factors c))
                                      #:offset 2)
                  cmd-sub))
         ;; Add 4-tuples
         (for*/block ([a (in-range 0 7)]
                      [b (in-range (+ a 1) 7)]
                      [c (in-range (+ b 1) 7)]
                      [d (in-range (+ c 1) 7)])
           (block (do-multiples-up-to (* (vector-ref factors a)
                                         (vector-ref factors b)
                                         (vector-ref factors c)
                                         (vector-ref factors d))
                                      #:offset 1)
                  cmd-add))
         ;; Subtract 5-tuples
         (for*/block ([a (in-range 0 7)]
                      [b (in-range (+ a 1) 7)]
                      [c (in-range (+ b 1) 7)]
                      [d (in-range (+ c 1) 7)]
                      [e (in-range (+ d 1) 7)])
           (block (do-multiples-up-to (* (vector-ref factors a)
                                         (vector-ref factors b)
                                         (vector-ref factors c)
                                         (vector-ref factors d)
                                         (vector-ref factors e))
                                      #:offset 2)
                  cmd-sub))
         ;; Add 6-tuples
         (for*/block ([a (in-range 0 7)]
                      [b (in-range (+ a 1) 7)]
                      [c (in-range (+ b 1) 7)]
                      [d (in-range (+ c 1) 7)]
                      [e (in-range (+ d 1) 7)]
                      [f (in-range (+ e 1) 7)])
           (block (do-multiples-up-to (* (vector-ref factors a)
                                         (vector-ref factors b)
                                         (vector-ref factors c)
                                         (vector-ref factors d)
                                         (vector-ref factors e)
                                         (vector-ref factors f))
                                      #:offset 1)
                  cmd-add))
         ;; Subtract 7-tuple
         (do-multiples-up-to 1201763915 #:offset 2)
         cmd-sub
         ;; Print result
         cmd-print))

;; Consistent output
(random-seed 851640648) ; Just a random number

;; (println project-euler-202)
(displayln (compile-code project-euler-202))
