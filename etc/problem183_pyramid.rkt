
#lang racket

;; Model language for Pyramid Scheme. The module pyramid-scheme is
;; identical in semantics to the language Pyramid Scheme (except a few
;; operators I didn't need and didn't bother to implement), except
;; that it uses Lisp notation. This is just for testing.

(module pyramid-scheme racket
  (provide #%module-begin #%app #%datum (rename-out [fromhash #%top])
           out chr + - * / ^ = <=> ! left right set do loop ? begin)

  (require racket/stxparam)
  (require (rename-in racket/base [+ racket+] [- racket-]
                                  [* racket*] [/ racket/]
                                  [= racket=]))

  ;; Redefine a variadic function to require exactly two arguments.
  (define (demand-arity-2 f)
    (lambda (x y) (f x y)))

  (define + (demand-arity-2 racket+))
  (define - (demand-arity-2 racket-))
  (define * (demand-arity-2 racket*))
  (define / (demand-arity-2 racket/))

  (define (^ a b) (expt a b))

  (define (= a b)
    (if (racket= a b) 1 0))

  (define (<=> a b)
    (cond
      [(racket= a b) 0]
      [(< a b) -1]
      [else 1]))

  (define (out a [b #f])
    (display a)
    (when b
      (display b)))

  (define (chr a)
    (string (integer->char a)))

  (define (! a)
    (if (racket= a 0) 1 0))

  (define (left a b)
    a)

  (define (right a b)
    b)

  (define-syntax-parameter variable-names-hash
    (lambda (stx) (raise-syntax-error #f "use of setter of getter outside of module block" stx)))

  (define-syntax-rule (fromhash . id)
    (hash-ref variable-names-hash (quote id)))

  (define-syntax-rule (#%module-begin . body)
    (#%plain-module-begin
     (let ([hash (make-hash)])
       (syntax-parameterize ([variable-names-hash (syntax-id-rules () [_ hash])]) . body))))

  (define-syntax-rule (set name value)
    (hash-set! variable-names-hash (quote name) value))

  (define-syntax-rule (do a b)
    (begin
      b
      (loop a b)))

  (define-syntax-rule (loop a b)
    (let cc ()
      (when (not (racket= a 0))
        b
        (cc))))

  (define-syntax-rule (? a b)
    (if (not (racket= a 0))
        b
        0)))

(module problem183 (submod ".." pyramid-scheme)
  (begin
    (set sum 0)
    (set x 1)
    (set n 5)
    (loop (= (<=> n 10001) -1)
      (begin
        (loop (= (<=> (* 2.718281828 (+ x 1)) n) -1)
              (set x (+ x 1)))
        (set y (+ x 1))
        (set recip (/ 1 x))
        (set k y)
        (? (= (<=> (/ (* y (^ y recip)) (^ n recip)) x) 1)
            (set k x))
        (set a n)
        (set b k)
        (loop (! (= b 0))
              (begin
                (set tmp a)
                (set a b)
                (loop (! (= (<=> tmp b) -1))
                      (set tmp (- tmp b)))
                (set b tmp)))
        (set denom (/ k a))

        ;; Remove powers of 2
        (loop (begin (set twomod denom)
                     (loop (= (<=> twomod 19) 1)
                           (set twomod (- twomod 20)))
                     (loop (= (<=> twomod 1) 1)
                           (set twomod (- twomod 2)))
                     (= twomod 0))
              (set denom (/ denom 2)))

        ;; Remove powers of 5
        (loop (begin (set fivemod denom)
                     (loop (= (<=> fivemod 49) 1)
                           (set fivemod (- fivemod 50)))
                     (loop (= (<=> fivemod 4) 1)
                           (set fivemod (- fivemod 5)))
                     (= fivemod 0))
              (set denom (/ denom 5)))

        (? (= denom 1)
           (set sum (- sum n)))

        (? (! (= denom 1))
           (set sum (+ sum n)))

        (set n (+ n 1))))
    (out sum (chr 10))))

(require 'problem183)
