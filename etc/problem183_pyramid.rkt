
#lang racket

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
    (set n 5)
    (loop (= (<=> n 101) -1)
      (begin
        (out n " ")
        (set n (+ n 1))))))

(require 'problem183)
