
#lang racket

;; Okay, let me explain. Problem 169 involved the same function f, and
;; we discovered that f(n) = fusc(n + 1), where fusc is Stern's
;; diatomic sequence
;; (https://en.wikipedia.org/wiki/Calkin%E2%80%93Wilf_tree#Stern's_diatomic_sequence).
;; This comes from the Calkin-Wilf Tree, which enumerates all rational
;; numbers uniquely.
;;
;; Specifically, fusc(n) / fusc(n+1) is the nth number in the
;; Calkin-Wilf tree (taken in breadth-first order), and this is the
;; only position in the tree where that rational number appears.
;;
;; Now the Project Euler problem wants us to figure out where f(n) /
;; f(n-1) is in the tree, i.e. fusc(n+1) / fusc(n). So we want to
;; reciprocate our input (hence why numerator is 987654321 and
;; denominator is 123456789) to get what they're asking for. So in
;; essence, they want us to find n such that the nth term of the
;; Calkin-Wilf tree is 987654321/123456789.
;;
;; Now we get to the really cool part. We don't actually need to
;; calculate n. Because they want the run-length encoding in binary.
;; The nth term of the Calkin-Wilf tree can be written in continued
;; fraction for [a_0; a_1, a_2, ..., a_k], where (a_k a_(k-1) a_(k-2)
;; ... a_0) is the run-length encoding in binary of n.
;;
;; We already have the fraction from the Calkin-Wilf tree and we want
;; the run-length encoding, so we can cut out the middle man. Instead
;; of calculating the run-length encoding of n, we can just get the
;; continued fraction representation of our fraction.
;;
;; There is one minor note. The run-length encoding does require that
;; the value be of odd length. So if we get an even-length continued
;; fraction, we need to find an equivalent one of odd length. In
;; general, if q > 1, then [a_0; a_1, a_2, ..., a_k, q] and [a_0; a_1,
;; a_2, ..., a_k, q-1, 1] are equivalent representations, so we can
;; use this trick to adjust the length.

(require threading)

(define (continued-fraction numerator denominator)
  (if (or (= numerator 0) (= denominator 0))
      null
      (let* ([whole-part (quotient numerator denominator)]
             [new-numerator (- numerator (* whole-part denominator))])
        (cons whole-part (continued-fraction denominator new-numerator)))))

(define (normalize-to-odd lst)
  (cond
    [(odd? (length lst)) lst]
    [(= (car lst) 1) (cons (+ (car lst) (cadr lst)) (cddr lst))]
    [else (cons 1 (cons (- (car lst) 1) (cdr lst)))]))

;; (define numerator 17)
;; (define denominator 13)

(define numerator 987654321)
(define denominator 123456789)

(displayln (~>
            (continued-fraction numerator denominator)
            reverse
            normalize-to-odd
            (map number->string _)
            (string-join ",")))
