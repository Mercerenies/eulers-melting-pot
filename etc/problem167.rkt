
#lang racket

;; Heuristic-based approach, failed because apparently some of the
;; periods are REEAAAAALLY long.

(require racket/generator)

(define (next-term previous-term sums)
  (for/first ([i (in-naturals (add1 previous-term))]
              #:when (= (hash-ref sums i 0) 1))
    i))

(define (ulam a b)
  (generator ()
    (yield a)
    (yield b)
    (let* ([sums (make-hash (list (cons (+ a b) 1)))]
           [terms-so-far (list b a)]
           [term (next-term b sums)])
      (let loop ()
        (yield term)
        (for ([c terms-so-far])
          (hash-update! sums (+ term c) add1 0))
        (set! terms-so-far (cons term terms-so-far))
        (set! term (next-term term sums))
        (loop)))))

(define (generator->list generator #:length max-length)
  (for/list ([i max-length])
    (generator)))

(define (difference-list input-list)
  (for/list ([x input-list]
             [y (cdr input-list)])
    (- y x)))

;; Heuristic approach to try and identify the period and fundamental
;; difference of the given Ulam sequence.
(define (identify-properties input-list)
  (let/ec return
    (println input-list)
    (for ([period (range 1 (- (length input-list) 1))])
      (let ([difference (- (list-ref input-list period) (list-ref input-list 0))])
        (when (for/and ([k (range (- (length input-list) period))])
                (= (- (list-ref input-list (+ k period)) (list-ref input-list k)) difference))
          (return period difference))))
    (values #f #f)))

(define brute-force-length 2000)

(define cutoff-size 400)

(define (identify-properties-ulam a b)
  (let ([seq (generator->list (ulam a b) #:length brute-force-length)])
    (identify-properties (drop seq cutoff-size))))

(define (get-ulam-constant a b index)
  (let-values ([(period difference) (identify-properties-ulam a b)])
    (println period)
    (println difference)
    (let* ([seq (drop (generator->list (ulam a b) #:length brute-force-length) cutoff-size)]
           [index (- index cutoff-size)]
           [index-modulo (modulo index period)]
           [known-term (list-ref seq index-modulo)]
           [steps (quotient (- index index-modulo) period)])
      (+ known-term (* steps difference)))))

;; (let ([seq (generator->list (ulam 2 5) #:length 300)])
;;   (println seq)
;;   (let-values ([(period difference) (identify-properties (drop seq 150))])
;;     (println period)
;;     (println difference)))

;; (let-values ([(period difference) (identify-properties-ulam 2 5)])
;;   (println period)
;;   (println difference))

;; We're zero-based, problem description is one-based, so adjust by 1.
(define k (- 1000000000000 1))

(println (for/sum ([n (range 2 11)])
           (println n)
           (get-ulam-constant 2 (+ (* 2 n) 1) k)))
