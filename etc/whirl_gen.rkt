
#lang racket

;; Generator script for Whirl programs.

(define ring-size 12)

(struct command (ring-name index))

(define noop 0)

(define op/noop (command 'operations 0))
(define op/exit (command 'operations 1))
(define op/set-to-one (command 'operations 2))
(define op/set-to-zero (command 'operations 3))
(define op/load-memory (command 'operations 4))
(define op/store-memory (command 'operations 5))
(define op/add-program (command 'operations 6))
(define op/add-memory (command 'operations 7))
(define op/logical-and (command 'operations 8))
(define op/add-program-if (command 'operations 9))
(define op/integer-io (command 'operations 10))
(define op/ascii-io (command 'operations 11))

(define math/noop (command 'math 0))
(define math/load-memory (command 'math 1))
(define math/store-memory (command 'math 2))
(define math/+ (command 'math 3))
(define math/* (command 'math 4))
(define math/div (command 'math 5))
(define math/set-to-zero (command 'math 6))
(define math/< (command 'math 7))
(define math/= (command 'math 8))
(define math/> (command 'math 9))
(define math/not (command 'math 10))
(define math/negate (command 'math 11))

(define (string-concat lst)
  (apply string-append lst))

(define state%
  (class object%
    (field [active-ring 'operations]
           [operations-ring (new ring% [name 'operations] [size ring-size])]
           [math-ring (new ring% [name 'math] [size ring-size])])
    (define/public (get-active-ring)
      (case active-ring
        [(operations) operations-ring]
        [(math) math-ring]
        [else (error "Bad ring type: ~a" active-ring)]))
    (define/public (swap-ring)
      (let ([old-active-ring (get-active-ring)])
        (set! active-ring (if (= active-ring 'operations) 'math 'operations))
        (string-append
         (send old-active-ring rotate-to noop)
         "0")))
    (define/public (move-to command)
      (string-append
       (if (= active-ring (command-ring-name command)) "" (swap-ring))
       (send (get-active-ring) rotate-to (command-index command))
       (begin (send (get-active-ring) reverse-direction) "0")))
    (define/public (execute command)
      (string-append (move-to command) "0"))))

(define ring%
  (class object%
    (super-new)
    (init-field name)
    (field [position 0])
    (init-field size)
    (field [direction 1])
    (define/public (get-name)
      name)
    (define/public (reverse-direction)
      (set! direction (- direction)))
    (define/public (rotate-to new-position)
      ;; Assumes that the previous instruction was either (1) the
      ;; beginning of the program, (2) a "1", or (3) an executing "0".
      ;;
      ;; The output of this function will always end in a 1, so
      ;; callers can safely assume that we just executed a 1 when this
      ;; finishes running.
      (let ([forward-distance (modulo (* direction (- new-position position)) size)]
            [backward-distance (modulo (* direction (- position new-position)) size)])
        (cond
          [(= new-position position)
           (reverse-direction)
           "101"] ;; Make sure we always output something nonempty, just to get to a consistent state.
          [(<= forward-distance backward-distance)
           (set! position new-position)
           (make-string forward-distance #\1)]
          [#t
           (reverse-direction)
           (set! position new-position)
           (format "0~a" (make-string backward-distance #\1))])))))

(let ([ring (new ring% [name 'test] [size 12])])
  (println (send ring rotate-to 0))
  (println (send ring rotate-to 3))
  (println (send ring rotate-to 2)))

