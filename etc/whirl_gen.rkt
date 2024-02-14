
#lang racket

(require multimethod)
(require threading)

;; Generator script for Whirl programs.

;; Conventions to make this actually remotely sane.
;;
;; (1) Assume all wheels are facing clockwise at all times. Anytime I
;; need to execute something, I'll double-rotate so that we're
;; clockwise after execution.
;;
;; (2) Whenever we do a jump, the math wheel should be on NOOP, and
;; the op wheel should be on IF. Anytime we end up at a jump target
;; (even if we just came from before that position), this should be
;; the case. That means that trivial jumps of length zero are fair
;; game.
;;
;; (3) As a corollary to the above, the PADD instruction is outright
;; banned. There is no safe way to do an unconditional jump and end up
;; in a consistent state. If I need to unconditionally jump, I'll do a
;; conditional jump with a constant condition.
;;
;; (4) Data is stored at the even positions (0-indexed) in the memory
;; buffer. ALL odd positions in the memory buffer are assumed to be
;; garbage temporaries. This is so that I can generate constant
;; numbers easily and move around.
;;
;; (5) Variables are stored at the low magnitude even positions. Our
;; big array is stored at the trailing right end of the memory buffer.
;; Again, all useful values are stored at even positions, so to move
;; to an array position we'll need to multiply the desired index by 2.
;;
;; (6) The largest variable that is NOT part of the trailing array is
;; the constant -1, which we'll use to move left in memory.

(define ring-size 12)

(struct command (ring-name index))

(struct var (name index))

(struct whirl-code (text comment))

(struct whirl-code-list (body))

(define (code text [comment #f])
  (whirl-code text comment))

(define (progn . body)
  (whirl-code-list body))

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

;; Variable indices
(define var/i (var 'i 0))
(define var/j (var 'j 2))
(define var/p (var 'p 4))
(define var/valuation (var 'valuation 6))
(define var/-1 (var 'minus-one 8)) ; Must be the largest non-array variable

(define (string-concat lst)
  (apply string-append lst))

(define state%
  (class object%
    (super-new)
    (field [active-ring 'operations]
           [operations-ring (new ring% [name 'operations] [size ring-size])]
           [math-ring (new ring% [name 'math] [size ring-size])]
           [memory-position 0]) ;; Should ALWAYS be even. Movements to odd positions are implementation details.
    (define/public (get-active-ring)
      (case active-ring
        [(operations) operations-ring]
        [(math) math-ring]
        [else (error "Bad ring type: ~a" active-ring)]))
    (define/public (get-inactive-ring)
      (case active-ring
        [(operations) math-ring]
        [(math) operations-ring]
        [else (error "Bad ring type: ~a" active-ring)]))
    (define/public (swap-ring)
      (set! active-ring (if (eq? active-ring 'operations) 'math 'operations)))
    (define/public (swap-ring-with-command)
      (let ([old-active-ring (get-active-ring)])
        (swap-ring)
        (string-append
         (send old-active-ring rotate-to noop)
         "00")))
    (define/public (spin-to command)
      (string-append
       (if (eq? active-ring (command-ring-name command)) "" (swap-ring-with-command))
       (send (get-active-ring) rotate-to (command-index command))))
    (define/public (execute command)
      (begin0
          (string-append (spin-to command) "00")
        (swap-ring)))
    (define/public (move-memory target-position)
      (begin0
          (cond
            [(= target-position memory-position)
             ""]
            [(> target-position memory-position)
             (apply string-append
                    (execute op/set-to-one)
                    (for/list ([i (range memory-position target-position)])
                      (execute op/add-memory)))]
            [(< target-position memory-position)
             (let* ([<-1>-var-index (var-index var/-1)]
                    [distance-to-destination (- <-1>-var-index target-position)])
               (apply string-append
                      (move-memory <-1>-var-index)
                      (execute op/load-memory)
                      (for/list ([i (range distance-to-destination)])
                        (execute op/add-memory))))])
        (set! memory-position target-position)))
    (define/public (normalize-rings)
      ;; Resets both rings to the default state (both at noop facing
      ;; clockwise and the pointer on 'operations).
      (begin0
          (string-append
           (send (get-active-ring) rotate-to noop)
           "00"
           (send (get-inactive-ring) rotate-to noop)
           (if (eq? active-ring 'operations) "00" ""))
        (set! active-ring 'operations)))))

(define ring%
  ;; For simplicity, we always assume our rings are facing clockwise.
  ;; We will execute instructions with "00", which quickly flips the
  ;; ring, then flips it *back* and executes.
  (class object%
    (super-new)
    (init-field name)
    (field [position 0])
    (init-field size)
    (define/public (get-name)
      name)
    (define/public (rotate-to new-position)
      (let ([distance (modulo (- new-position position) size)])
        (set! position new-position)
        (make-string distance #\1)))))

(define-generic (print-code obj))

(define-instance ((print-code whirl-code) obj)
  (if (whirl-code-comment obj)
      (printf "~a // ~a\n" (whirl-code-text obj) (whirl-code-comment obj))
      (printf "~a\n" (whirl-code-text obj))))

(define-instance ((print-code whirl-code-list) obj)
  (for ([item (whirl-code-list-body obj)])
    (print-code item)))

;; Note: code-length only considers non-comment characters, since
;; that's what we care about for the purposes of branching.
(define-generic (code-length obj))

(define-instance ((code-length whirl-code) obj)
  (string-length (whirl-code-text obj)))

(define-instance ((code-length whirl-code-list) obj)
  (~>> obj whirl-code-list-body (map code-length) (apply +)))

(define interpreter-state (make-parameter #f))

(define (exec-str command)
  ;; Low-level exec command that just moves the wheel(s) and executes.
  ;; Returns a string.
  (send (interpreter-state) execute command))

(define (exec command)
  ;; Low-level exec command that just moves the wheel(s) and executes.
  ;; Returns a code object.
  (code (exec-str command)))

(define (print-number)
  ;; Print the value at the current memory position, as an integer.
  (code
   (string-append
    (exec-str op/set-to-one)
    (exec-str op/integer-io))
   "Print"))

(define (number->word n)
  ;; Convert a number into words.
  (cond
    [(= n 0) "zero"]
    [(= n 1) "one"]
    [(= n 2) "two"]
    [(= n 3) "three"]
    [(= n 4) "four"]
    [(= n 5) "five"]
    [(= n 6) "six"]
    [(= n 7) "seven"]
    [(= n 8) "eight"]
    [(= n 9) "nine"]
    [(= n 10) "ten"]
    [(= n 11) "eleven"]
    [(= n 12) "twelve"]
    [(= n 13) "thirteen"]
    [(= n 14) "fourteen"]
    [(= n 15) "fifteen"]
    [(= n 16) "sixteen"]
    [(= n 17) "seventeen"]
    [(= n 18) "eighteen"]
    [(= n 19) "nineteen"]
    [(= n 20) "twenty"]
    [(= n 30) "thirty"]
    [(= n 40) "forty"]
    [(= n 50) "fifty"]
    [(= n 60) "sixty"]
    [(= n 70) "seventy"]
    [(= n 80) "eighty"]
    [(= n 90) "ninety"]
    [(>= n 100) ">= one hundred"] ; Hopefully we won't see this case so just do something simple :)
    [#t (format "~a-~a" (number->word (* 10 (quotient n 10))) (number->word (modulo n 10)))]))

(define (move-memory target-var)
  ;; Move to the given memory position. Clobbers op wheel value.
  (code
   (send (interpreter-state) move-memory (var-index target-var))
   (format "Seek to ~a" (var-name target-var))))

(define (assign destination-var source-var)
  ;; Set the destination variable to the current value of the source
  ;; variable. Clobbers both wheel values and seeks to
  ;; destination-var.
  (code
   (string-append
    (send (interpreter-state) move-memory (var-index source-var))
    (exec-str math/load-memory)
    (send (interpreter-state) move-memory (var-index destination-var))
    (exec-str math/store-memory))
   (format "Assign [~a] = [~a]" (var-name destination-var) (var-name source-var))))

(define (prelude)
  ;; Sets up constants
  (code
   (string-append
    (send (interpreter-state) move-memory (var-index var/-1))
    (exec-str math/set-to-zero)
    (exec-str math/not)
    (exec-str math/negate)
    (exec-str math/store-memory))
   "Initialize negative one constant"))

(define project-euler-179
  (parameterize ([interpreter-state (new state%)])
    (progn
     (prelude)
     (move-memory var/i)
     (exec op/set-to-one)
     (exec op/store-memory)
     (print-number)
     (assign var/j var/i)
     (move-memory var/j)
     (print-number))))

(print-code project-euler-179)
