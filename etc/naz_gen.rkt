
#lang racket

;; Generator script for naz programs.

(define (seq . args)
  (string-join args ""))

(define ((nullary-command instruction-name))
  ;; Nullary commands do not use their argument, so we just put a zero
  ;; there.
  (format "0~a" instruction-name))

(define ((simple-command instruction-name) n)
  (unless (<= 0 n 9)
    (raise (format "Invalid command parameter: ~a" n)))
  (format "~a~a" n instruction-name))

(define add (simple-command "a"))
(define div (simple-command "d"))
(define call (simple-command "f"))
(define halt (nullary-command "h"))
(define mul (simple-command "m"))
(define negate (simple-command "n"))
(define output (simple-command "o"))
(define mod (simple-command "p"))
(define from-input (simple-command "r"))
(define sub (simple-command "s"))
(define get-var (simple-command "v"))

(define (set-var n)
  (format "2x~av" n))

(define (goto-if operation var-number fn-number)
  (let ([operation-string (case operation
                            [(register<var) "l"]
                            [(register=var) "e"]
                            [(register>var) "g"]
                            [else (raise (format "Invalid operation: ~a" operation))])])
    (format "3x~av~a~a" var-number fn-number operation-string)))

;; Precondition: def-function calls shall not be nested within one
;; another. I do not check this precondition.
(define (def-function function-number . body)
  (format "1x~af~a\n" function-number (string-join body "")))

;; Convenience helpers just to make the code more immediately readable
(define (mul10) (seq (mul 5) (mul 2)))
(define (div10) (seq (div 5) (div 2)))

(define var-tmp 0)
(define var-tmp1 1)
(define var-reverse-src 2)
(define var-reverse-dest 3)
(define var-reverse-count 4)

;; This function takes the number in the register and performs "mod
;; 10" on it. We can't do so directly with the "p" instruction because
;; instruction constants are limited to the range of 0 to 9. Clobbers
;; var-tmp and var-tmp1.
(define fn-mod10 0)

;; Conditional helper for fn-mod10.
(define fn-mod10-add5-helper 1)

;; Adds the register to var-reverse-dest, recursively. The register
;; will be set to zero. Clobbers var-tmp and var-tmp1.
(define fn-add-to-var 2)
(define fn-add-to-var-helper 3)

;; This function moves var-reverse-src into var-reverse-dest,
;; reversing the characters along the way and keeping count in
;; var-reverse-count. Assumes unconditionally that the input is
;; greater than zero (i.e. the recursion check is done at the end,
;; do-while style, rather than the beginning, while-style). Clobbers
;; var-tmp and var-tmp1.
(define fn-reverse-num 4)

;; Prints var-reverse-dest, assuming the number of digits is
;; var-reverse-count. Assumes unconditionally that var-reverse-count
;; is greater than zero. Clobbers var-tmp and var-tmp1
(define fn-print-num 5)

(displayln (seq
            (def-function fn-mod10
              ;; Store original input in var-tmp
              (set-var var-tmp)
              ;; Store input % 5 in var-tmp1
              (mod 5)
              (set-var var-tmp1)
              ;; Store (input / 5) % 2 in var-tmp
              (get-var var-tmp)
              (div 5)
              (mod 2)
              (set-var var-tmp)
              ;; Add 5 to result if necessary
              (mul 0)
              (add 1)
              (goto-if 'register=var var-tmp fn-mod10-add5-helper)
              (get-var var-tmp1))
            (def-function fn-mod10-add5-helper
              (get-var var-tmp1)
              (add 5))
            (def-function fn-add-to-var
              (set-var var-tmp)
              (mul 0)
              (set-var var-tmp1)
              (get-var var-tmp)
              (goto-if 'register>var var-tmp1 fn-add-to-var-helper))
            (def-function fn-add-to-var-helper
              (set-var var-tmp)
              ;; Add 1 to var-reverse-dest
              (get-var var-reverse-dest)
              (add 1)
              (set-var var-reverse-dest)
              ;; Retrieve input and subtract one
              (get-var var-tmp)
              (sub 1)
              (call fn-add-to-var))
            (def-function fn-reverse-num
              ;; Increment var-reverse-count
              (get-var var-reverse-count)
              (add 1)
              (set-var var-reverse-count)
              ;; Append a digit to var-reverse-dest
              (get-var var-reverse-dest)
              (mul10)
              (set-var var-reverse-dest)
              (get-var var-reverse-src)
              (call fn-mod10)
              (call fn-add-to-var)
              ;; Divide var-reverse-src by 10 and continue
              (mul 0)
              (set-var var-tmp)
              (get-var var-reverse-src)
              (div10)
              (set-var var-reverse-src)
              (goto-if 'register>var var-tmp fn-reverse-num))
            (def-function fn-print-num
              ;; Print out first digit of var-reverse-dest
              (get-var var-reverse-dest)
              (call fn-mod10)
              (output 1)
              ;; Divide var-reverse-dest by 10
              (get-var var-reverse-dest)
              (div10)
              (set-var var-reverse-dest)
              ;; Set var-tmp to 0
              (mul 0)
              (set-var var-tmp)
              ;; Subtract one from var-reverse-count
              (get-var var-reverse-count)
              (sub 1)
              (set-var var-reverse-count)
              ;; Call recursively if var-reverse-count > 0
              (goto-if 'register>var var-tmp fn-print-num))
            ;; Start by pre-computing 11 * 11 = 121, since the
            ;; multi-digit one will be the hardest to do in naz.
            (mul 0)
            (add 8)
            (add 9)
            (mul 7)
            (add 2)
            ;; Now multiply all of the other (single-digit) values in.
            (mul 7)
            (mul 7)
            (mul 7)
            (mul 5)
            (mul 5)
            (mul 5)
            (mul 5)
            (mul 5)
            (mul 3)
            (mul 3)
            (mul 3)
            (mul 3)
            (mul 3)
            (mul 3)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            (mul 2)
            ;; And print it :D
            (set-var var-reverse-src)
            (mul 0)
            (set-var var-reverse-dest)
            (mul 0)
            (set-var var-reverse-count)
            (call fn-reverse-num)
            (call fn-print-num)))
