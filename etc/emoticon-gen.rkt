
#lang racket

;; Generator for Emoticon

;; NOTE: No attempt is made to escape the string; it MUST be a valid
;; non-emoticon token.
(define (word plaintext)
  (~a plaintext))

(define (emote face nose mouth)
  (string-append face nose mouth))

(define face-counter "X:")
(define face-source "Z:")
(define face-current-list-name "A:")
(define face-set-markers "G:")
(define face-space "S:")
(define face-empty "E:")
(define face-default ":")

;; Change current list
(define (set-current-list face)
  (emote face "-" "O"))

;; Put length of face on left of current list
(define (get-length-of face)
  (emote face "-" "C"))

(define (reverse face)
  (emote face "-" "X"))

;; Rotate face (to the right), using left of current list as rotation
;; count
(define (rotate face)
  (emote face "-" "@"))

(define (move-left-curr-to face)
  (emote face "-" "<"))

(define (move-right-curr-to face)
  (emote face "-" ">"))

(define (copy-left-curr-to face)
  (emote face "-" "["))

(define (copy-right-curr-to face)
  (emote face "-" "]"))

(define (assign-curr-to face)
  (emote face "-" "D"))

;; Just look at the docs, this one is crazy complicated
(define (insert face)
  (emote face "-" "V"))

(define (print-left face)
  (emote face "-" "P"))

(define (print-and-pop-left face)
  (emote face "-" "Q"))

(define (maths-left op face)
  (emote face op "{"))

(define (maths-right op face)
  (emote face op "}"))

(define (compare-left op face)
  (emote face op "\\"))

(define (compare-right op face)
  (emote face op "/"))

(define (open-block) (emote ":" "-" "("))
(define (close-block) (emote ":" "-" ")"))
(define (divide-block) (emote ":" "-" "|"))
(define (break-on-left) (emote ":" "-" "3"))
(define (break-and-pop-on-left) (emote ":" "-" "E"))

(define (program . args)
  (if (and (= (length args) 1) (list? (first args)))
      (apply program (first args))
      (string-join args " ")))

;; All loop upper bounds are inclusive bounds. At the start of each
;; loop iteration, and after loop termination, the current list will
;; be ":".
;;
;; Runs until the rightmost of face-var is > rightmost of
;; face-upper-bound. Increments rightmost of face-var at each
;; iteration.
(define (for-loop face-var face-upper-bound . body)
  (when (or (equal? face-var ":") (equal? face-upper-bound ":"))
    (error "face-var and face-upper-bound cannot be ':', ':' is used for temporaries"))
  (program
    (open-block)
    (set-current-list face-var)
    (compare-right ">" face-upper-bound)
    (set-current-list ":")
    (break-and-pop-on-left)
    (program body)
    (set-current-list face-var)
    (word "1")
    (maths-right "+" face-var)
    (close-block)))

;; So the online interpreter has a really bizarrely strict
;; infinite-loop checker that chokes after a few hundred iterations.
;; So we're unrolling our loops in the hopes that the loop checker
;; doesn't flag us. As above, loop bounds are inclusive on both sides.
(define (for-loop-unrolled face-var lower-bound upper-bound . body)
  (program (for/list ([x (in-range lower-bound (+ upper-bound 1))])
             (program (set-current-list face-var)
                      (word x)
                      (program body)
                      (pop-right face-var)))))

(define (push-right face word-value)
  (program (set-current-list face)
           (word word-value)))

(define face-storage-array "|8") ; Stores our data array
(define face-40 ":.") ; Stores the constant 40
(define face-60 ":,") ; Stores the constant 60

;; Loop variables
(define face-white-to-spend "8-")
(define face-black-to-spend "8--")
(define face-white-left "8^")
(define face-black-left "8^-")

(define face-temporary "::") ; Miscellaneous temporary storage

(define face-garbage ":-G") ; Used only for pop-right

;; Pop rightmost of list (this is the easiest way I can see to do it...)
(define (pop-right face)
  (program (set-current-list face)
           (move-right-curr-to face-garbage)
           (set-current-list face-empty)
           (assign-curr-to face-garbage)))

(define project-euler-181
  (program ;; Initialize constants
           (push-right face-40 40)
           (push-right face-60 60)
           ;; Initialize array
           (push-right face-storage-array 1)
           (set-current-list ":::::")
           (word 100)
           (print-and-pop-left ":::::")
           (for-loop-unrolled face-temporary 0 40
                              (push-right face-white-left 0)
                              (for-loop face-white-left face-60
                                        (push-right face-storage-array 0))
                              (pop-right face-white-left))
           (set-current-list ":::::")
           (word 100)
           (print-and-pop-left ":::::")))

(displayln project-euler-181)
