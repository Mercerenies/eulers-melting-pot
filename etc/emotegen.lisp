
;; Emotinomicon code generation helper

(defgeneric translate (cmd))

(defmethod translate ((cmd list))
  (ecase (first cmd)
    (push0 "😀")
    (push1 "😅")
    (push2 "😉")
    (push3 "😍")
    (push4 "😒")
    (push5 "😗")
    (push6 "😜")
    (push7 "😡")
    (push8 "😁")
    (push9 "😆")
    (push10 "🔟")
    (push100 "💯")
    (add "➕")
    (sub "➖")
    (div "➗")
    (mul "✖")
    (input-char "⏫")
    (output-char "⏬")
    (open-loop "⏪")
    (close-loop "⏩")
    (dup "🆙")
    (drop "😊")
    (reverse-stack "😎")
    (reverse-n "😓") ; DOESN'T WORK IN THE REFERENCE IMPLEMENTATION !!
    (pow "😘")
    (log "😝")
    (negate "😢")
    (abs "😂")
    (2n "😇")
    (3n "☺️")
    (4n "😏")
    (n/2 "😔")
    (n/3 "😙")
    (n/4 "😞")
    (n^2 "😣")
    (n^3 "😃")
    (n^4 "😈")
    (sqrt "😋")
    (cbrt "😐")
    (4rt "😕")
    (floor "😚")
    (ceil "😟")
    (round "😤")
    (n+1 "😄")
    (n-1 "👿")
    (mod "😌")
    (2^n "😑")
    (3^n "😖")
    (4^n "😛")
    (fibo "😠")
    (lucas "😥")
    (pi "😦")
    (e "😫")
    (phi "😰")
    (ln "😵")
    (log10 "😺")
    (e^n "😿")
    (10^n "😧")
    (pop-and-goto "😬")
    (pop-and-modify-code "😱")
    (pop-and-get-code "😶")
    (n%2 "😻")
    (rand "🙀")
    (output-num "😨")
    (begin-quote "😭")
    (end-quote "😲")
    (factorial "️❗")
    (if-then-else "️❓")
    (skip "❕")
    (skip-if-true "❔")
    (double-factorial "‼")
    (skip-if-false "⁉")))

(defmethod translate ((cmd string))
  cmd)

(defmethod translate-all (cmd)
  (mapcar #'translate cmd))

(defun lit-string (body)
  `((begin-string)
    ,@body
    (end-string)))

(defun do-loop (body)
  `((open-loop)
    ,@body
    (close-loop)))

;; Only supports numbers from 0 to 100
(defun push-num (n)
  (case n
    (0 '((push0)))
    (1 '((push1)))
    (2 '((push2)))
    (3 '((push3)))
    (4 '((push4)))
    (5 '((push5)))
    (6 '((push6)))
    (7 '((push7)))
    (8 '((push8)))
    (9 '((push9)))
    (10 '((push10)))
    (100 '((push100)))
    (t (multiple-value-bind (quotient remainder) (floor n 10)
         `((push10) ,@(push-num quotient) (mul) ,@(push-num remainder) (add))))))

;; Using repeated multiplication, we do better than naive push-num
;; since we'll be using this number A LOT.
(defparameter *65536*
  '((push10) (push6) (add) (dup) (mul) (dup) (mul)))

;; Copies the Nth element on the stack
;;
;; (copy-nth 0) is equivalent to (dup)
;; (defun copy-nth (n)
;;   `(,@(push-num n)
;;     (reverse-n)
;;     (dup)
;;     ,@(push-num (1+ n))
;;     (reverse-n)
;;     ,@(push-num n)
;;     (reverse-n)
;;     ,@(push-num (1+ n))
;;     (reverse-n)))

(defun padding (n)
  (loop for i from 1 to n
        collect '(reverse-stack)))

;; Each variable occupies two cells, so make sure to use even numbers
;; here. We store the higher order bits in the given position N and
;; the lower order bits in N+1.
(defparameter *var-numerator* 0)
(defparameter *var-denominator* 2)
(defparameter *var-numbers-count* 4)
(defparameter *var-temporary* 6)
(defparameter *var-whole-part* 8)
(defparameter *var-new-numerator* 10)

(defun save-var (n body)
  "Save the top of the stack to the variable. Supports up to 32
  bits (unsigned). Note that this evaluates the body twice, since
  there's no reliable way to swap stack positions.

  Note that the stack effect of `body` must be exactly ( -- x), and
  that value will be pushed onto the stack."
  `(,@(push-num n)
    ,@body
    ,@*65536*
    (div)
    (floor)
    (pop-and-modify-code)
    ,@(push-num (1+ n))
    ,@*65536*
    ,@body
    (mod)
    (pop-and-modify-code)))

(defun get-var (n)
  `(,@(push-num n)
    (pop-and-get-code)
    ,@*65536*
    (mul)
    ,@(push-num (1+ n))
    (pop-and-get-code)
    (add)))

(format t "~{~A~}~%"
        (translate-all `(,@(padding 20) ; Space used for read-write variables
                         ,@(save-var *var-numerator*
                                     `(,@(push-num 987654321)))
                         ,@(save-var *var-denominator*
                                     `(,@(push-num 123456789)))
                         ,@(save-var *var-numbers-count*
                                     `(,@(push-num 0)))
                         (push1) ; Loop sentinel
                         ,@(do-loop
                             `((drop)
                               ,@(save-var *var-whole-part*
                                           `(,@(get-var *var-numerator*)
                                               ,@(get-var *var-denominator*)
                                               (div)
                                               (floor)))
                               ;; We have the whole part; leave it on
                               ;; the stack for later and just keep
                               ;; going.
                               ,@(get-var *var-whole-part*)
                               ,@(save-var *var-new-numerator*
                                           `(,@(get-var *var-numerator*)
                                             ,@(get-var *var-whole-part*)
                                             ,@(get-var *var-denominator*)
                                             (mul)
                                             (sub)))
                               ,@(save-var *var-numerator*
                                           `(,@(get-var *var-denominator*)))
                               ,@(save-var *var-denominator*
                                           `(,@(get-var *var-new-numerator*)))
                               ,@(save-var *var-temporary*
                                           `(,@(get-var *var-numbers-count*)
                                             ,@(push-num 1)
                                             (add)))
                               ,@(save-var *var-numbers-count*
                                           `(,@(get-var *var-temporary*)))
                               ,@(get-var *var-numerator*)
                               ,@(get-var *var-denominator*)
                               (mul)
                               ;; Most of the arithmetic operations in
                               ;; the language operate on bignums that
                               ;; don't work correctly when taken for
                               ;; truthiness. But fortunately, n+1 and
                               ;; n-1 coerce back to regular numbers
                               ;; that are correctly falsy at zero.
                               (n+1)
                               (n-1)))
                         ,@(get-var *var-numbers-count*)
                         (output-num))))

;; Mocking up the second half of the program
(format t "~{~A~}~%"
        (translate-all `(,@(save-var *var-numbers-count*
                                     `((push2)))
                         ,@(push-num 100)
                         ,@(push 20)
                         ,@(get-var *var-numbers-count*)
                         (n%2))))

;; (format t "~{~A~}~%"
;;         (translate-all `(,@(push-num 10)
;;                          ,@(push-num 20)
;;                          ,@(push-num 30)
;;                          ,@(push-num 40)
;;                          ,@(push-num 50)
;;                          ,@(push-num 60)
;;                          ,@(copy-nth 2)
;;                          (output-num)
;;                          (output-num)
;;                          (output-num)
;;                          (output-num)
;;                          (output-num)
;;                          (output-num))))
