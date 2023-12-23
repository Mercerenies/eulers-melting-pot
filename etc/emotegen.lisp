
;; Emotinomicon code generation helper

(defparameter +default-push-num-length+ 13)

(defclass label ()
  ((label-name :type symbol :accessor label-name :initarg :label-name)))

(defclass goto ()
  ((label-name :type symbol :accessor label-name :initarg :label-name)))

;; Condition must have stack effect ( -- x) and must NOT depend on the
;; current top of the stack. If condition is true, then we pop it and
;; jump to the label. If condition is false, we proceed.
;;
;; If inverted-condition is true, then we jump on a false condition
;; and proceed on a true one. The default for inverted-condition is
;; false.
(defclass goto-maybe ()
  ((branch-condition :type list :accessor branch-condition :initarg :branch-condition)
   (label-name :type symbol :accessor label-name :initarg :label-name)
   (inverted-condition :type boolean :accessor inverted-condition :initarg :inverted-condition :initform nil)))

(defun label (label-name)
  (make-instance 'label :label-name label-name))

(defun goto (label-name)
  (make-instance 'goto :label-name label-name))

(defun goto-maybe (branch-condition label-name &key (inverted-condition nil))
  (make-instance 'goto-maybe
                 :branch-condition branch-condition
                 :label-name label-name
                 :inverted-condition inverted-condition))

(defgeneric parse (cmd index labels-hash)
  (:documentation "Takes the command, the current position in the
   source code, and a hash table of known labels. The last argument is
   mutable. Returns the new position after executing the given
   command. We need to pre-process the input so we know the source
   index of labels for goto statements, since that's our only
   nontrivial conditional capability."))

(defmethod parse ((cmd list) index labels-hash)
  ;; List commands are always of length 1 in the resulting source
  ;; code.
  (1+ index))

(defmethod parse ((cmd label) index labels-hash)
  ;; Labels do not produce any code, but they modify the labels hash.
  (when (gethash (label-name cmd) labels-hash)
    (error "Duplicate label ~A" (label-name cmd)))
  (setf (gethash (label-name cmd) labels-hash) index)
  index)

(defmethod parse ((cmd string) index labels-hash)
  (+ index (length cmd)))

(defmethod parse ((cmd goto) index labels-hash)
  (+ index (1+ +default-push-num-length+)))

(defmethod parse ((cmd goto-maybe) index labels-hash)
  (let* ((index1 (+ index +default-push-num-length+))
         (index2 (parse-seq (branch-condition cmd) index1 labels-hash))
         (index3 (+ index2 3)))
    index3))

(defun parse-seq (cmds starting-index labels-hash)
  (loop with index = starting-index
        for cmd in cmds
        do (setf index (parse cmd index labels-hash))
        finally (return index)))

(defgeneric translate (cmd labels-hash)
  (:documentation "Takes the command and produces a string. The string must be of the length
   promised by #'parse."))

(defmethod translate ((cmd label) labels-hash)
  "")

(defmethod translate ((cmd goto) labels-hash)
  (let ((destination-index (gethash (label-name cmd) labels-hash)))
    (unless destination-index
      (error "No such label ~A" (label-name cmd)))
    (unless (< destination-index 10000)
      (error "Label index ~A too big, increase the limit :(" destination-index))
    (format nil "~{~A~}"
            (append (mapcar (lambda (c) (translate c labels-hash)) (push-num-fixed-length destination-index))
                    (list (translate '(pop-and-goto) labels-hash))))))

(defmethod translate ((cmd goto-maybe) labels-hash)
  (let ((destination-index (gethash (label-name cmd) labels-hash)))
    (unless destination-index
      (error "No such label ~A" (label-name cmd)))
    (unless (< destination-index 10000)
      (error "Label index ~A too big, increase the limit :(" destination-index))
    (format nil "~{~A~}"
            (append (mapcar (lambda (c) (translate c labels-hash)) (push-num-fixed-length destination-index))
                    (mapcar (lambda (c) (translate c labels-hash)) (branch-condition cmd))
                    (list (translate (if (inverted-condition cmd) '(skip-if-true) '(skip-if-false)) labels-hash)
                          (translate '(pop-and-goto) labels-hash)
                          (translate '(drop) labels-hash))))))

(defmethod translate ((cmd string) labels-hash)
  cmd)

(defmethod translate ((cmd list) labels-hash)
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
    (double-factorial "‼️")
    (skip-if-false "⁉️")))

(defmethod translate-all (cmds)
  (let ((labels-hash (make-hash-table :test 'equal)))
    (parse-seq cmds 0 labels-hash)
    (mapcar (lambda (cmd) (translate cmd labels-hash)) cmds)))

(defun lit-string (body)
  `((begin-string)
    ,@body
    (end-string)))

(defun do-loop (body)
  `((open-loop)
    ,@body
    (close-loop)))

(defun do-if (condition body &key (label (gensym)) (inverted-condition t))
  "condition shall have stack effect ( -- x) and body can have arbitrary
   stack effect."
  `(,(goto-maybe condition label :inverted-condition inverted-condition)
    ,@body
    ,(label label)))

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

;; I think the moon phases are pretty, so I'm going to use them as
;; padding :) We don't have to use different characters (we could just
;; spam 'new moon', but I consider myself something of an artist)
(defparameter *padding*
  #("🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"))

(defparameter *padding-position*
  0)

(defun generate-padding-list (n)
  (if (<= n 0)
      nil
      (cons (elt *padding* *padding-position*)
            (progn (setf *padding-position* (mod (1+ *padding-position*) (length *padding*)))
                   (generate-padding-list (1- n))))))

(defun pad-list (list desired-length)
  (append list (generate-padding-list (- desired-length (length list)))))

(defun push-num-fixed-length (n &optional (desired-length +default-push-num-length+))
  "As push-num but padding to a desired length. The default
   desired-length of 13 assumes that n is less than 10,000, as every
   number less than that can be represented in 13 characters or less."
  (let ((num-instructions (push-num n)))
    (pad-list num-instructions desired-length)))

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
    (n-1)
    (n+1)
    (div)
    (floor)
    (pop-and-modify-code)
    ,@(push-num (1+ n))
    ,@*65536*
    ,@body
    (n-1)
    (n+1)
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
                               ;; truthiness. But fortunately, n-1 and
                               ;; n+1 coerce back to regular numbers
                               ;; that are correctly falsy at zero.
                               (n+1)
                               (n-1)))
                         ,@(get-var *var-numbers-count*)
                         (output-num))))

;; Mocking up the second half of the program
(format t "~{~A~}~%"
        (translate-all `(,@(save-var *var-numbers-count*
                                     `((push3)))
                         ,@(push-num 100)
                         (push3)
                         ,@(save-var *var-whole-part*
                                     `((push1)))
                         ,@(get-var *var-whole-part*)
                         ,@(do-if
                           `((push1)
                             ,@(get-var *var-numbers-count*)
                             (n%2)
                             (sub)
                             (n-1)
                             (n+1))
                           `(,@(do-if
                               `(,@(get-var *var-whole-part*)
                                 (n-1))
                               `(;; Case 1: We need to correct, but the top of the stack is not 1.
                                 (push1)
                                 (sub)
                                 (push1)
                                 ,@(save-var *var-temporary*
                                             `(,@(get-var *var-numbers-count*)))
                                 ,@(save-var *var-numbers-count*
                                             `(,@(get-var *var-temporary*)
                                               (n+1)))))
                             ,@(do-if
                               `((push1)
                                 ,@(get-var *var-numbers-count*)
                                 (n%2)
                                 (sub)
                                 (n-1)
                                 (n+1))
                               `(;; Case 2: We need to correct, top of the stack equals 1.
                                 (drop)
                                 (n-1)
                                 (n+1)
                                 (n+1))))))))

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
