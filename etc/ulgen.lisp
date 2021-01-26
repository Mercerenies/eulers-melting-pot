
;; Underload

(defparameter *commands*
  '((swap . "~")
    (dup . ":")
    (pop . "!")
    (cat . "*")
    (parens . "a")
    (eval . "^")
    (output . "S")))

(defgeneric translate (x))

(defmethod translate ((x string))
  x)

(defmethod translate ((x (eql nil)))
  "")

(defmethod translate ((x symbol))
  (or (cdr (assoc x *commands*)) (error "Bad command ~S" x)))

(defmethod translate ((x list))
  (if (eql (first x) 'str)
      (format nil "(~{~A~})"
              (mapcar #'translate (rest x)))
      (format nil "~{~A~}"
              (mapcar #'translate x))))

(defun succ ()
  `((str dup) swap cat
    (str cat) cat))

(defun add ()
  `((str dup)
    swap cat
    (str swap)
    cat swap cat
    (str cat)
    cat))

(defun mul ()
  (translate 'cat))

(defun pow ()
  (translate 'eval))

(defun pair ()
  '(swap parens swap parens cat))

(defun fst ()
  '(eval pop))

(defun snd ()
  '(eval swap pop))

(defun zero ()
  '(str pop (str "")))

(defun pred ()
  (let ((f `(,(snd) dup ,(succ) ,(pair))))
    `(,(zero) ,(zero) ,(pair) swap
      (str ,f) swap
      eval eval ,(fst))))

(defun sub ()
  `((str ,(pred)) swap eval eval))

(defun false ()
  '(str swap pop eval))

(defun true ()
  '(str pop eval))

(defun is-zero ()
  `(,(true) swap (str pop ,(false)) swap eval eval))

;; ///// Y Combinator

;; Thank you Esolang Wiki :)
;;
;; https://esolangs.org/wiki/Underload#Print_a_numeral_as_decimal
(defun output-num ()
  "((:(1)*(:(2)*(:(3)*(:(4)*(:(5)*(:(6)*(:(7)*(:(8)*(:(9)*(!~:^)))))))))):(~^~(~a~*~a~*)~a*^:(0)*)~a*~:(a(:^)*())~*a(:^)*~()~(0)~(~!^))~*^^!S!!!")

(defparameter *program*
  `("((ABC)*)" ,(y) output))
;  `((str (str "true") output) (str (str "false") output) ,(zero) ,(is-zero) eval))

(format t "~A~%" (translate *program*))

