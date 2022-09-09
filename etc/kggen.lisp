
(defun pushlit (x)
  (format nil "~A" x))

(defun oprogn (&rest args)
  (format nil "~{~A~}" args))

(defun oif (true-case &optional false-case)
  (format nil "[~A~@[|~A~]]" true-case false-case))

(defun ofor (a &optional b)
  (format nil "(~A~@[|~A~])" a b))

(defun owhile (a &optional b)
  (format nil "{~A~@[|~A~]}" a b))

(defun push-stack-length () "!")
(defun dup () ":")
(defun opop () "_")
(defun printch () ",")
(defun printnum () ".")
(defun dig-bottom () "'")
(defun bury-top () "\"")
(defun oreverse () "^")
(defun swap () "$")
(defun getset-register () "&")
(defun add () "+")
(defun sub () "-")
(defun mul () "*")
(defun div () "/")
(defun omod () "%")
(defun lt () "<")
(defun gt () ">")
(defun oeq () "=")

(defun dup2 ()
  (format nil "~@{~A~}"
          (bury-top)
          (dup)
          (dig-bottom)
          (swap)
          (bury-top)
          (dup)
          (dig-bottom)
          (swap)))

(defun int-div ()
  "Takes two values on the stack, integer divides them."
  (format nil "~@{~A~}"
          (dup)
          (bury-top)
          (bury-top)
          (dup)
          (dig-bottom)
          (omod)
          (sub)
          (dig-bottom)
          (div)))

(defun calc-gcd ()
  "Assumes two values on top of the stack, pops them and pushes the result."
  (format nil "~@{~A~}"
          (owhile (dup)
                  (oprogn (dup)
                          (bury-top)
                          (omod)
                          (dig-bottom)
                          (swap)))
          (opop)))

(defun inner-while-condition ()
  "Assumes m and n at top of stack, doesn't pop them but pushes the result."
  (format nil "~@{~A~}"
          (dup2)
          (swap)
          (bury-top)
          (dup)
          (dig-bottom)
          (add)
          (mul)
          (pushlit 2)
          (mul)
          (pushlit "d")
          (dup)
          (mul)
          (dup)
          (mul)
          (lt)))

;; Stack: count n m

(format t "~@{~A~}"
        (pushlit 0)
        (pushlit 1)
        (ofor (oprogn (pushlit "dsA") (sub) (mul))
              (oprogn (dup) (printnum) (pushlit 5) (pushlit 5) (add) (printch)
                      (dup)
                      (pushlit 1)
                      (add)
                      (inner-while-condition)
                      (owhile ""
                              (oprogn (pushlit 2)
                                      (add)
                                      (inner-while-condition)))
                      (opop)
                      (pushlit 1)
                      (add)))
        (printnum))
