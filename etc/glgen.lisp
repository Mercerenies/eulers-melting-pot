
(defun instr-to-gl (instr)
  (ecase (first instr)
    (add "a")
    (bring-to-top "b")
    (copy-top "c")
    (divide "d")
    (end-loop "e")
    (flip "f")
    (greater "g")
    (execute-instr "h")
    (input "i")
    (jump-forward "j")
    (kill-stack "k")
    (start-loop "l")
    (multiply "m")
    (push (make-string (second instr) :initial-element #\n))
    (output-num "o")
    (output-asc "p")
    (noop "q")
    (remainder "r")
    (subtract "s")
    (terminate "t")
    (unbring-to-top "u")
    (push-ascii (format nil "v~%~a" (code-char (second instr))))
    (push-100 "w")
    (pop-top "x")
    (pop-at (make-string (second instr) :initial-element #\y))
    (negate "z")))

(defun translate (stmts)
  (format nil "~{~a~%~}" (mapcar #'instr-to-gl stmts)))

;; -0, +1
(defconstant +read-one-number+
  '((push-ascii 65)
    (push-ascii 113)
    (subtract)
    (input)
    (subtract)
    (push 10)
    (multiply)
    (push-ascii 65)
    (push-ascii 113)
    (subtract)
    (input)
    (subtract)
    (add)
    (input)
    (pop-top)))

;; -0, +1
(defconstant +push-zero+
  `((push-100)
    (push-100)
    (subtract)))

;; -0, +1
(defconstant +push-one+
  `((push-ascii 99)
    (push-100)
    (subtract)))

;; -2, +1
(defconstant +maximum+
  `((copy-top)
    (unbring-to-top)
    (flip)
    (copy-top)
    (unbring-to-top)
    (greater)
    (bring-to-top)
    (flip)
    (bring-to-top)
    (flip)
    (negate)
    (start-loop)
    (pop-top)
    (flip)
    ,@+push-zero+
    (end-loop)
    (pop-top)
    (pop-top)))

(defconstant +program+
  `(,@+push-zero+
    (copy-top)
    (push-100)
    ;; Main outer loop
    (start-loop)
    ;; When we get here, the "current" row is at the bottom of the
    ;; stack, padded on both sides with zeroes. On top is the number
    ;; of iterations remaining.
    (copy-top)
    (push-100)
    (subtract)
    ,@+push-one+
    (add)
    ;; Inner loop
    (start-loop)
    ;; Now, the row is on the bottom, padded with zeroes. On top of
    ;; that is the number of big iterations remaining, and on top of
    ;; that is the number of small iterations remaining.
    ,@+read-one-number+
    (copy-top)
    (bring-to-top)
    (add)
    (flip)
    (bring-to-top)
    (copy-top)
    (unbring-to-top)
    (add)
    ,@+maximum+
    (flip)
    (unbring-to-top)
    (flip)
    (bring-to-top)
    ,@+push-one+
    (flip)
    (subtract)
    ;; End of inner loop
    (end-loop)
    (flip)
    ,@+push-one+
    (flip)
    (subtract)
    ;; End of outer loop
    (end-loop)
    (pop-top)
    (push-100)
    ;; Fold loop
    (start-loop)
    (unbring-to-top)
    ,@+maximum+
    (bring-to-top)
    ,@+push-one+
    (flip)
    (subtract)
    ;; End fold loop
    (end-loop)
    (pop-top)
    (output-num)
    (push 10)
    (output-asc)
    (terminate)))

(format t "~a~%" (translate +program+))
