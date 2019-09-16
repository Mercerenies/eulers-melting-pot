
(defun convergent (x)
  (cond
   ((zerop x) 2)
   ((= 2 (mod x 3)) (* 2 (1+ (/ (- x 2) 3))))
   (t 1)))

(defun main ()
  (let ((conv 99)
        (num 1)
        (denom 0))
    (loop for i from conv downto 0
          maximizing num into tmp
          do (progn
               ;; Reciprocate
               (psetq num denom denom num)
               ;; Add value
               (incf num (* denom (convergent i)))
               ;; Simplify fraction
               (setf num (/ num (gcd num denom)))
               (setf denom (/ denom (gcd num denom))))
          finally (format t "MAX ~S~%" tmp))
    (let ((result 0))
      (loop while (/= num 0)
            do (setf result (+ result (mod num 10)))
            do (setf num (floor (/ num 10))))
      (format t "~S~%" result))))

(main)
