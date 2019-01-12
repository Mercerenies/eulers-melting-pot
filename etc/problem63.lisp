
(defun find-n-digit-powers (n)
  (loop
     for i from 2 to 9
     for p = (expt i n)
     while (<= (length (format nil "~A" p)) n)
     when (= (length (format nil "~A" p)) n)
         collect p))

;; We know 10^k has more than k digits, so the only way this can work
;; is if the base is <= 9. Further, 9^22 has 21 digits, so 22 and
;; larger powers will always be too small. Thus, we can stop at
;; exponent 21.
(let ((count (loop
                for n from 1 to 21
                sum (length (find-n-digit-powers n)))))
  ;; 1+ because we include the 1^1 case which is excluded from
  ;; find-n-digit-powers.
  (format t "~A" (1+ count)))
