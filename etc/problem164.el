
;; Working solution using naive caching.

(setq problem164-cache (make-hash-table :test 'equal :size 2000))

;; Takes the last two digits and the remaining number of digits as arguments.
(defun problem164-count (a b n)
  (cond
   ((<= n 0) 1)
   ((gethash (list a b n) problem164-cache))
   (t (let* ((max (- 9 a b))
             (result (loop for i from 0 to max
                           sum (problem164-count b i (1- n)))))
        (puthash (list a b n) result problem164-cache)))))

(defun problem164-total-count ()
  ;; Remember: First digit cannot be zero.
  (loop for i from 1 to 9
        sum (problem164-count 0 i 19)))

(message "%d" (problem164-total-count))
