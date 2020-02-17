
;; Snowman source generator / "compiler"

;; This "magic" string at the start of the program activates abdf
;;
;; "(@("

(defparameter *letter-operators*
  '((decr "NdE" 1 1)
    (incr "NiN" 1 1)
    (print "sP" 1 0)
    (to-string "tS" 1 1)
    (nth "aA" 2 1)
    (range "nR" 2 1)
    (equal? "eQ" 2 1)
    (dup "dU" 1 2)
    (div "nD" 2 1)
    (mod "NmO" 2 1)
    (floor "nF" 1 1)
    (ceil "nC" 1 1)
    (not "nO" 1 1)
    (wrap "wR" 1 1)))

(defparameter *vars*
  '())

(defconstant +reserved-vars+
  2)

(defparameter *next-available-var*
  +reserved-vars+)

(defun index-to-var (n)
  (concatenate 'string
               (make-string (floor n 2) :initial-element #\=)
               (if (zerop (mod n 2)) "+" "!")))

(defun translate-var (name)
  (check-type name symbol)
  (or
   (cdr (assoc name *vars*))
   (let ((new-var (index-to-var *next-available-var*)))
     (incf *next-available-var*)
     (push (cons name new-var) *vars*)
     new-var)))

;; Duplicates v1 into v2 (leaves v2 as active variable)
(defun duplicate-var (v1 v2)
  (when (numberp v1)
    (setq v1 (index-to-var v1)))
  (when (numberp v2)
    (setq v2 (index-to-var v2)))
  `("~" ,v1 "#" "dU" "*" ,v2 "*" "~"))

(defun translate-input (input)
  (etypecase input
    (string `(,@(duplicate-var input 0) "#"))
    (number (list input))
    (list input)))

(defun compile-call (name inputs outputs)
  (let ((fn (or (assoc name *letter-operators*)
                (error "Unknown function ~S" name))))
    (unless (= (length inputs) (third fn))
      (error "Expecting ~S inputs to ~S, got ~S" (third fn) (first fn) (length inputs)))
    (unless (= (length outputs) (fourth fn))
      (error "Expecting ~S outputs to ~S, got ~S" (fourth fn) (first fn) (length outputs)))
    (append
     (loop for input in inputs
           append (translate-input input))
     (list (second fn))
     (loop for output in outputs
           append `(,output "*")))))

(defun list-to-text (list)
  (format nil "~{~A~^ ~}"
          (loop with last-was-number = nil
                for elem in list
                if (and (numberp elem) last-was-number)
                    collect "vN"
                collect elem
                do (setq last-was-number (numberp elem)))))

(defun translate (line)
  (flet ((translate-arg (value) (translate-literal value :can-be-var t)))
    (case (first line)
      ;; (store literal variable)
      (store (destructuring-bind (literal var) (rest line)
               (let ((literal (translate-literal literal))
                     (var (translate-var var)))
                 `(,var ,literal "*"))))
      ;; (move src dest)
      (move (destructuring-bind (src dest) (rest line)
              (let ((src (translate-var src))
                    (dest (translate-var dest)))
                `(,src "#" ,dest "*"))))
      ;; (map var array out-var block)
      (map (destructuring-bind (var array out-var block) (rest line)
             (let ((var (translate-var var)))
               (let ((block (translate-block block :prefix `(,var "*") :suffix `(,var "#")))
                     (array (translate-literal array :can-be-var t))
                     (out-var (translate-var out-var)))
                 (append
                  (translate-input array)
                  block
                  `("aM" ,out-var "*"))))))
      ;; (each var array block)
      (each (destructuring-bind (var array block) (rest line)
              (let ((var (translate-var var)))
                (let ((block (translate-block block :prefix `(,var "*")))
                      (array (translate-literal array :can-be-var t)))
                  (append
                   (translate-input array)
                   block
                   '("aE"))))))
      ;; (if cond true-block false-block)
      (if (destructuring-bind (cond true-block false-block) (rest line)
            (let ((cond (translate-literal cond :can-be-var t))
                  (true-block (translate-block true-block))
                  (false-block (translate-block false-block)))
              (append
               true-block
               false-block
               (translate-input cond)
               '("bI")))))
      ;; (while cond var body)
      (while (destructuring-bind (cond var body) (rest line)
               (let ((var (translate-var var)))
                 (let ((cond (translate-block cond :suffix `(,var "#")))
                       (body (translate-block body)))
                   (append
                    body
                    cond
                    '("bW"))))))
      (t (destructuring-bind (name inputs outputs) line
           (let ((inputs (mapcar #'translate-arg inputs))
                 (outputs (mapcar #'translate-var outputs)))
             (compile-call name inputs outputs)))))))

(defun translate-literal (literal &key can-be-var)
  (etypecase literal
    (number literal)
    (list (translate-block literal))
    (symbol (assert can-be-var)
            (translate-var literal))))

(defun translate-block (lines &key prefix suffix)
  (append
   '(":")
   prefix
   (loop for line in lines
         append (translate line))
   suffix
   '(";")))

(defun translate-lines (lines)
  (loop for line in lines
        append (translate line)))

(defun full-translate (lines)
  (list-to-text
   (append
    '("(" "@" "(")
    (translate-lines lines))))

;; (format t "~A~%"
;;         (full-translate
;;          '((store 1 total)
;;            (while ((incr (total) (total))
;;                    (dup (total) (total total1))
;;                    (equal? (total1 10) (c))
;;                    (not (c) (c)))
;;                   c
;;                   ((dup (total) (total total2))
;;                    (to-string (total2) (total2))
;;                    (print (total2) ()))))))

;; ///// gcd part is not efficient enough, maybe hand-write that
;; section if possible?
(format t "~A~%"
        (full-translate
         '((store 0 total)
           (range (4 12001) (iter))
           (each d iter
                 ((dup (d) (d d1))
                  (dup (d1) (d1 d2))
                  (div (d1 3) (n0))
                  (div (d2 2) (n1))
                  (floor (n0) (n0))
                  (ceil (n1) (n1))
                  (incr (n0) (n0))
                  (range (n0 n1) (iter1))

                  (dup (d) (d aaa))
                  (mod (aaa 100) (bbb))
                  (equal? (bbb 0) (bbb))
                  (if bbb
                      ((dup (d) (d aaa))
                       (to-string (aaa) (aaa))
                       (print (aaa) ())
                       (wrap (13) (aaa))
                       (print (aaa) ())
                       (wrap (10) (aaa))
                       (print (aaa) ()))
                    ())

                  (each n iter1
                        ;; gcd calculation
                        ((while ((dup (d) (d dzero))
                                 (equal? (dzero 0) (cond))
                                 (not (cond) (cond)))
                                cond
                                ((dup (d) (t0 t1))
                                 (mod (n t0) (d))
                                 (move t1 n)))
                         (equal? (n 1) (eq))
                         (if eq
                             ((incr (total) (total)))
                           ())))))
           (to-string (total) (total))
           (print (total) ()))))

;; (format t "~A~%"
;;         (full-translate
;;          '((range (0 1000001) (phi))
;;            (range (2 1000001) (iter))
;;            (each index iter
;;                  ((nth (phi index) (phi[index]))
;;                   (equal? (phi[index] index) (cond))
;;                   (if cond
;;                       ((dup (index) (index jndex)))
;;                     ())))
;;            (to-string (963) (aaa))
;;            (print (aaa) ()))))
