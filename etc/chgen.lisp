
(defparameter *loop-counter*
  0)

(defun identify-vars (x)
  (typecase x
    (symbol (list x))
    (integer (list x))
    (list (mapcan #'identify-vars (cdr x)))
    (t ())))

(defun replace-var (x a b)
  (typecase x
    (symbol (if (eql x a) b x))
    (integer (if (eql x a) b x))
    (list (if (eql (car x) 'call) x (cons (car x) (mapcar (lambda (y) (replace-var y a b)) (cdr x)))))
    (t x)))

(defun replace-vars (x)
  (let ((vars (loop for i upfrom 0
                    for v in (identify-vars x)
                    for v1 = (format nil "SYMBOL~A" i)
                    collect (list v v1))))
    (values
     (reduce (lambda (acc var) (apply #'replace-var acc var)) vars :initial-value x)
     (mapcar (lambda (x) (list (if (symbolp (first x)) 0 (first x)) (second x))) vars))))

(defun loop-counter! ()
  (prog1 (format nil "loop~D" *loop-counter*)
    (incf *loop-counter*)))

(defun to-nth (x)
  (case (first x)
    (1 "1st")
    (2 "2nd")
    (3 "3rd")
    (t (format nil "~Dth" x))))

(defun handle-loop (x loop-down countdown)
  (let ((counter (loop-counter!))
        (counter1 (loop-counter!)))
    (concatenate 'string
                 (format nil "~A the ~A. " counter loop-down)
                 (tap-instructions x)
                 (format nil "~A~:[~*~; the ~A~] until ~Aed. " counter1 countdown loop-down counter))))

(defgeneric special-instruction (header &rest args))

(defmethod special-instruction ((header (eql 'mov)) &rest args) ; (mov src dest)
  (destructuring-bind (src dest) args
    (tap-instructions `((push ,src (1))
                        (pop ,dest (1))))))

(defmethod special-instruction ((header (eql 'if)) &rest args) ; (if g . body)
  (destructuring-bind (g . body) args
    (tap-instructions `((mov ,g "TEMP_VAR")
                        (do-loop "TEMP_VAR"
                          ,@body
                          (mov "ZERO_VAR" "TEMP_VAR"))))))

(defmethod special-instruction ((header (eql 'forever)) &rest args) ; (forever . body)
  (tap-instructions `((mov "ONE_VAR" "TEMP_VAR")
                      (do-loop "TEMP_VAR"
                        ,@args
                        (mov "ONE_VAR" "TEMP_VAR")))))

(defmethod special-instruction ((header (eql 'return)) &rest args) ; (return g)
  (tap-instructions `((push ,(first args) (1))
                      (exit))))

(defun tap-instruction (x)
  (let ((val
         (case (first x)
           (read `("Take ~A from refrigerator. " ,(second x)))                                  ; (read g)
           (push `("Put ~A into ~A mixing bowl. " ,(second x) ,(to-nth (third x))))             ; (push g (n))
           (pop `("Fold ~A into ~A mixing bowl. " ,(second x) ,(to-nth (third x))))             ; (pop g (n))
           (add `("Add ~A to ~A mixing bowl. " ,(second x) ,(to-nth (third x))))                ; (add g (n))
           (sub `("Remove ~A from ~A mixing bowl. " ,(second x) ,(to-nth (third x))))           ; (sub g (n))
           (mul `("Combine ~A into ~A mixing bowl. " ,(second x) ,(to-nth (third x))))          ; (mul g (n))
           (div `("Divide ~A into ~A mixing bowl. " ,(second x) ,(to-nth (third x))))           ; (div g (n))
           (roll `("Stir the ~A mixing bowl for ~A minutes. " ,(to-nth (second x)) ,(third x))) ; (roll (n) x)
           (clear `("Clean the ~A mixing bowl. " ,(to-nth (second x))))                         ; (clear (n))
           (pprint `("Pour contents of the ~A mixing bowl into the 1st baking dish. "           ; (pprint (n))
                     ,(to-nth (second x))))
           (do-loop (handle-loop (cddr x) (second x) nil))                                      ; (do-loop g.body)
           (for-loop (handle-loop (cddr x) (second x) t))                                       ; (for-loop g.body)
           (last "Set aside. ")                                                                 ; (last)
           (call `("Serve with ~A. " ,(second x)))                                              ; (call sub)
           (exit "Refrigerate. ")                                                               ; (exit)
           (t (apply #'special-instruction x)))))
    (if (listp val)
        (apply #'format nil val)
        val)))

(defun tap-instructions (x)
  (apply #'concatenate 'string
         (mapcar #'tap-instruction x)))

(defun do-body (x fst) ; (name . body)
  (multiple-value-bind (x1 vars) (replace-vars x)
    (format nil "~A.~%~%Ingredients.~%~{~{~A g ~A~}~%~}~%Method.~%~A~%~%~:[~;Serves 1.~%~%~]"
            (first x)
            (append '((0 "TEMP_VAR") (0 "ZERO_VAR") (1 "ONE_VAR")) vars)
            (tap-instructions (cdr x1)) fst)))

(defun tap-subs (x)
  (apply #'concatenate 'string
         (loop for first-exec = t then nil
               for subr in x
               collect (do-body subr first-exec))))

(defun result ()
  (tap-subs '((f
               (push 0 (3))
               (push 12 (2))
               (call solns)
               (pprint (1)))
              (solns ; (solns perimeter)
               (push 0 (3))
               (pop accum (3))
               (pop counter1 (2))
               (mov counter1 perm)
               (for-loop counter1
                (push perm (3))
                (sub counter1 (3))
                (pop counter2 (3))
                (for-loop counter2
                          (push perm (3))
                          (sub counter1 (3))
                          (sub counter2 (3))
                          (pop counter3 (3))
                          (push counter1 (2))
                          (push counter2 (2))
                          (push counter3 (2))
                          (call satisfies)
                          (add accum (1))
                          (pop accum (1))))
               (push accum (1)))
              (satisfies ; (satisfies c1 c2 c3)
               (call is-positive)
               (pop c1 (2))
               (call is-positive)
               (pop c2 (2))
               (call is-positive)
               (pop c3 (2))
               (pop somewhere (1))
               (mul somewhere (1))
               (pop somewhere (1))
               (mul somewhere (1))
               (pop somewhere (1))
               (push c1 (3))
               (mul c1 (3))
               (push c2 (3))
               (mul c2 (3))
               (pop x (3))
               (add x (3))
               (pop x (3))
               (push c3 (3))
               (mul c3 (3))
               (sub x (3))
               (pop x (3))
               (push somewhere (1))
               (mul x (1)))
              (negate ; (negate n)
               (pop value (2))
               (if value
                   (return 0))
               (return 1))
              (is-positive ; (is-positive n)
               (pop pos (2))
               (push 0 (2))
               (sub 1 (2))
               (pop neg (2))
               (push pos (2))
               (mul neg (2))
               (pop neg (2))
               (forever
                (push pos (2))
                (call negate)
                (pop cmp (1))
                (if cmp
                    (return 1))
                (push neg (2))
                (call negate)
                (pop cmp (1))
                (if cmp
                    (return 0))
                (push pos (1))
                (sub 1 (1))
                (pop pos (1))
                (push neg (1))
                (sub 1 (1))
                (pop neg (1)))))))

