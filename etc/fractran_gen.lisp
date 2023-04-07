
(defvar *primes* '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271))

;; alist mapping symbols to indices in the *primes* variable. The
;; symbols can be user-defined variables or uninterned (generated)
;; symbols used for intermediate arithmetic.
(defvar *associations* (list))

(defun get-var (var-name)
  (or
   (cdr (assoc var-name *associations*))
   (prog1
       (length *associations*)
     (push (cons var-name (length *associations*)) *associations*))))

(defstruct num
  exponents)

(defun num (&rest args)
  (make-num :exponents args))

(defun one ()
  (num))

(defun nth-prime (n)
  (make-num :exponents (concatenate 'list (make-list n :initial-element 0) '(1))))

(defstruct (fraction (:constructor make-fraction-raw (numerator denominator)))
  (numerator '() :type num)
  (denominator '() :type num))

(defgeneric make-fraction (n d))

(defmethod make-fraction ((n num) (d num))
  (make-fraction-raw n d))

(defmethod make-fraction ((n list) (d list))
  (make-fraction-raw (apply #'num n) (apply #'num d)))

;; Given a number as a list of prime exponents in increasing order,
;; return a string that our FRACTRAN interpreter can read.
(defmethod print-object ((num num) stream)
  (let ((exponents (num-exponents num)))
    (flet ((factor-to-string (p n)
             (case n
               (0 "")
               (1 (format nil "~A" p))
               (t (format nil "~A^~A" p n)))))
      (if (every (lambda (x) (= x 0)) exponents)
          "1"
          (format stream "~{~A~^*~}"
                  (remove-if (lambda (s) (equal s "")) (mapcar #'factor-to-string *primes* exponents)))))))

;; Fractions are represented as cons cells of numbers.
(defmethod print-object ((frac fraction) stream)
  (format stream "~A % ~A" (fraction-numerator frac) (fraction-denominator frac)))

;; Pad on the right with zeroes
(defgeneric pad-to (len num))

(defmethod pad-to (len (xs list))
  (if (>= (length xs) len)
      xs
      (concatenate 'list xs (make-list (- len (length xs)) :initial-element 0))))

(defmethod pad-to (len (n num))
  (apply #'num (pad-to len (num-exponents n))))

(defun simplify (frac)
  (let ((numer (num-exponents (fraction-numerator frac)))
        (denom (num-exponents (fraction-denominator frac))))
    (let ((numer (pad-to (length denom) numer))
          (denom (pad-to (length numer) denom)))
      (let ((numer (mapcar (lambda (x y) (- x (min x y))) numer denom))
            (denom (mapcar (lambda (x y) (- y (min x y))) numer denom)))
        (make-fraction numer denom)))))

(defgeneric mul (a b))

(defmethod mul ((a num) (b num))
  (let ((a (pad-to (length (num-exponents b)) a))
        (b (pad-to (length (num-exponents a)) b)))
    (apply #'num (mapcar #'+ (num-exponents a) (num-exponents b)))))

(defmethod mul ((a fraction) (b fraction))
  (let ((a-n (fraction-numerator a))
        (a-d (fraction-denominator a))
        (b-n (fraction-numerator b))
        (b-d (fraction-denominator b)))
    (simplify (make-fraction (mul a-n b-n) (mul a-d b-d)))))

(defun print-fraction (frac)
  (format t "~A~%" frac))

;; Everything here is in continuation-passing style. Every function
;; takes a k parameter indicating its own state variable. Then the
;; function returns a new k parameter indicating its continuation
;; value (where it goes next).

;; If var is nil, return the number 1. If var is not nil, return the
;; prime number represented by the variable.
(defun var-value (var)
  (if var
      (nth-prime (get-var var))
      (one)))

(defun op-add (dest-var src-var my-k1)
  ;; Returns continuation arg
  (assert (not (eq dest-var src-var)))
  (let ((my-k2 (gensym "add-k2"))
        (term-k (gensym "add-term-k")))
    (let ((dest-value (var-value dest-var))
          (src-value (var-value src-var))
          (my-k1-value (var-value my-k1))
          (my-k2-value (var-value my-k2))
          (term-k-value (var-value term-k)))
      ;; Do the addition step
      (print-fraction (make-fraction (mul dest-value my-k2-value) (mul src-value my-k1-value)))
      ;; Reset our continuation state
      (print-fraction (make-fraction my-k1-value my-k2-value))
      ;; If done, pass on to k
      (print-fraction (make-fraction term-k-value my-k1-value)))
    my-k1))

(op-add 'd 'x (num 2))
;; To make setting up initial state waaaaay easier
(format *error-output* "~A~%"
        (mapcar (lambda (c) (cons (car c) (nth (cdr c) *primes*))) *associations*))
