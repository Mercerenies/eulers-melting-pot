
;;; Beeswax code generator

(defgeneric to-code (code))

;; beeswax-code is a wrapper around a 2-dimensional array that
;; provides automatic adjustment to the size when needed.

(defclass beeswax-code ()
  ((raw-code :accessor raw-code
             :initform (make-array '(0 0)
                                   :element-type 'standard-char
                                   :initial-element #\SPACE
                                   :adjustable t))))

;; Note: If we use :element-type 'standard-char here, Common Lisp says
;; something silly like "don't allocate 2GB arrays" and then crashes.
;; So we use boxed arrays to avoid those nonsense warnings :P

(defun make-code ()
  (make-instance 'beeswax-code))

(defun get-char (code y x)
  (with-accessors ((raw-code raw-code)) code
    (if (array-in-bounds-p raw-code y x)
        (aref raw-code y x)
        #\SPACE)))

(defun (setf get-char) (new-value code y x)
  (with-accessors ((raw-code raw-code)) code
    (unless (array-in-bounds-p raw-code y x)
      (destructuring-bind (h w) (array-dimensions raw-code)
        (let ((h (max h (1+ y)))
              (w (max w (1+ x))))
          (adjust-array raw-code (list h w) :initial-element #\space))))
    (setf (aref raw-code y x) new-value)))

(defun dims (code)
  (array-dimensions (raw-code code)))

(defmethod to-code ((code beeswax-code))
  (destructuring-bind (h w) (dims code)
    (format t "~{~{~A~}~%~}"
            (loop for y from 0 below h
                  collect (loop for x from 0 below w
                                collect (get-char code y x))))))

;; beeswax-program is a higher-level abstraction that's used to
;; generate the actual code.

(defconstant +local-var-row0+ 0)
(defconstant +local-var-row1+ 1)
(defconstant +gray-row0+      2)
(defconstant +gray-row1+      3)
(defconstant +red-row0+       4)
(defconstant +red-row1+       5)
(defconstant +code-row+       6)

(defclass beeswax-program ()
  ((code :accessor code
         :initform (make-code))
   (pointer :accessor pointer
            :initform 0)))

(defun make-program ()
  (make-instance 'beeswax-program))

(defparameter *program* (make-program))

;; Returns the new program pointer, which points to (0-indexed) one
;; after the end or (1-indexed) the end of the string just added.
(defun write-code (string &key (program *program*))
  (with-accessors ((code code) (pointer pointer)) program
    (loop for ch across string
          do (progn (setf (get-char code +code-row+ pointer) ch)
                    (incf pointer)))
    pointer))

;; Meant to be used for low numbers (Maybe up to 15 or so)
(defun produce-number (n)
  (etypecase n
    (symbol (produce-number (ecase n
                              (local0 (1+ +local-var-row0+))
                              (local1 (1+ +local-var-row1+))
                              (gray0 (1+ +gray-row0+))
                              (gray1 (1+ +gray-row1+))
                              (red0 (1+ +red-row0+))
                              (red1 (1+ +red-row1+))
                              (code (1+ +code-row+)))))
    (number (if (< n 10)
                (format nil "~A" n)
                (format nil "~AP" (produce-number (1- n)))))))

(defmethod to-code ((program beeswax-program))
  (to-code (code program)))

;; Actual commands
(defun raw (s)
  (write-code s))

(defun 2^31 ()
  (raw (format nil "~A~~~AB" (produce-number 31) (produce-number 2))))

(defun unpack () ; Unpacks lstack[1] into lstack[1,2] (mod in 2, div in 1)
  (raw "f@")
  (2^31)
  (raw "~@%")
  (set-value 'local0 4)
  (raw "g?@")
  (2^31)
  (raw "~@:f")
  (get-value 'local0 4)
  (raw "~g?"))

(defun pack () ; Packs lstack[1,2] into lstack[1]
  (raw "~f~f")
  (2^31)
  (raw "~g?.~g?+"))

(defun get-value (y x)
  (let ((y (produce-number y))
        (x (produce-number x)))
    (write-code (format nil "~A~~~A@G" y x))))

(defun set-value (y x) ; Sets to top of lstack
  (let ((y (produce-number y))
        (x (produce-number x)))
    (write-code (format nil "~~~A~~@~A@D" y x))))

(defun get-at-index (y) ; Assumes x is in top of lstack
  (let ((y (produce-number y)))
    (write-code (format nil "@~A~~G" y))))

(defun set-at-index (y) ; Assumes value is in top of lstack, x is third on lstack
  (let ((y (produce-number y)))
    (write-code (format nil "~~~A~~D" y))))

(defun get-at-index-1 (y)
  (let ((y0 (ecase y (gray 'gray0) (red 'red0)))
        (y1 (ecase y (gray 'gray1) (red 'red1))))
    (raw "f")
    (get-at-index y0) ; Modulo value
    (set-value 'local0 4)
    (raw "g?")
    (get-at-index y1) ; Div value
    (raw "f")
    (get-value 'local0 4)
    (raw "~g?")
    (pack)))

(defun set-at-index-1 (y)
  (let ((y0 (ecase y (gray 'gray0) (red 'red0)))
        (y1 (ecase y (gray 'gray1) (red 'red1))))
    (raw "@f@")
    (unpack)
    (raw "f~")
    (set-value 'local0 5) ; Modulo value
    (raw "g?")
    (raw "@g@")
    (set-at-index y1)
    (get-value 'local0 5)
    (raw "@g@")
    (set-at-index y0)
    (raw "?")))

(defun get-local-packed (x)
  (get-value 'local0 x) ; Modulo value
  (raw "f")
  (get-value 'local1 x) ; Div value
  (raw "~g?~")
  (pack))

(defun set-local-packed (x)
  (unpack)
  (raw "f~")
  (set-value 'local0 5) ; Modulo value
  (raw "g?")
  (set-value 'local1 x)
  (get-value 'local0 5)
  (set-value 'local0 x))

(defun nop () ; Need these wherever we're going to land from a goto.
  (write-code ">"))

(defun ll ()
  (write-code "p"))

(defun lr ()
  (write-code "q"))

(defun reflect (a b) ; Specify two points and we'll create a reflect
  (if (< b a)        ; position between them.
      (reflect b a)
      (let ((x (1- b)) ; 1-indexed argument to 0-indexed array
            (y (+ +code-row+ (- b a))))
        (setf (get-char (code *program*) y x) #\u))))

(defun condition-form (form)
  (ecase form
    (a>0 (raw "\""))
    (a<=0 (raw "\"Q"))
    (a=0 (raw "'"))
    (a!=0 (raw "'Q"))
    (a=b (raw "K"))
    (a!=b (raw "KQ"))
    (a>b (raw "L"))
    (a<=b (raw "LQ"))))

(defmacro if-stmt (form &body body)
  (let ((jump-point-a (gensym))
        (jump-point-b (gensym)))
    `(progn (condition-form ',form)
            (let ((,jump-point-a (lr)))
              ,@body
              (let ((,jump-point-b (nop)))
                (reflect ,jump-point-a ,jump-point-b))))))

(defmacro do-while-stmt (form &body body)
  (let ((jump-point-a (gensym))
        (jump-point-b (gensym)))
    `(let ((,jump-point-a (nop)))
       ,@body
       (condition-form ',form)
       (raw "Q")
       (let ((,jump-point-b (ll)))
         (reflect ,jump-point-a ,jump-point-b)))))

(defmacro while-stmt (form &body body)
  `(if-stmt ,form
     (do-while-stmt ,form
       ,@body)))

;; Locals: i, j, g/r, packing1, packing2

(raw "_") ; Program start

(raw "1") (set-value 'gray0 1)
(raw "0") (set-value 'gray1 1)
(raw "1") (set-value 'red0 1)
(raw "0") (set-value 'red1 1)
(raw "2") (set-value 'local0 1)

(raw "2") (set-value 'local0 1)
(do-while-stmt a<=b

  ;; g calculation
  (raw "1") (set-value 'local0 2)
  (raw "1") (set-local-packed 3)
  (get-value 'local0 2) (raw "PPf") (get-value 'local0 1) (raw "~g?~") ; j + 2 < i
  (while-stmt a>b
    (get-local-packed 3)
    (raw "f")
    (get-value 'local0 2)
    (get-at-index-1 'red)
    (raw "~g?+")
    (set-local-packed 3)
    (get-value 'local0 2) (raw "P") (set-value 'local0 2) ; j += 1
    (get-value 'local0 2) (raw "PPf") (get-value 'local0 1) (raw "~g?~")) ; j + 2 < i
  (get-local-packed 3)
  (raw "f")
  (get-value 'local0 1)
  (raw "@g?")
  (set-at-index-1 'gray)

  ;; r calculation
  (raw "1") (set-value 'local0 2)
  (raw "1") (set-local-packed 3)
  (get-value 'local0 2) (raw "f") (get-value 'local0 1) (raw "~g?~") ; j < i
  (while-stmt a>b
    (get-local-packed 3)
    (raw "f")
    (get-value 'local0 2)
    (get-at-index-1 'gray)
    (raw "~g?+")
    (set-local-packed 3)
    (get-value 'local0 2) (raw "P") (set-value 'local0 2) ; j += 1
    (get-value 'local0 2) (raw "f") (get-value 'local0 1) (raw "~g?~")) ; j < i
  (get-local-packed 3)
  (raw "f")
  (get-value 'local0 1)
  (raw "@g?")
  (set-at-index-1 'red)

  (get-value 'local0 1) (raw "P") (set-value 'local0 1) ; i += 1
  (raw "f5F.~2.P~g?")) ; i <= 51
(raw "5F.~2.P") ; 51
(get-at-index-1 'gray)
(raw "{")

(to-code *program*)
