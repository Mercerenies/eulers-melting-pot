
;; Generator for Pickle protocol 0 opcodes

(defmacro define-opcode (name code argc)
  (let ((args (gensym)))
    `(defun ,name (&rest ,args)
       (unless (= (length ,args) ,argc)
         (error "Wrong number of arguments to ~S, got ~S expected ~S" ,name (length ,args) ,argc))
       (format nil "~A~{~A~%~}" ,code ,args))))

(define-opcode int "I" 1)
(define-opcode long "L" 1)
(define-opcode str "S" 1) ; (!!!) Don't forget to escape
(define-opcode none "N" 0)
(define-opcode unicode "V" 1)
(define-opcode ofloat "F" 1)
(define-opcode oappend "a" 0)
(define-opcode olist "l" 0)
(define-opcode tuple "t" 0)
(define-opcode dict "d" 0)
(define-opcode setitem "s" 0)
(define-opcode opop "0" 0)
(define-opcode dup "2" 0)
(define-opcode mark "(" 0)
(define-opcode oget "g" 1)
(define-opcode put "p" 1)
(define-opcode global "c" 2)
(define-opcode oreduce "R" 0)
(define-opcode build "b" 0)
(define-opcode inst "i" 2)
(define-opcode stop "." 0)
(define-opcode persid "P" 1)

(defun nop ()
  "")

(defun flatten (arg)
  (typecase arg
    (cons (mapcan #'flatten arg))
    (null nil)
    (t (list arg))))

(defun oprogn (&rest args)
  (format nil "~{~A~}"
          (flatten args)))

(defun escape (s)
  (format nil "~{~A~}"
          (loop for ch across s
                collect (if (and (<= 32 (char-code ch) 128) (not (eql ch #\\)))
                            (string ch)
                            (format nil "\\u~4,'0X" (char-code ch))))))

(defun mktuple (&rest args)
  (oprogn (mark)
          args
          (tuple)))

(defun call (fn &rest args)
  (oprogn fn
          (apply #'mktuple args)
          (oreduce)))

(defparameter *mul* (global "operator" "mul"))
(defparameter *add* (global "operator" "add"))
(defparameter *lt* (global "operator" "lt"))
(defparameter *print* (global "builtins" "print"))
(defparameter *setattr* (global "builtins" "setattr"))
(defparameter *getattr* (global "builtins" "getattr"))
(defparameter *bool* (global "builtins" "bool"))
(defparameter *true* (call *bool* (int 1)))
(defparameter *false* (call *bool* (int 0)))

(defparameter *memo-if* 100)

(defun bytes (literal)
  "Construct a Python bytes object"
  (call (global "builtins" "bytes")
        (unicode literal)
        (unicode "UTF-8")))

(defun do-branch (cnd true false)
  "Helper for if statements. You probably meant to call if-stmt directly."
  (call *add*
        (call *mul*
              (oprogn cnd
                      (put *memo-if*)
                      true))
        (call *mul*
              (oprogn false
                      (call (global "operator" "not_")
                            (oget *memo-if*))))))

(defun oif (cnd true false)
  (let ((true (bytes (escape (oprogn true (stop)))))
        (false (bytes (escape (oprogn false (stop))))))
    (call (global "pickle" "loads")
          (do-branch cnd true false))))

(defun oif-novalue (cnd true false)
  (oprogn (oif cnd
               (oprogn true (none))
               (oprogn false (none)))
          (opop)))

(defun do-while (name cnd &rest body)
  (oprogn (declare-block name
                         body
                         (oif-novalue cnd
                                      (call-block name)
                                      (nop))
                         (none))
          (call-block name)
          (opop)))

(defun while (name cnd &rest body)
  (oif-novalue cnd
               (do-while name cnd body)
               (nop)))

(defun declare-block (name &rest body)
  (set-global name
              (bytes (escape (oprogn body (stop))))))

(defun call-block (name)
  (call (global "pickle" "loads")
        (get-global name)))

(defun set-global (name value &key (pop t))
  (oprogn (global "builtins" "__dict__")
          (unicode name)
          value
          (setitem)
          (if pop (opop) (nop))))

(defun get-global (name)
  (global "builtins" name))

(defun for (name var-name lower upper &rest body)
  (oprogn (set-global var-name lower)
          (while name (call *lt* (get-global var-name) upper)
                 body
                 (set-global var-name (call *add* (get-global var-name) (int 1))))))

(defun oprint (value)
  (call *print* value))

(defun getattr (obj key)
  (call *getattr* obj key))

(defun list-setitem (list index value)
  (call (getattr (global "builtins" "list") (unicode "__setitem__"))
        index
        value))

;; (let ((*pprint-first-newline* nil))
;;   (format t "~@{~A~}~%"
;;           (oif (int 0)
;;                (oprogn (call *print* (int 42)))
;;                (oprogn (call *print* (int 99))))
;;           (call *print* (unicode "Done."))
;;           (opop)
;;           (set-global "foobar" (unicode "This is the value of foobar"))
;;           (call *print*
;;                 (get-global "foobar"))
;;           (opop)
;;           (set-global "i" (int 0))
;;           (while "sample_loop" (call (global "operator" "lt") (get-global "i") (int 10))
;;                  (call *print* (get-global "i"))
;;                  (opop)
;;                  (set-global "i" (call *add* (get-global "i") (int 1))))
;;           (stop)))

(let ((*pprint-first-newline* nil))
  (format t "~@{~A~}~%"
          (set-global "sieve" (oprogn (mark) (olist)))
          (set-global "rads" (oprogn (mark) (olist)))
          (for "__for_loop" "i" (int 0) (int 120000)
               (oprint (get-global "i")))
          (oprint (get-global "sieve"))))
