
; Slots are numbered from 1 to an arbitrary positive integer cap.
; Slots 0-4 are valid but are used for miscellaneous temporary variable storage purposes
; When moving, adding, or setting, make sure the source and destination are not the same
; Valid arguments:
;  (n) - The value in the slot n
;  n - A free integer, must be nonnegative
; Instructions:
;  (set slotn val) - Set var
;  (add slotn val) - Add var
;  (down slotn)
;  (up slotn)
;  (not slotn)
;  (when val &rest stmts)
;  (times slotn &rest stmts) - Do until the slot is zero
;  (begin &rest stmts)
;  (lt slot1 slot2) - Stores result in slot1
;  (print slot1) - Clear out the slots after slot1; VERY EXPERIMENTAL
;  (inject string) - For debug purposes, inject the string
;  (to slot1) - Move to the slot but don't do anything
; Note that case must be taken to keep the seek pointer stable. That is, inside of any loop or
; conditional bodies, the seek should start and end at the same position

(defparameter *seek-position*
  0)

(defparameter *instructions*
  '((set slot value)
    (add slot value)
    (sub slot value)))

(defgeneric translate-stmt (head &rest args))

(defun seek-to (n)
  (check-type n (integer 0 *))
  (princ (make-string (abs (- *seek-position* n))
                      :initial-element (if (< *seek-position* n)
                                           #\>
                                           #\<)))
  (setq *seek-position* n))

(defun seek-pos ()
  *seek-position*)

(defmacro save-seek-pos (&body body)
  (let ((init (gensym)))
    `(let ((,init (seek-pos)))
       (prog1 (progn ,@body)
         (seek-to ,init)))))

(defmacro do-loop (&body body)
  `(prog2
       (princ "[")
       (progn ,@body)
     (princ "]")))

(defmacro do-if (&body body)
  `(do-loop
     (prog1
         (save-seek-pos
           ,@body)
       (zero))))

(defun up ()
  (princ "+"))

(defun down ()
  (princ "-"))

(defun zero ()
  (do-loop (down)))

(defun set-value (n)
  (check-type n (integer 0 *))
  (zero)
  (princ (make-string n :initial-element #\+))
  n)

(defun add-value (n)
  (check-type n (integer 0 *))
  (princ (make-string n :initial-element #\+))
  n)

(defun move-value (slot0 slot1)
  (check-type slot0 (integer 0 *))
  (check-type slot1 (integer 0 *))
  (save-seek-pos
    (seek-to slot1)
    (zero)
    (seek-to slot0)
    (do-loop
      (down)
      (seek-to slot1)
      (up)
      (seek-to slot0))))

(defun copy-value (slot0 slot1)
  (check-type slot0 (integer 0 *))
  (check-type slot1 (integer 0 *))
  (save-seek-pos
    (seek-to slot1)
    (zero)
    (seek-to 0)
    (zero)
    (seek-to slot0)
    (do-loop
      (down)
      (seek-to slot1)
      (up)
      (seek-to 0)
      (up)
      (seek-to slot0))
    (move-value 0 slot0)))

(defun add-value-to-slot (slot0 slot1)
  (check-type slot0 (integer 0 *))
  (check-type slot1 (integer 0 *))
  (save-seek-pos
    (copy-value slot0 1)
    (seek-to 1)
    (do-loop
      (down)
      (seek-to slot1)
      (up)
      (seek-to 1))))

(defun translate (program)
  (with-output-to-string (*standard-output*)
    (loop with *seek-position* = 0
          for stmt in program
          do (apply #'translate-stmt stmt))))

(defmethod translate-stmt ((head (eql 'set)) &rest args)
  (destructuring-bind (slot value) args
    (etypecase value
      ((integer 0 *)
       (seek-to slot)
       (set-value value))
      (list
       (copy-value (first value) slot)))))

(defmethod translate-stmt ((head (eql 'add)) &rest args)
  (destructuring-bind (slot value) args
    (etypecase value
      ((integer 0 *)
       (seek-to slot)
       (add-value value))
      (list
       (add-value-to-slot (first value) slot)))))

(defmethod translate-stmt ((head (eql 'not)) &rest args)
  (destructuring-bind (slot) args
    (seek-to 0)
    (set-value 1)
    (seek-to slot)
    (do-if
      (seek-to 0)
      (zero)
      (seek-to slot))
    (move-value 0 slot)))

(defmethod translate-stmt ((head (eql 'when)) &rest args)
  (destructuring-bind (value . stmts) args
    (etypecase value
      ((integer 0 *)
       (unless (zerop value)
         (mapc #'translate-stmt stmts)))
      (list
       (seek-to (first value))
       (do-if
         (mapc (lambda (x) (apply #'translate-stmt x)) stmts)
         (seek-to (first value)))))))

(defmethod translate-stmt ((head (eql 'times)) &rest args)
  (destructuring-bind (slot . stmts) args
    (seek-to slot)
    (do-loop
      (mapc (lambda (x) (apply #'translate-stmt x)) stmts)
      (seek-to slot)
      (down))))

(defmethod translate-stmt ((head (eql 'begin)) &rest args)
  (mapc #'translate-stmt args))

(defmethod translate-stmt ((head (eql 'lt)) &rest args)
  (destructuring-bind (slot1 slot2) args
    (copy-value slot2 0)
    (seek-to 1)
    (set-value 1)
    (seek-to slot1)
    (do-loop
      (down)
      (seek-to 0)
      (down)
      (do-if
        (seek-to slot1)
        (zero)
        (seek-to 1)
        (zero)
        (seek-to 0))
      (seek-to slot1))
    (move-value 1 slot1)))

(defmethod translate-stmt ((head (eql 'down)) &rest args)
  (destructuring-bind (slot) args
    (seek-to slot)
    (down)))

(defmethod translate-stmt ((head (eql 'up)) &rest args)
  (destructuring-bind (slot) args
    (seek-to slot)
    (up)))

(defmethod translate-stmt ((head (eql 'print)) &rest args)
  (destructuring-bind (slot1) args
    (seek-to slot1)
    ; Taken from https://esolangs.org/wiki/Brainfuck_algorithms#Print_value_of_cell_x_as_number_.288-bit.29
    (princ "[>>+>+<<<-]>>>[<<<+>>>-]<<+>[<->[>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]++++++++[<++++++>-]>[<<+>>-]>[<<+>>-]<<]>]<[->>++++++++[<++++++>-]]<[.[-]<]<")))

(defmethod translate-stmt ((head (eql 'inject)) &rest args)
  (destructuring-bind (str) args
    (princ str)))

(defmethod translate-stmt ((head (eql 'to)) &rest args)
  (destructuring-bind (slot) args
    (seek-to slot)))

(defun run ()
  (translate
   (let ((accum 7)
         (z0 10)
         (z1 11)
         (z2 12)
         (counter 15)
         (when-even 20)
         (when-even-temp 21)
         (big 40))
     `((set ,accum 0)
       (set ,z0 0)
       (set ,z1 1)
       (set ,z2 1)
       (set ,counter 33)
       (set ,when-even 1)
       (times ,counter
              (set ,z0 (,z1))
              (set ,z1 (,z2))
              (add ,z2 (,z0))
              (down ,when-even)
              (set ,when-even-temp (,when-even))
              (not ,when-even-temp)
              (when (,when-even-temp)
                (add ,accum (,z2))
                (set ,when-even 3)))
       (set ,big (,accum))
       (print ,big)))))
