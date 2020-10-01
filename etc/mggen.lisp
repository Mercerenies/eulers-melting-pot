
;; MagiStack has general access to any stack element, but the
;; functionality is not readily exposed. So that's what I'm doing
;; here.

;; (cp from-bottom)
;; (mv from-bottom)

(defun translate-command (cmd)
  (etypecase cmd
    (string cmd)
    (symbol (string cmd))
    (list (ecase (first cmd)
            (cp (let ((idx (second cmd)))
                  (apply #'concatenate 'string
                         (append
                          (loop repeat idx collect ";")
                          '("~:")
                          (loop repeat idx collect ";\\")
                          '("~;")))))
            (mv (let ((idx (second cmd)))
                  (apply #'concatenate 'string
                         (append
                          (loop repeat idx collect ";")
                          '("~")
                          (loop repeat idx collect ";\\")
                          '("~;")))))
            (block (translate-commands (rest cmd)))))))

(defun translate-commands (cmds)
  (apply #'concatenate 'string
         (loop for cmd in cmds
               collect (translate-command cmd))))

(format t "~A~%"
        (let ((x1  '(cp 2))
              (y1  '(cp 3))
              (x2  '(cp 4))
              (y2  '(cp 5))
              (x3  '(cp 6))
              (y3  '(cp 7))
              (tri '(cp 8))
              (u   '(cp 9))
              (v   '(cp 10))
              (w   '(cp 11)))
          (translate-commands
           `("0 0"
             "|"
             (block
               "^^^^^^"
               ,y2 ,y3 - ,x1 ,x3 - * ,x3 ,x2 - ,y1 ,y3 - * +
               ,y2 ,y3 - "0" ,x3 - * ,x3 ,x2 - "0" ,y3 - * +
               ,y3 ,y1 - "0" ,x3 - * ,x1 ,x3 - "0" ,y3 - * +
               ,tri ,u - ,v -
               "0" ,tri "0\\`" - "2" * "1" +
               ":" (mv 8) "*\\"
               ":" (mv 8) "*\\"
               ":" (mv 8) "*\\"
               ":" (mv 8) "*\\"
               "$"
               "0" ,u "`!"
               ,u ,tri "`!*"
               "0" ,v "`!*"
               ,v ,tri "`!*"
               "0" ,w "`!*"
               ,w ,tri "`!*"
               "~;+~"
               "$$$$$$$$$$"
               "1+"
               ":91+::**\\`")
             "1=<"
             "$.91+,"))))
