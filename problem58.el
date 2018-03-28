
(require 'calc-ext)

(let ((num 0)
      (den 1)
      (v 1)
      (d 2)
      (c 4))

  (while (or (zerop num) (>= (* num 10) den))
    (setq v (+ v d))
    (setq c (1- c))
    (when (= c 0)
      (setq c 4)
      (setq d (+ d 2)))
    (setq den (1+ den))
    (when (car (math-prime-test v 2))
      (setq num (1+ num))))

  (when (= c 4)
    (setq d (- d 2)))
  (message "%d" (1+ d)))
