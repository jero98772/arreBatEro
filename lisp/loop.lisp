(loop for i from 1 to 5 do
      (print i))

(do ((i 10 (- i 1)))
    ((< i -5))
  (print i))

(dotimes (i 50)
  (print (1+ i)))

(dolist (item '(1 2 3 4 5))
  (print item))

