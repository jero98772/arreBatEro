;; Define a function to add two numbers
(defun add (x y)
  (+ x y))

;; Currying: Define a curried version of the add function
(defun curried-add (x)
  (lambda (y) (+ x y)))

;; Partial Application: Define a function that partially applies the add function
(defun partial-add (x)
  (lambda (y) (add x y)))

;; Test the functions
(let ((add5 (curried-add 5))) ; Currying
  (format t "Curried add 5 + 3 = ~a~%" (funcall add5 3)))

(let ((add5 (partial-add 5))) ; Partial Application
  (format t "Partially applied add 5 + 3 = ~a~%" (funcall add5 3)))
