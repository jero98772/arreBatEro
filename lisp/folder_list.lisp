 ;; Define a simple fold function (catamorphism)
(defun foldl (f init lst)
  "Left fold (catamorphism) over a list."
  (if (null lst)
      init
      (foldl f (funcall f init (car lst)) (cdr lst))))

;; Example usage: Summing the elements of a list
(let ((list '(1 2 3 4 5)))
  (format t "Sum of list ~a: ~a~%"
          list
          (foldl #'+ 0 list)))
