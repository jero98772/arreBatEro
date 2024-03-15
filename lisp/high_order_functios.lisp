;; Map function: Apply a function to each element of a list and return a new list
(defun my-map (func lst)
  (if (null lst)
      '()
      (cons (funcall func (car lst))
            (my-map func (cdr lst)))))

(defun mult3(x)( * x 566))

;; Test the map function
(write (my-map #'mult3 '(1 2 3 4)))  ; Returns (2 3 4 5)

;; Reduce function: Apply a function to each element of a list to accumulate a result
(defun my-reduce (func lst initial-value)
  (if (null lst)
      initial-value
      (funcall func (car lst) (my-reduce func (cdr lst) initial-value))))

;; Test the reduce function
(write (my-reduce #'+ '(1 2 3 4) 0)) ; Returns 10

;; Filter function: Filter elements of a list based on a predicate function
(defun my-filter (predicate lst)
  (if (null lst)
      '()
      (if (funcall predicate (car lst))
          (cons (car lst) (my-filter predicate (cdr lst)))
          (my-filter predicate (cdr lst)))))

;; Test the filter function
(write (my-filter #'evenp '(1 2 3 4 5))  ); Returns (2 4)