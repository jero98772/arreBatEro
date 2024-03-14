;; Functions to manipulate lists
(defun my-reverse (lst)
  (if (null lst)
      '()
      (append (my-reverse (cdr lst)) (list (car lst)))))  ; Reverses a list

(defun my-length (lst)
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))  ; Computes the length of a list

(defun ds ()
;; Creating lists
  (setq numbers '(1 2 3 4 5 6 7 8))  ; List of numbers
  (setq strings '("apple" "banana" "cherry"))  ; List of strings
  (setq mixed '(1 "apple" 3.14))  ; List of mixed types
  (write numbers)
  (terpri)
  (write strings)
  (terpri)
  (write mixed)
  (terpri)
  (write (car numbers))
  (terpri)
  (write (cdr numbers))
  (terpri)
  (write (first numbers))
  (terpri)
  (write (rest numbers))
  (terpri)
  (write (nth 1 numbers))
  (terpri)
  (write (nth 2 numbers))
  (terpri)
  (write (my-reverse numbers))
  (terpri)
  (write (my-length numbers))


)
(ds)