(defun match-pattern (pattern data)
  "Match the PATTERN against the DATA and return T if successful."
  (cond
    ((null pattern) (null data)) ; If pattern is empty, data should also be empty
    ((and (consp pattern) (consp data)) ; If both pattern and data are lists
     (and (match-pattern (car pattern) (car data))
          (match-pattern (cdr pattern) (cdr data))))
    ((eql pattern data) t)      ; If pattern and data are atoms and equal
    (t nil)))                   ; Otherwise, pattern does not match data

;; Example usage
(let ((pattern '(123))       ; Pattern to match: (1 2 _)
      (data '(123)))         ; Data to match against: (1 2 3)
  (if (match-pattern pattern data)
      (format t "Pattern matches!~%")
      (format t "Pattern does not match.~%")))