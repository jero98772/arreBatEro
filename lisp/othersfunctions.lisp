(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Test the factorial function
(write (factorial 5))  ; Computes factorial of 5

(defun fibonacci (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Test the fibonacci function


;; Memoization table to store computed Fibonacci numbers
(defvar *fib-table* (make-hash-table))

(defun fibonaccidp (n)
  (if (<= n 1)
      n
      (let ((memoized-value (gethash n *fib-table*)))
        (if memoized-value
            memoized-value
            (let ((fib-value (+ (fibonaccidp (- n 1)) (fibonaccidp (- n 2)))))
              (setf (gethash n *fib-table*) fib-value)
              fib-value)))))


;; Example usage
(terpri)

(format t "Fibonacci of ~d is ~d~%" 5 (fibonaccidp 30))

(terpri)

(write (fibonacci 20))


(defun is-prime (n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (loop for i from 3 to (sqrt n) by 2
                 until (zerop (mod n i))
                 finally (return (zerop (mod n i)))))))

;; Test the is-prime function
(write (is-prime 11))  