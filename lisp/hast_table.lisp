;; Define a hash table with symbols as keys and integers as values
(defvar *hash-table* (make-hash-table))

;; Add some key-value pairs to the hash table
(setf (gethash 'apple *hash-table*) 5)
(setf (gethash 'banana *hash-table*) 7)
(setf (gethash 'orange *hash-table*) 9)

;; Retrieve values from the hash table
(format t "Value for apple: ~a~%" (gethash 'apple *hash-table*))    ; Output: Value for apple: 5
(format t "Value for banana: ~a~%" (gethash 'banana *hash-table*))  ; Output: Value for banana: 7
(format t "Value for orange: ~a~%" (gethash 'orange *hash-table*))  ; Output: Value for orange: 9
 
