(defun write-file (file-path data)
  (with-open-file (stream file-path
                           :direction :output
                           :if-exists :supersede)
    (dolist (line data)
      (princ line stream))))

;; Test the write-file function
(write-file "output.txt" '("line 1" "line 2" "line 3"))  ; Writes lines to "output.txt"


(defun read-file (file-path)
  (with-open-file (stream file-path)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

;; Test the read-file function
(write (read-file "output.txt"))  ; Reads lines from "example.txt"