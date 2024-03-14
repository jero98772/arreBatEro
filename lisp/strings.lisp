
(defun count-occurrences (char str)
  (loop for c across str
        count (char= c char)))

;; Test the count-occurrences function
(write (count-occurrences #\l "hello")  ); Returns 2



(defun palindrome-p (str)
  (string= str (reverse str)))

;; Test the palindrome-p function
(write (palindrome-p "racecar")  ); Returns T