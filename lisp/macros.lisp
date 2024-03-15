;; Define a simple macro to double its input
(defmacro double (x)
  `(* 2 ,x))

;; Test the double macro
(double 5)  ; Expands to (* 2 5) and returns 10


 
