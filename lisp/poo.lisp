
;; Define a simple class Person
(defclass person ()
  ((name :initarg :name :accessor name)
   (age :initarg :age :accessor age)))

;; Define a method to greet a person
(defmethod greet ((p person))
  (format t "Hello, my name is ~a and I am ~d years old.~%" (name p) (age p)))

;; Create an instance of Person
(defparameter p1 (make-instance 'person :name "Alice" :age 30))

;; Call the greet method
(greet p1)  ; Outputs "Hello, my name is Alice and I am 30 years old." 
