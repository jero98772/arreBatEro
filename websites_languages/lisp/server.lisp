(in-package :my-server)

(define-easy-handler (hello :uri "/") ()
  "Hello, World!")

(defvar *acceptor* nil) ; Declare a global variable for the server.

(defun start-server ()
  (setf *acceptor* (start (make-instance 'easy-acceptor :port 8080)))
  (format t "Server started at http://localhost:8080~%"))

(defun stop-server ()
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped.~%")))
