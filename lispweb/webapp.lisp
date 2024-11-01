;; Define a package for our web application
(defpackage :my-web-app
  (:use :cl :hunchentoot))
(in-package :my-web-app)

;; Define a simple handler function that responds with "Hello, World!"
(defun hello-world-handler (request)
  (setf (hunchentoot:content-type*) "text/html")
  "<h1>Hello, World!</h1>")

;; Set up the server to use our handler for the root URL "/"
(hunchentoot:define-easy-handler (hello :uri "/") ()
  (hello-world-handler))

;; Start the server on port 8080
(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8089)))
