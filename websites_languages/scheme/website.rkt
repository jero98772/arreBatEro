#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (start request)
  (response/xexpr
   '(html
     (head (title "My Scheme Website"))
     (body
      (h1 "Welcome to My Scheme Website")
      (p "This is a simple website built with Racket and Scheme!")))))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port 8080)
