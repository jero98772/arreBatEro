;lein new myapp
;lein uberjar
(ns myapp.core
  (:gen-class))
(defn add [a b]
  (+ a b))
