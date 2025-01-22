(ns my-website.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [hiccup.page :refer [html5]]))

(defroutes app-routes
  (GET "/" [] 
       (html5
        [:head
         [:title "My Website"]]
        [:body
         [:h1 "Welcome to my website!"] 
         [:p "This is a simple website built in Clojure using Ring and Compojure."]]))
  
  (route/not-found "Page not found"))

(defn -main []
  (jetty/run-jetty app-routes {:port 3000}))
