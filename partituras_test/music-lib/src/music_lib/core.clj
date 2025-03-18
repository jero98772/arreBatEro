(ns music-lib.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [taoensso.carmine :as car]
            [music-lib.db :refer [create! read! update! delete! get-all-keys count-keys
                                 create-edn! read-edn!]]))

;; Helper function to serve HTML with correct content type
(defn html-response [resource-path]
  (-> (response/resource-response resource-path {:root "public"})
      (response/content-type "text/html; charset=utf-8")))

(defroutes app-routes
  ;; Main routes with proper content type
  (GET "/" [] {:status 200
               :headers {"Content-Type" "text/html"}
               :body (slurp (clojure.java.io/resource "public/index.html"))})
  (GET "/add-partiture" [] (html-response "add-partiture.html"))

		(GET "/api/partiture" []
		  (let [partiture-keys (get-all-keys "partiture:*")  ; Renamed from 'keys' to avoid shadowing
		        partiture-count (count partiture-keys)
		        partiture-list (map #(read-edn! %) partiture-keys)]
		    (println "partiture with data:" partiture-list)  ; Now prints the actual data
		    {:status 200
		     :headers {"Content-Type" "application/json"}
		     :body (json/generate-string {:count partiture-count :partitureList partiture-list})}))

		(POST "/api/partiture" {params :params}
		  (try
		    (let [id (str "partiture:" (java.util.UUID/randomUUID))
		          data (assoc params 
		                     :id id
		                     :created-at (str (java.util.Date.)))]
		      (println "Creating partiture with data:" data) ;; Add this for debugging
		      (create-edn! id data)
		      {:status 201
		       :headers {"Content-Type" "application/json"}
		       :body (json/generate-string {:id id})})
		    
		    (catch Exception e
		      (println "Error creating partiture:" (ex-message e))
		      {:status 500
		       :headers {"Content-Type" "application/json"}
		       :body (json/generate-string {:error (str "Could not create partiture: " (ex-message e))})})))
  
(GET "/api/partiture/:id" [id]
   (println "Fetching partiture:" id) 
  (let [key (str "partiture:" id)  ;; Ensure key format
        data (read-edn! key)]      ;; Fetch data
    (if data
      (do
        (println "Retrieved partiture:" key "->" data)  ;; Log data in the terminal
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string data)})  ;; Convert EDN to JSON
      (do
        (println "Partiture not found for ID:" id)  ;; Log error in the terminal
        {:status 404
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:error "Partiture not found"})}))))
  
  (PUT "/api/partiture/:id" {params :params}
       (let [id (:id params)
             existing (read-edn! id)]
         (if existing
           (do
             (update! id (pr-str (merge existing params)))
             {:status 200
              :headers {"Content-Type" "application/json"}
              :body "{\"status\": \"updated\"}"})
           {:status 404
            :headers {"Content-Type" "application/json"}
            :body "{\"error\": \"Partiture not found\""})))
  
  (DELETE "/api/partiture/:id" [id]
          (let [result (delete! id)]
            (if (pos? result)
              {:status 200
               :headers {"Content-Type" "application/json"}
               :body "{\"status\": \"deleted\"}"}
              {:status 404
               :headers {"Content-Type" "application/json"}
               :body "{\"error\": \"Partiture not found\""})))
  


  
  ;; Static html routes with proper content type
  (GET "/edit/:id" [id] (html-response "edit.html"))
  
  (GET "/delete/:id" [id] (html-response "delete.html"))
  
  (GET "/all" [] (html-response "all.html"))
  
  ;; Serve static assets
  (route/resources "/")
  
  ;; Catch-all route for 404 errors
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-defaults (-> site-defaults
                        (assoc-in [:security :anti-forgery] false)
                        (assoc-in [:responses :content-types] true)))))