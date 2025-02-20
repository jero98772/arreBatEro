(ns mongo-test.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]))

(def mongo-conn (mg/connect))
(def mongo-db (mg/get-db mongo-conn "testdb"))

(defn test-mongo []
  (mc/insert mongo-db "users" {:name "MongoUser" :age 30})
  (println "MongoDB Users:" (mc/find-maps mongo-db "users")))

(defn -main []
  (println "Running MongoDB Test...")
  (test-mongo)
  (println "MongoDB Test Completed!"))
