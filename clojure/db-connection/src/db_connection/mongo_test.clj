(ns db-connection.mongo-test
  (:require [monger.core :as mg]
            [monger.collection :as mc])
  (:gen-class))

(defn test-mongo []
  (let [mongo-conn (mg/connect)
        mongo-db (mg/get-db mongo-conn "mydb")]
    (mc/insert mongo-db "users" {:name "Bob" :age 25})
    (println "MongoDB Users:" (mc/find-maps mongo-db "users"))))

(defn -main []
  (println "Testing MongoDB...")
  (test-mongo)
  (println "MongoDB test completed!"))
