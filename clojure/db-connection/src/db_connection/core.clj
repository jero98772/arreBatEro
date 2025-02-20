(ns db-connection.core
  (:require [clojure.java.jdbc :as jdbc]  ;; Standard JDBC for SQLite/PostgreSQL
            [taoensso.carmine :as redis]  ;; Redis
            [monger.core :as mg]          ;; MongoDB
            [monger.collection :as mc])
  (:import [java.sql DriverManager])
  (:gen-class))

;; 🔹 SQLite Connection
(def sqlite-db "jdbc:sqlite:database.sqlite")

(defn sqlite-test []
  (with-open [conn (DriverManager/getConnection sqlite-db)]
    (let [stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)")
      (.execute stmt "INSERT INTO users (name) VALUES ('Alice')")
      (let [rs (.executeQuery stmt "SELECT * FROM users")]
        (while (.next rs)
          (println "SQLite User:" (.getString rs "name")))))))

;; 🔹 Redis Connection
(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379"}})

(defn redis-test []
  (redis/wcar redis-conn (redis/set "greeting" "Hello from Redis!"))
  (println "Redis Greeting:" (redis/wcar redis-conn (redis/get "greeting"))))

;; 🔹 MongoDB Connection
(def mongo-conn (mg/connect))
(def mongo-db (mg/get-db mongo-conn "mydb"))

(defn mongo-test []
  (mc/insert mongo-db "users" {:name "Bob" :age 25})
  (println "MongoDB Users:" (mc/find-maps mongo-db "users")))

;; 🔹 PostgreSQL Connection
(def pg-db "jdbc:postgresql://localhost:5432/mydb?user=myuser&password=mypassword")

(defn postgres-test []
  (with-open [conn (DriverManager/getConnection pg-db)]
    (let [stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT)")
      (.execute stmt "INSERT INTO users (name) VALUES ('Charlie')")
      (let [rs (.executeQuery stmt "SELECT * FROM users")]
        (while (.next rs)
          (println "PostgreSQL User:" (.getString rs "name")))))))

(defn -main []
  (println "Testing databases...")
  (sqlite-test)
  (redis-test)
  (mongo-test)
  (postgres-test)
  (println "All tests completed!"))
