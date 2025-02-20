(ns db-connection.redis-test
  (:require [taoensso.carmine :as redis])
  (:gen-class))

(def redis-conn {:pool {} :spec {:uri "redis://localhost:6379"}})

(defn test-redis []
  (redis/wcar redis-conn (redis/set "greeting" "Hello from Redis!"))
  (println "Redis Greeting:" (redis/wcar redis-conn (redis/get "greeting"))))

(defn -main []
  (println "Testing Redis...")
  (test-redis)
  (println "Redis test completed!"))
