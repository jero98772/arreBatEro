(ns db-connection.postgres-test
  (:import [java.sql DriverManager])
  (:gen-class))

(def pg-db "jdbc:postgresql://localhost:5432/mydb?user=myuser&password=mypassword")

(defn test-postgres []
  (with-open [conn (DriverManager/getConnection pg-db)]
    (let [stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT)")
      (.execute stmt "INSERT INTO users (name) VALUES ('Charlie')")
      (let [rs (.executeQuery stmt "SELECT * FROM users")]
        (while (.next rs)
          (println "PostgreSQL User:" (.getString rs "name")))))))

(defn -main []
  (println "Testing PostgreSQL...")
  (test-postgres)
  (println "PostgreSQL test completed!"))
