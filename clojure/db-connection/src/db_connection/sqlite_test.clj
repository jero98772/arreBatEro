(ns db-connection.sqlite-test
  (:import [java.sql DriverManager])
  (:gen-class))

(def sqlite-db "jdbc:sqlite:database.sqlite")

(defn test-sqlite []
  (with-open [conn (DriverManager/getConnection sqlite-db)]
    (let [stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)")
      (.execute stmt "INSERT INTO users (name) VALUES ('Alice')")
      (let [rs (.executeQuery stmt "SELECT * FROM users")]
        (while (.next rs)
          (println "SQLite User:" (.getString rs "name")))))))

(defn -main []
  (println "Testing SQLite...")
  (test-sqlite)
  (println "SQLite test completed!"))
