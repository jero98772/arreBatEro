(ns music-lib.db
  (:require [taoensso.carmine :as car :refer [wcar]]))


;; Connection pool configuration for Carmine 3.x
(def server-conn {
  :pool {; Commons Pool options
         :max-total 10}    ; Maximum number of connections
         
  :spec {; Connection spec
         :host "127.0.0.1"
         :port 6379
         :password nil
         :timeout-ms 3000}})

;; Utility macro for connection
(defmacro wcar* [& body] `(car/wcar server-conn ~@body))

;; CRUD Operations
(defn create!
  "Create a new key-value pair. Returns 'OK' if successful."
  [key value]
  (wcar* (car/set key value)))

(defn read!
  "Read value by key. Returns nil if key doesn't exist."
  [key]
  (wcar* (car/get key)))

(defn update!
  "Update existing key's value. Returns 'OK' if successful."
  [key value]
  (wcar* (car/set key value)))

(defn delete!
  "Delete key(s). Returns number of keys deleted."
  [& keys]
  (wcar* (apply car/del keys)))

;; Bonus: Complex data handling using EDN
(defn create-edn!
  "Store Clojure data as EDN string"
  [key value]
  (wcar* (car/set key (pr-str value))))

(defn read-edn!
  "Read and parse EDN string back to Clojure data"
  [key]
  (when-let [value (wcar* (car/get key))]
    (read-string value)))

(defn get-all-keys
  "Get all keys in the database (use with caution in production!)
  Optional pattern matching using Redis pattern syntax (default: *)"
  ([] (get-all-keys "*"))
  ([pattern]
   (wcar* (car/keys pattern))))

(defn count-keys
  "Return total number of keys in current database"
  []
  (wcar* (car/dbsize)))
;; Example usage:
(comment
  ;; String values
  (create! "user:1:name" "Alice")
  (read! "user:1:name")    ; => "Alice"
  (update! "user:1:name" "Alicia")
  (delete! "user:1:name")

  ;; EDN values
  (create-edn! "user:1" {:name "Bob" :age 30 :hobbies ["climbing" "gaming"]})
  (read-edn! "user:1")   ; => {:name "Bob", :age 30, :hobbies ["climbing" "gaming"]}

  ;; Update with existence check (only update if exists)
  (wcar* (car/set "user:1:age" "31" :xx))  ; Returns "OK" if exists
  ;; Create with existence check (only create if new)
  (wcar* (car/set "user:1:age" "30" :nx))  ; Returns "OK" if new key
)