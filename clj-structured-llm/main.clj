(ns ollama-structured
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.pprint :as pprint]))

(def ollama-url "http://localhost:11434/api/chat")

(def book-schema
  {:type "object"
   :properties
   {:title {:type "string"}
    :author {:type "string"}
    :themes {:type "array"
             :items {:type "string"}}}
   :required ["title" "author" "themes"]})

(defn get-structured-response [prompt]
  (let [payload {:model "gemma4:E2B"
                 :messages [{:role "user"
                             :content prompt}]
                 :stream false
                 :format book-schema}

        response (http/post
                   ollama-url
                   {:body (json/generate-string payload)
                    :content-type :json
                    :accept :json
                    :as :json})

        body (:body response)]

    ;; Debug
    (println "Raw response:")
    (pprint/pprint body)

    (when-let [content (get-in body [:message :content])]
      (json/parse-string content true))))
(println "FILE LOADED")


(defn -main []
  (println "Querying Ollama for structured data...")
  (try
    (let [result (get-structured-response
                   "Give me a famous sci-fi book.")]
      (println "\n--- Success! ---")
      (if result
        (pprint/pprint result)
        (println "No structured content returned.")))
    (catch Exception e
      (println "Exception:")
      (.printStackTrace e))))

;; actually run it
(apply -main *command-line-args*)