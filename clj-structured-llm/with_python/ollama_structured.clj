(ns ollama-structured
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.pprint :as pprint]))

(def ollama-url "http://localhost:11434/api/chat")

(def book-schema
  {:type "object"
   :properties {:title   {:type "string"}
                :author  {:type "string"}
                :themes  {:type "array" :items {:type "string"}}}
   :required ["title" "author" "themes"]})

(defn get-structured-response
  "Sends `prompt` to the local Ollama server and asks it to return JSON
   matching book-schema. Returns a Clojure map (parsed JSON) or nil."
  [prompt]
  (let [payload  {:model    "gemma4:E2B"
                  :messages [{:role "user" :content prompt}]
                  :stream   false
                  :format   book-schema}
        response (http/post ollama-url
                             {:body         (json/generate-string payload)
                              :content-type :json
                              :accept       :json
                              :as           :json})
        body     (:body response)]
    ;; Debug
    (println "Raw response:")
    (pprint/pprint body)
    (when-let [content (get-in body [:message :content])]
      (json/parse-string content true))))

(defn -main
  "Optional: lets you still run this file standalone with
   `clojure -M -m ollama-structured`, independent of the Python side."
  [& _args]
  (println "Querying Ollama for structured data...")
  (try
    (let [result (get-structured-response "Give me a famous sci-fi book.")]
      (println "\n--- Success! ---")
      (if result
        (pprint/pprint result)
        (println "No structured content returned.")))
    (catch Exception e
      (println "Exception:")
      (.printStackTrace e))))

;; NOTE: intentionally NOT calling (-main) here.
;; When Python/jpype `require`s this namespace, we don't want side effects
;; firing automatically — Python decides when to invoke get-structured-response.