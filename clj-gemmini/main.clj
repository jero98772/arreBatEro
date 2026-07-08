(ns gemini-client.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]))

(def base-url "https://generativelanguage.googleapis.com/v1beta/models/gemini-flash-latest:generateContent")

(defn query-gemini
  "Sends a text prompt to the Gemini API and returns the text response."
  [api-token prompt]
  (let [;; Gemini API expects this specific nested JSON structure
        payload {:contents [{:parts [{:text prompt}]}]}
        
        ;; Make the POST request
        response (client/post base-url
                              {:query-params {"key" api-token}
                               :content-type :json
                               :accept       :json
                               :body         (json/generate-string payload)
                               :throw-exceptions false})] ; Handle errors gracefully
    
    (if (= 200 (:status response))
      ;; Parse the successful response to extract just the text
      (-> response
          :body
          (json/parse-string true) ; true converts JSON keys to Clojure keywords
          :candidates
          first
          :content
          :parts
          first
          :text)
      
      (throw (ex-info "Failed to query Gemini" 
                      {:status (:status response) 
                       :error (:body response)})))))

(def my-token "token here")

;; Run the query
(println (query-gemini my-token "Explain functional programming in one sentence."))

