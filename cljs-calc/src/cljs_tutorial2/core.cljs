(ns cljs-tutorial2.core
    (:require 
              [reagent.core :as reagent :refer [atom]]
              [reagent.dom :as rd]))

(enable-console-print!)

;; Calculator state
(defonce app-state 
  (atom {:display "0"
         :previous nil
         :operation nil
         :waiting-for-operand false
         :history []}))

;; Helper functions
(defn parse-number [s]
  (js/parseFloat s))

(defn format-number [n]
  (if (js/Number.isInteger n)
    (str (int n))
    (str n)))

(defn add-to-history [calculation result]
  (swap! app-state update :history conj 
    {:calculation calculation 
     :result result 
     :timestamp (js/Date.)}))

;; Calculator operations
(defn calculate [first-operand operator second-operand]
  (let [prev (parse-number first-operand)
        current (parse-number second-operand)]
    (case operator
      "+" (+ prev current)
      "-" (- prev current)
      "*" (* prev current)
      "/" (if (zero? current) 
            (do (js/alert "Cannot divide by zero") prev)
            (/ prev current))
      current)))

;; Event handlers
(defn input-digit [digit]
  (let [{:keys [display waiting-for-operand]} @app-state]
    (if waiting-for-operand
      (swap! app-state assoc 
        :display (str digit)
        :waiting-for-operand false)
      (swap! app-state assoc 
        :display (if (= display "0") 
                  (str digit)
                  (str display digit))))))

(defn input-decimal []
  (let [{:keys [display waiting-for-operand]} @app-state]
    (if waiting-for-operand
      (swap! app-state assoc 
        :display "0."
        :waiting-for-operand false)
      (when (not (.includes display "."))
        (swap! app-state update :display str ".")))))

(defn clear []
  (swap! app-state assoc 
    :display "0"
    :previous nil
    :operation nil
    :waiting-for-operand false))

(defn clear-history []
  (swap! app-state assoc :history []))

(defn perform-operation [next-operation]
  (let [{:keys [display previous operation]} @app-state
        input-value (parse-number display)]
    (if (nil? previous)
      (swap! app-state assoc
        :previous display
        :waiting-for-operand true
        :operation next-operation)
      (when operation
        (let [result (calculate previous operation display)
              calculation (str previous " " operation " " display)]
          (add-to-history calculation result)
          (swap! app-state assoc
            :display (format-number result)
            :previous (format-number result)
            :waiting-for-operand true
            :operation next-operation))))))

(defn calculate-result []
  (let [{:keys [display previous operation]} @app-state]
    (when (and previous operation)
      (let [result (calculate previous operation display)
            calculation (str previous " " operation " " display)]
        (add-to-history calculation result)
        (swap! app-state assoc
          :display (format-number result)
          :previous nil
          :operation nil
          :waiting-for-operand true)))))

;; Components
(defn calculator-button [props content on-click]
  [:button 
   (merge {:class "calc-button"
           :on-click on-click} props)
   content])

(defn display []
  [:div {:class "display"}
   [:div {:class "display-value"} (:display @app-state)]])

(defn button-row [buttons]
  [:div {:class "button-row"}
   (for [[i button] (map-indexed vector buttons)]
     ^{:key i} button)])

(defn history-panel []
  (let [history (:history @app-state)]
    [:div {:class "history-panel"}
     [:div {:class "history-header"}
      [:h3 "History"]
      [:button {:class "clear-history-btn"
                :on-click clear-history}
       "Clear History"]]
     [:div {:class "history-list"}
      (if (empty? history)
        [:div {:class "no-history"} "No calculations yet"]
        (for [[i entry] (map-indexed vector (reverse history))]
          ^{:key i}
          [:div {:class "history-entry"}
           [:div {:class "calculation"} (:calculation entry)]
           [:div {:class "result"} (str "= " (format-number (:result entry)))]
           [:div {:class "timestamp"} (.toLocaleTimeString (:timestamp entry))]]))]]))

(defn calculator []
  [:div {:class "calculator-container"}
   [:div {:class "calculator"}
    [:h1 "Calculator"]
    [display]
    [button-row
     [(calculator-button {:class "calc-button clear"} "C" clear)
      (calculator-button {} "±" #(swap! app-state update :display 
                                   (fn [d] (format-number (* -1 (parse-number d))))))
      (calculator-button {} "%" #(swap! app-state update :display 
                                   (fn [d] (format-number (/ (parse-number d) 100)))))
      (calculator-button {:class "calc-button operator"} "÷" #(perform-operation "/"))]]
    
    [button-row
     [(calculator-button {} "7" #(input-digit "7"))
      (calculator-button {} "8" #(input-digit "8"))
      (calculator-button {} "9" #(input-digit "9"))
      (calculator-button {:class "calc-button operator"} "×" #(perform-operation "*"))]]
    
    [button-row
     [(calculator-button {} "4" #(input-digit "4"))
      (calculator-button {} "5" #(input-digit "5"))
      (calculator-button {} "6" #(input-digit "6"))
      (calculator-button {:class "calc-button operator"} "−" #(perform-operation "-"))]]
    
    [button-row
     [(calculator-button {} "1" #(input-digit "1"))
      (calculator-button {} "2" #(input-digit "2"))
      (calculator-button {} "3" #(input-digit "3"))
      (calculator-button {:class "calc-button operator"} "+" #(perform-operation "+"))]]
    
    [button-row
     [(calculator-button {:class "calc-button zero"} "0" #(input-digit "0"))
      (calculator-button {} "." input-decimal)
      (calculator-button {:class "calc-button equals"} "=" calculate-result)]]]
   
   [history-panel]])

;; Render the app
(rd/render [calculator]
           (. js/document (getElementById "app")))

;; Reload function
(defn on-js-reload []
  (swap! app-state update-in [:__figwheel_counter] inc))