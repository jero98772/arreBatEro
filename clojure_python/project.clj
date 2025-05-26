(defproject myapp "0.1.0-SNAPSHOT"
  :description "Clojure-Python integration example"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main myapp.core
  :aot [myapp.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})