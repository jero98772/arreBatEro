(defproject ollama-structured "0.1.0-SNAPSHOT"
  :description "Ollama structured response library"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.12.3"]
                 [cheshire "5.11.0"]]
  :main ollama-structured.core
  :aot [ollama-structured.core]
  :uberjar-name "ollama-structured-standalone.jar"
  :test-paths ["test"]
  :resource-paths ["resources"]
  :profiles {:uberjar {:aot :all
                        :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})