(defproject pythonclj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.11.1"]]
		:main pythonclj.core
  :aot [pythonclj.core] ; <== AOT compile
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
