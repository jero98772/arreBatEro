(defproject my-website "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [ring/ring-core "1.9.3"]
                 [ring/ring-jetty-adapter "1.9.3"]
                 [compojure "1.6.2"]
                 [hiccup "1.0.5"]]
  :plugins [[lein-ring "0.12.5"]]

  :ring {:handler my-website.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
  																						[org.clojure/clojure "1.10.3"]
                        [ring/ring-mock "0.3.2"]
                        [compojure "1.6.2"]
                        [hiccup "1.0.5"]]}}
  :main ^:skip-aot my-website.core)

