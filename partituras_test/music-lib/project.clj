(defproject music-lib "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-core "1.9.4"]
                 [ring/ring-jetty-adapter "1.9.4"]
                 [ring/ring-defaults "0.3.3"]
                 [compojure "1.7.0"]
                 [cheshire "5.11.0"]
                 [com.taoensso/carmine "3.2.0"]] ; Redis client
                 
  :main ^:skip-aot music-lib.core
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler music-lib.core/app}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})