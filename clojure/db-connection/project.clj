(defproject db-connection "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
		:dependencies [[org.clojure/clojure "1.11.1"]
               ;; SQLite
               [org.xerial/sqlite-jdbc "3.36.0.3"]
               ;; Redis (Carmine)
               [com.taoensso/carmine "3.1.0"]
               ;; MongoDB
               [com.novemberain/monger "3.5.0"]
               ;; PostgreSQL
               [org.postgresql/postgresql "42.5.0"]]
  :main ^:skip-aot db-connection.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})


