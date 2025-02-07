(defproject sniffj "0.1.0"
  :description "Advanced packet sniffer in Clojure"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.pcap4j/pcap4j-core "1.8.2"]
                 [org.pcap4j/pcap4j-packetfactory-static "1.8.2"]
                 [byte-streams "0.2.4"]
                 [org.slf4j/slf4j-simple "1.7.26"]]
  :main sniffj.core
  :jvm-opts ["-Djava.library.path=/usr/lib/jni"]) ; Path to native pcap library
