; project.clj

; src/packet_sniffer/core.clj
(ns sniffj.core
  (:require [clojure.string :as str]
            [byte-streams :as bs])
  (:import [org.pcap4j.core
           Pcaps
           PcapNetworkInterface
           PcapNetworkInterface$PromiscuousMode
           PcapHandle
           RawPacketListener]
          [org.pcap4j.packet
           Packet
           IpV4Packet
           TcpPacket
           UdpPacket
           IcmpV4Packet
           EthernetPacket]
          [org.pcap4j.util MacAddress]))

(def ^:dynamic *packet-count* (atom 0))

(defn hex-dump
  "Create a hex dump of packet data"
  [data]
  (let [bytes (bs/to-byte-array data)]
    (for [i (range 0 (count bytes) 16)]
      (let [chunk (take 16 (drop i bytes))]
        (str
         (format "%04X: " i)
         (str/join " " (map #(format "%02X" %) chunk))
         "   "
         (str/join (map #(if (and (>= % 32) (<= % 126))
                          (char %)
                          \.)
                       chunk)))))))

(defn print-ethernet-header
  "Print Ethernet frame header information"
  [packet]
  (let [eth-header (.getHeader packet)]
    (println "\nEthernet Header:")
    (println "  Source MAC:" (.toString (.getSourceAddress eth-header)))
    (println "  Destination MAC:" (.toString (.getDestinationAddress eth-header)))
    (println "  Type:" (format "0x%04X" (.value (.getType eth-header))))))

(defn print-ip-header
  "Print IP packet header information"
  [ip-packet]
  (let [ip-header (.getHeader ip-packet)]
    (println "\nIP Header:")
    (println "  Version:" (.getVersion ip-header))
    (println "  Header Length:" (* (.getIhl ip-header) 4) "bytes")
    (println "  TTL:" (.getTtl ip-header))
    (println "  Protocol:" (.getProtocol ip-header))
    (println "  Source IP:" (.getSourceAddress ip-header))
    (println "  Destination IP:" (.getDestinationAddress ip-header))))

(defn print-tcp-header
  "Print TCP packet header information"
  [tcp-packet]
  (let [tcp-header (.getHeader tcp-packet)]
    (println "\nTCP Header:")
    (println "  Source Port:" (.getSrcPort tcp-header))
    (println "  Destination Port:" (.getDstPort tcp-header))
    (println "  Sequence Number:" (.getSequenceNumber tcp-header))
    (println "  Acknowledgment Number:" (.getAcknowledgmentNumber tcp-header))
    (println "  Flags:")
    (println "    SYN:" (.getSyn tcp-header))
    (println "    ACK:" (.getAck tcp-header))
    (println "    FIN:" (.getFin tcp-header))
    (println "    RST:" (.getRst tcp-header))
    (println "    PSH:" (.getPsh tcp-header))
    (println "    URG:" (.getUrg tcp-header))
    (println "  Window Size:" (.getWindow tcp-header))))

(defn print-udp-header
  "Print UDP packet header information"
  [udp-packet]
  (let [udp-header (.getHeader udp-packet)]
    (println "\nUDP Header:")
    (println "  Source Port:" (.getSrcPort udp-header))
    (println "  Destination Port:" (.getDstPort udp-header))
    (println "  Length:" (.getLength udp-header))))

(defn analyze-packet
  "Analyze a captured packet and print its details"
  [packet]
  (swap! *packet-count* inc)
  (println "\n=== Packet" @*packet-count* "===")
  
  (when-let [eth-packet (cast EthernetPacket packet)]
    (print-ethernet-header eth-packet)
    
    (when-let [ip-packet (.get (.getPayload packet) IpV4Packet)]
      (print-ip-header ip-packet)
      
      (let [ip-payload (.getPayload ip-packet)]
        (cond
          (.get ip-payload TcpPacket)
          (print-tcp-header (.get ip-payload TcpPacket))
          
          (.get ip-payload UdpPacket)
          (print-udp-header (.get ip-payload UdpPacket))
          
          (.get ip-payload IcmpV4Packet)
          (println "\nICMP Packet detected"))))))

(defn start-capture!
  "Start capturing packets on the specified network interface"
  [interface-name]
  (let [nif (or (Pcaps/getDevByName interface-name)
                (throw (Exception. (str "Interface " interface-name " not found"))))
        handle (-> nif
                  (.openLive 65536
                            PcapNetworkInterface$PromiscuousMode/PROMISCUOUS
                            10000))]
    
    (println "Starting capture on interface:" interface-name)
    (println "Press Ctrl+C to stop")
    
    (.loop handle -1
           (reify RawPacketListener
             (gotPacket [this packet]
               (try
                 (analyze-packet (.getPacket packet))
                 (catch Exception e
                   (println "Error processing packet:" (.getMessage e)))))))))

(defn list-interfaces
  "List all available network interfaces"
  []
  (println "\nAvailable Network Interfaces:")
  (doseq [nif (Pcaps/findAllDevs)]
    (println (str "- " (.getName nif) ": " (.getDescription nif)))))

(defn -main
  "Main entry point - starts the packet sniffer"
  [& args]
  (if (empty? args)
    (do
      (println "Usage: lein run <interface-name>")
      (list-interfaces))
    (start-capture! (first args))))