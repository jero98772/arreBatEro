(ns opencv-test.core
  (:import [org.opencv.core Core Mat]
           [org.opencv.highgui HighGui Imgcodecs]))

(defn -main []
  (System/loadLibrary Core/NATIVE_LIBRARY_NAME)
  (println "Hello, OpenCV in Clojure!")
  (let [image (Imgcodecs/imread "test.jpg")]
    (if (.empty image)
      (println "Failed to load image")
      (do
        (HighGui/imshow "Test Image" image)
        (HighGui/waitKey 0)))))

(-main)
