(ns prejenter.core
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio ImageIO]))

(defn hello-world [^File dst]
  (let [img (BufferedImage. 200 200 BufferedImage/TYPE_3BYTE_BGR)
        g (.getGraphics img)]
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 200 200)
    (.setColor g Color/BLACK)
    (.drawString g "Hello, World!" 100 100)
    (.dispose g)
    (ImageIO/write img "png" dst)))

(comment

  (hello-world (File. "/path/to/image.png"))

  )
