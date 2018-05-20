(ns prejenter.core
  (:require [prejenter.renderer :as renderer])
  (:import [java.awt Color Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio ImageIO]))

(set! *warn-on-reflect* true)

(defn- enable-antialiasing [^Graphics2D g]
  (doto g
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)))

(defn gen-image [{:keys [width height] :as ctx} x]
  (let [img (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR)
        g (.getGraphics img)
        ctx (assoc ctx :g g)]
    (enable-antialiasing g)
    (renderer/render ctx x)
    (.dispose g)
    img))

(comment

  (def context
    {:width 640 :height 480
     :font-family "Gill Sans"
     :font-size 30
     :color Color/BLACK
     :background-color Color/WHITE})

  (def slide
    [:slide
     [:title
      {:font-style :italic :font-weight :bold}
      "Hello, World!"]
     [:items {:color Color/BLUE}
      "foo"
      "bar"
      "baz"]])

  (let [img (gen-image context slide)]
    (ImageIO/write img "png" (File. "/path/to/image.png")))

  )
