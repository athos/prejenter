(ns prejenter.core
  (:require [prejenter.layout :as layout]
            [prejenter.renderer :as renderer])
  (:import [java.awt Graphics2D RenderingHints]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)

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
    (renderer/render ctx (layout/layout ctx x))
    (.dispose g)
    img))

(comment

  (import '[java.awt Color]
          '[java.awt.image]
          '[java.io File]
          '[javax.imageio ImageIO])

  (def context
    {:width 640 :height 480
     :padding 20
     :font-family "Gill Sans"
     :font-size 30
     :color Color/BLACK
     :background-color Color/WHITE})

  (def slide
    [:slide
     [:title
      {:font-style :italic :font-weight :bold
       :padding-left 10 :padding-bottom 10
       :height 75 :text-align :center :vertical-align :middle}
      [:inline "Hello, " [:text {:color Color/RED :font-size 40} "World!"]]]
     [:items {:color Color/BLUE}
      [:text {:color Color/RED} "foo"]
      "bar"
      "baz"]
     [:lines {:color Color/GREEN}
      "hoge"
      "fuga"
      "piyo"]
     [:image {:src (ImageIO/read (File. "/path/to/clojure_logo.png"))
              :x 400 :y 50 :width 128 :height 128}]])

  (let [img (gen-image context slide)]
    (ImageIO/write img "png" (File. "/path/to/image.png")))

  )
