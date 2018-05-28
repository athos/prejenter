(ns prejenter.slide
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

(defn generate-slide [{:keys [width height] :as ctx} x]
  (let [img (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR)
        g (.getGraphics img)
        ctx (assoc ctx :g g)]
    (enable-antialiasing g)
    (renderer/render ctx (layout/layout ctx x))
    (.dispose g)
    img))
