(ns prejenter.export
  (:require [clojure.java.io :as io]
            [prejenter.slide :as slide])
  (:import [com.itextpdf.awt PdfGraphics2D]
           [com.itextpdf.text Document Image Rectangle]
           [com.itextpdf.text.pdf PdfContentByte PdfWriter]))

(set! *warn-on-reflection* true)

(defn- render-image [^PdfContentByte content ctx slide]
  (let [width (double (:width ctx))
        height (double (:height ctx))
        template (.createTemplate content width height)
        image (Image/getInstance (slide/generate-slide ctx slide) nil)]
    (.addImage content image width 0.0 0.0 height 0.0 0.0)
    (.addTemplate content template 0.0 0.0)))

(defn export [file {:keys [width height] :as ctx} slides]
  (let [width (long width)
        height (long height)
        doc (Document. (Rectangle. width height))
        writer (PdfWriter/getInstance doc (io/output-stream file))]
    (future
      (.open doc)
      (with-open [doc doc]
        (let [content (.getDirectContent writer)]
          (doseq [slide slides]
            (render-image content ctx slide)
            (.newPage doc)))))))
