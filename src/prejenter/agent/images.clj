(ns prejenter.agent.images
  (:require [clojure.spec.alpha :as s]
            [prejenter.agent :as agent]
            [prejenter.slide :as slide]))

(defrecord ImagesAgent [state context slides]
  agent/Agent
  (get-state [this] @state)
  (update-state! [this args]
    (apply swap! state args))
  (slide [this page]
    (when (s/int-in-range? 0 (count slides) page)
      (slide/generate-slide context (nth slides page))))
  (show [this slide] slide))

(defn make-images-agent [context slides]
  (->ImagesAgent (atom {}) context slides))
