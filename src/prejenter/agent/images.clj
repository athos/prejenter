(ns prejenter.agent.images
  (:require [clojure.spec.alpha :as s]
            [prejenter.agent :as agent]
            [prejenter.slide :as slide]))

(defrecord ImagesAgent [state context slides]
  agent/Agent
  (current-page [this] (:current-page @state))
  (update-page! [this page]
    (swap! state assoc :current-page page))
  (slide [this page]
    (when (s/int-in-range? 0 (count slides) page)
      (slide/generate-slide context (nth slides page))))
  (show [this slide] slide))

(defn make-images-agent [context slides]
  (->ImagesAgent (atom {}) context slides))
