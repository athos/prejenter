(ns prejenter.agent.images
  (:require [prejenter.agent :as agent]
            [prejenter.slide :as slide]))

(defrecord ImagesAgent [state context slides]
  agent/Agent
  (current-page [this] (:current-page @state))
  (update-page! [this page]
    (swap! state assoc :current-page page))
  (slide [this page]
    (when-let [slide (and (>= page 0) (nth slides page nil))]
      (slide/generate-slide context (nth slides page))))
  (show [this slide] slide))

(defn make-images-agent [context slides]
  (->ImagesAgent (atom {}) context slides))
