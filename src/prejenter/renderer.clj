(ns prejenter.renderer
  (:require [prejenter.utils :as utils]
            [prejenter.layout :as layout])
  (:import [java.awt Color Graphics2D]))

(defmulti render* (fn [ctx elem] (:tag elem)))

(defn render [ctx elem]
  (render* ctx elem))

(defn move-to [ctx x y]
  (-> ctx
      (update ::current-x + x)
      (update ::current-y + y)))

(defn render-elems [ctx elems]
  (doseq [{:keys [attrs] :as elem} elems
          :let [{::layout/keys [x y]} attrs
                ctx (move-to ctx x y)]]
    (render ctx elem)))

(defn render-text [{:keys [g] :as ctx} attrs text]
  (.setColor g (utils/attr-value ctx attrs :color Color/BLACK))
  (.setFont g (::layout/font attrs))
  (let [{::keys [current-x current-y]} ctx
        {::layout/keys [x y width height ascent]} attrs]
    (.drawString g text current-x (int (+ current-y ascent)))))

(defn render-image [{:keys [g] :as ctx} image {:keys [x y width height]}]
  (if (and width height)
    (.drawImage g image x y width height nil)
    (.drawImage g image x y nil)))

(defmethod render* :slide [{:keys [g width height] :as ctx} {:keys [attrs body]}]
  (let [padding-left (utils/attr-value ctx attrs :padding-left)
        padding-top (utils/attr-value ctx attrs :padding-top)
        ctx (assoc ctx ::current-x padding-left ::current-y padding-top)]
    (.setColor g (:background-color ctx))
    (.fillRect g 0 0 width height)
    (render-elems ctx body)))

(defmethod render* :text [ctx {:keys [attrs body]}]
  (render-text ctx attrs body))

(defmethod render* :title [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :inline [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :items [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :image [ctx [_ attrs]]
  (render-image ctx (:src attrs) (dissoc attrs :src)))
