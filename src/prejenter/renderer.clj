(ns prejenter.renderer
  (:require [prejenter.layout :as layout])
  (:import [java.awt Color Graphics2D]))

(set! *warn-on-reflection* true)

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

(defn render-text [{:keys [^Graphics2D g] :as ctx} attrs ^String text]
  (.setColor g (::layout/color attrs))
  (.setFont g (::layout/font attrs))
  (let [{::keys [current-x current-y]} ctx
        {::layout/keys [width height ascent]} attrs]
    (.drawString g text (int current-x) (int (+ current-y ascent)))))

(defn render-image [{:keys [g] :as ctx} {::layout/keys [image width height]}]
  (.drawImage ^Graphics2D g image
              (::current-x ctx)
              (::current-y ctx)
              width height nil))

(defmethod render* :slide [{:keys [width height] :as ctx} {:keys [attrs body]}]
  (let [{:keys [padding-top padding-left]} attrs
        ctx (assoc ctx ::current-x padding-left ::current-y padding-top)
        ^Graphics2D g (:g ctx)]
    (.setColor g (:background-color ctx))
    (.fillRect g 0 0 width height)
    (render-elems ctx body)))

(defmethod render* :text [ctx {:keys [attrs body]}]
  (render-text ctx attrs body))

(defmethod render* :image [ctx {:keys [attrs]}]
  (render-image ctx attrs))

(defmethod render* :title [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :inline [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :lines [ctx {:keys [attrs body]}]
  (render-elems ctx body))

(defmethod render* :items [ctx {:keys [attrs body]}]
  (render-elems ctx body))
