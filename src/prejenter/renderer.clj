(ns prejenter.renderer
  (:require [prejenter.layout :as layout])
  (:import [java.awt Color Graphics2D]))

(set! *warn-on-reflection* true)

(defmulti render-element (fn [ctx elem] (:tag elem)))

(defn render* [ctx elem]
  (render-element ctx elem))

(defn render [ctx elem]
  (render* ctx elem))

(defn move-to [ctx x y]
  (-> ctx
      (update ::current-x + x)
      (update ::current-y + y)))

(defn fill-rect [{:keys [^Graphics2D g]} x y width height color]
  (.setColor g color)
  (.fillRect g x y width height))

(defn render-background [ctx attrs]
  (when-let [color (:background-color attrs)]
    (let [{::layout/keys [width height padding-top padding-left]} attrs
          x (- (::current-x ctx) padding-left)
          y (- (::current-y ctx) padding-top)]
      (fill-rect ctx x y width height color))))

(defn render-elements [ctx attrs elems]
  (render-background ctx attrs)
  (doseq [{:keys [attrs] :as elem} elems
          :let [{::layout/keys [x y]} attrs
                ctx (move-to ctx x y)]]
    (render* ctx elem)))

(defn render-text [{:keys [^Graphics2D g] :as ctx} attrs ^String text]
  (.setColor g (::layout/color attrs))
  (.setFont g (::layout/font attrs))
  (let [{::keys [current-x current-y]} ctx
        {::layout/keys [width height ascent]} attrs]
    (.drawString g text (int current-x) (int (+ current-y ascent)))))

(defn render-image [{:keys [^Graphics2D g] :as ctx} attrs]
  (let [{::layout/keys [image width height]} attrs]
    (.drawImage g image (::current-x ctx) (::current-y ctx)
                (- width
                   (::layout/padding-left attrs)
                   (::layout/padding-right attrs))
                (- height
                   (::layout/padding-top attrs)
                   (::layout/padding-bottom attrs))
                nil)))

(defmethod render-element :slide [{:keys [width height] :as ctx} {:keys [attrs body]}]
  (let [{::layout/keys [padding-top padding-left]} attrs
        ctx (assoc ctx ::current-x padding-left ::current-y padding-top)]
    (fill-rect ctx 0 0 width height (:background-color ctx))
    (render-elements ctx attrs body)))

(defmethod render-element :text [ctx {:keys [attrs body]}]
  (render-background ctx attrs)
  (render-text ctx attrs body))

(defmethod render-element :image [ctx {:keys [attrs]}]
  (render-background ctx attrs)
  (render-image ctx attrs))

(defmethod render-element :title [ctx {:keys [attrs body]}]
  (render-elements ctx attrs body))

(defmethod render-element :inline [ctx {:keys [attrs body]}]
  (render-elements ctx attrs body))

(defmethod render-element :block [ctx {:keys [attrs body]}]
  (render-elements ctx attrs body))

(defmethod render-element :lines [ctx {:keys [attrs body]}]
  (render-elements ctx attrs body))

(defmethod render-element :items [ctx {:keys [attrs body]}]
  (render-elements ctx attrs body))
