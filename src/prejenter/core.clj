(ns prejenter.core
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio ImageIO]))

(defmulti render* (fn [ctx elem] (first elem)))

(defn- normalize-elem [[tag & args :as elem]]
  (if (map? (first args))
    elem
    `[~tag {} ~@args]))

(declare render-coll)

(defn render [ctx tree]
  (if (vector? tree)
    (cond (keyword? (first tree))
          (or (render* ctx (normalize-elem tree))
              ctx)

          (fn? (first tree))
          (let [[f & args] tree
                tree' (apply f args)]
            (render ctx tree'))

          :else (render-coll ctx tree))
    (render-coll ctx tree)))

(defn render-coll [ctx tree]
  (reduce render ctx tree))

(defmethod render* :slide [{:keys [g] :as ctx} [_ _ & body]]
  (.setColor g (:background-color ctx))
  (.fillRect g 0 0 (:width ctx) (:height ctx))
  (render-coll ctx body))

(defmethod render* :title [{:keys [g] :as ctx} [_ _ title]]
  (.setColor g (:color ctx))
  (.drawString g title 10 10))

(defmethod render* :items [{:keys [g] :as ctx} [_ _ & items]]
  (.setColor g (:color ctx))
  (doseq [[i item] (map-indexed vector items)]
    (.drawString g (str "- " item) 10 (+ 50 (* i 10)))))

(defn gen-image [{:keys [width height] :as ctx} x]
  (let [img (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR)
        g (.getGraphics img)
        ctx (assoc ctx :g g)]
    (render ctx x)
    (.dispose g)
    img))

(comment

  (def context
    {:width 640 :height 480
     :color Color/BLACK
     :background-color Color/WHITE})

  (def slide
    [:slide
     [:title "Hello, World!"]
     [:items
      "foo"
      "bar"
      "baz"]])

  (let [img (gen-image context slide)]
    (ImageIO/write img "png" (File. "/path/to/image.png")))

  )
