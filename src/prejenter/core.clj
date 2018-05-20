(ns prejenter.core
  (:import [java.awt Color Graphics2D Font RenderingHints]
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
          (let [ctx' (render* ctx (normalize-elem tree))]
            (assert (map? ctx')
                    (str `render* " must return a context map, but returned "
                         (pr-str ctx') " for " (first tree)))
            ctx')

          (fn? (first tree))
          (let [[f & args] tree
                tree' (apply f args)]
            (render ctx tree'))

          :else (render-coll ctx tree))
    (render-coll ctx tree)))

(defn render-coll [ctx tree]
  (reduce render ctx tree))

(defn attr-value
  ([ctx attrs attr-name]
   (attr-value ctx attrs attr-name nil))
  ([ctx attrs attr-name default-value]
   (or (get attrs attr-name)
       (get ctx attr-name)
       default-value)))

(defn render-text [{:keys [g] :as ctx} attrs x y text]
  (let [font-size (attr-value ctx attrs :font-size)
        font-family (attr-value ctx attrs :font-family)
        font-style (get {:normal Font/PLAIN :italic Font/ITALIC}
                        (attr-value ctx attrs :font-style)
                        Font/PLAIN)
        font-weight (get {:normal Font/PLAIN :bold Font/BOLD}
                         (attr-value ctx attrs :font-weight)
                         Font/PLAIN)
        font (Font. font-family (bit-or font-style font-weight) font-size)
        color (attr-value ctx attrs :color Color/BLACK)]
    (.setFont g font)
    (.setColor g color)
    (.drawString g text x y)
    ctx))

(defmethod render* :slide [{:keys [g] :as ctx} [_ _ & body]]
  (.setColor g (:background-color ctx))
  (.fillRect g 0 0 (:width ctx) (:height ctx))
  (render-coll ctx body))

(defmethod render* :title [{:keys [g] :as ctx} [_ attrs title]]
  (render-text ctx attrs 10 50 title))

(defmethod render* :items [{:keys [g] :as ctx} [_ attrs & items]]
  (reduce (fn [ctx [i item]]
            (render-text ctx attrs 10 (+ 100 (* i 30)) (str "- " item)))
          ctx
          (map-indexed vector items)))

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
    (render ctx x)
    (.dispose g)
    img))

(comment

  (def context
    {:width 640 :height 480
     :font-family "Gill Sans"
     :font-size 30
     :color Color/BLACK
     :background-color Color/WHITE})

  (def slide
    [:slide
     [:title
      {:font-style :italic :font-weight :bold}
      "Hello, World!"]
     [:items {:color Color/BLUE}
      "foo"
      "bar"
      "baz"]])

  (let [img (gen-image context slide)]
    (ImageIO/write img "png" (File. "/path/to/image.png")))

  )
