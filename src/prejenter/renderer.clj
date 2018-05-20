(ns prejenter.renderer
  (:import [java.awt Color Graphics2D Font]))

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

(defn- set-font [{:keys [g] :as ctx} attrs]
  (let [font-size (attr-value ctx attrs :font-size)
        font-family (attr-value ctx attrs :font-family)
        font-style (get {:normal Font/PLAIN :italic Font/ITALIC}
                        (attr-value ctx attrs :font-style)
                        Font/PLAIN)
        font-weight (get {:normal Font/PLAIN :bold Font/BOLD}
                         (attr-value ctx attrs :font-weight)
                         Font/PLAIN)
        font (Font. font-family (bit-or font-style font-weight) font-size)]
    (.setFont g font)))

(defn- text-metrics [^Graphics2D g ^String text]
  (let [fm (.getFontMetrics g)
        lm (.getLineMetrics fm text g)]
    {:width (.stringWidth fm text)
     :height (.getHeight lm)
     :ascent (.getAscent lm)}))

(defn render-text [{:keys [g] :as ctx} attrs text]
  (.setColor g (attr-value ctx attrs :color Color/BLACK))
  (set-font ctx attrs)
  (let [{:keys [::current-x ::current-y]} ctx
        {:keys [width height ascent]} (text-metrics g text)]
    (.drawString g text current-x (int (+ current-y ascent)))
    (-> ctx
        (update ::current-x + width)
        (update ::current-y + height))))

(defn- paddings
  ([attrs] (paddings attrs 0))
  ([attrs default-value]
   (let [padding (get attrs :padding default-value)]
     (into {} (map (fn [k] [k (get attrs k padding)]))
           [:padding-top :padding-left :padding-bottom :padding-right]))))

(defn- apply-paddings [ctx paddings]
  (-> ctx
      (update ::min-x + (:padding-left paddings))
      (update ::max-x - (:padding-right paddings))
      (update ::min-y + (:padding-top paddings))
      (update ::max-y - (:padding-bottom paddings))))

(defn with-paddings [ctx attrs f]
  (let [paddings (paddings attrs)
        ctx' (apply-paddings ctx paddings)]
    ;; TODO: inline elements need more tweaks
    (-> ctx'
        (assoc ::current-x (::min-x ctx'))
        (update ::current-y + (:padding-top paddings))
        f
        (assoc ::current-x (::min-x ctx))
        (update ::current-y + (:padding-bottom paddings))
        (merge (select-keys ctx [::min-x ::max-x ::min-y ::max-y])))))

(defmethod render* :slide [{:keys [g width height] :as ctx} [_ attrs & body]]
  (let [ctx (assoc ctx
                   ::current-x 0, ::current-y 0
                   ::min-x 0, ::max-x width
                   ::min-y 0, ::max-y height)
        attrs (merge (select-keys ctx [:padding :padding-top :padding-left
                                       :padding-bottom :padding-right])
                     attrs)]
    (with-paddings ctx attrs
      (fn [ctx]
        (.setColor g (:background-color ctx))
        (.fillRect g 0 0 width height)
        (render-coll ctx body)))))

(defmethod render* :title [{:keys [g] :as ctx} [_ attrs title]]
  (with-paddings ctx attrs
    (fn [ctx]
      (render-text ctx attrs title))))

(defmethod render* :items [{:keys [g] :as ctx} [_ attrs & items]]
  (with-paddings ctx attrs
    (fn [ctx]
      (reduce (fn [ctx [i item]]
                (-> (render-text ctx attrs (str "・" item))
                    (assoc ::current-x (::min-x ctx))))
              ctx
              (map-indexed vector items)))))
