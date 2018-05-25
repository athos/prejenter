(ns prejenter.layout
  (:require [prejenter.element :as elem])
  (:import [java.awt Color Font Graphics2D]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)

(defn- normalize-element [[tag & args :as elem]]
  (letfn [(splice-seqs [args]
            (persistent! (rec (transient []) args)))
          (rec [ret xs]
            (reduce (fn [ret x]
                      (if (seq? x)
                        (rec ret x)
                        (conj! ret x)))
                    ret
                    xs))]
   (if (map? (first args))
     (elem/->Element tag (first args) (splice-seqs (rest args)))
     (elem/->Element tag {} (splice-seqs args)))))

(defmulti layout-element (fn [ctx elem] (:tag elem)))

(defn layout* [ctx elem]
  (cond (string? elem)
        (recur ctx [:text {} elem])

        (vector? elem)
        (let [[tag & args] elem]
          (cond (keyword? tag)
                (layout-element ctx (normalize-element elem))

                (fn? tag)
                (layout* ctx (apply tag args))

                :else (throw (ex-info "Tag must be a keyword or fn" {:tag tag}))))
        :else (throw (ex-info "Element must be a string or vector" {:element elem}))))

(defn layout [ctx elem]
  (layout* ctx elem))

(defn layout-elements [ctx elems]
  (map #(layout* ctx %) elems))

(defn- paddings [attrs]
  (let [padding (get attrs :padding 0)]
    (into {} (map (fn [k] [k (get attrs k padding)]))
          [:padding-top :padding-left :padding-bottom :padding-right])))

(defn- apply-paddings [ctx paddings]
  (-> ctx
      (update ::min-x + (:padding-left paddings))
      (update ::max-x - (:padding-right paddings))
      (update ::min-y + (:padding-top paddings))
      (update ::max-y - (:padding-bottom paddings))))

(defn with-paddings [ctx attrs f]
  (let [paddings (paddings attrs)
        ctx (apply-paddings ctx paddings)]
    (f ctx paddings)))

(defn locate-element [{:keys [attrs] :as elem} x y]
  (let [{:keys [padding-left padding-top]} (paddings attrs)]
    (elem/add-attrs elem ::x (+ x padding-left) ::y (+ y padding-top))))

(def ^:private inheritable-attrs
  #{:font-size :font-family :font-style :font-weight :color})

(defn- inject-attrs [ctx attrs]
  (merge ctx (select-keys attrs inheritable-attrs)))

(defn layout-in-inline [ctx {:keys [attrs body] :as elem}]
  (with-paddings ctx attrs
    (fn [ctx' {:keys [padding-top padding-left padding-bottom padding-right]}]
      (let [ctx' (inject-attrs ctx' attrs)
            elems (layout-elements ctx' body)
            widths (map #(elem/attr-value % ::width) elems)
            xs (reductions + 0 widths)
            width (+ (apply + widths) padding-left padding-right)
            height (->> (map #(elem/attr-value % ::height) elems)
                        (apply max)
                        (+ padding-top padding-bottom))]
        (assoc elem
               :attrs (assoc attrs ::width width ::height height)
               :body (map #(locate-element %1 %2 0) elems xs))))))

(defn layout-in-block [ctx {:keys [attrs body] :as elem}]
  (with-paddings ctx attrs
    (fn [ctx' {:keys [padding-top padding-left padding-bottom]}]
      (let [ctx' (inject-attrs ctx' attrs)
            elems (layout-elements ctx' body)
            heights (map #(elem/attr-value % ::height) elems)
            ys (reductions + 0 heights)
            width (- (::max-x ctx) (::min-x ctx))
            height (+ (apply + heights) padding-top padding-bottom)]
        (assoc elem
               :attrs (assoc attrs ::width width ::height height)
               :body (map #(locate-element %1 0 %2) elems ys))))))

(defn attr-value
  ([ctx attrs attr-name]
   (attr-value ctx attrs attr-name nil))
  ([ctx attrs attr-name default-value]
   (or (get attrs attr-name)
       (get ctx attr-name)
       default-value)))

(defn ^Font attrs-font [ctx attrs]
  (let [font-size (attr-value ctx attrs :font-size)
        font-family (attr-value ctx attrs :font-family)
        font-style (get {:normal Font/PLAIN :italic Font/ITALIC}
                        (attr-value ctx attrs :font-style)
                        Font/PLAIN)
        font-weight (get {:normal Font/PLAIN :bold Font/BOLD}
                         (attr-value ctx attrs :font-weight)
                         Font/PLAIN)]
    (Font. font-family (bit-or font-style font-weight) font-size)))

(defn with-font [{:keys [^Graphics2D g] :as ctx} attrs f]
  (let [old-font (.getFont g)
        font (attrs-font ctx attrs)]
    (try
      (.setFont g font)
      (f font)
      (finally
        (.setFont g old-font)))))

(defn- text-metrics [^Graphics2D g ^String text]
  (let [fm (.getFontMetrics g)
        lm (.getLineMetrics fm text g)]
    {:width (.stringWidth fm text)
     :height (.getHeight lm)
     :ascent (.getAscent lm)}))

(defmethod layout-element :text [{:keys [g] :as ctx} {:keys [attrs body] :as elem}]
  (let [text (apply str body)
        {:keys [padding-top padding-left padding-bottom padding-right]} (paddings attrs)]
    (with-font ctx attrs
      (fn [font]
        (let [{:keys [width height ascent]} (text-metrics g text)
              color (attr-value ctx attrs :color Color/BLACK)]
          (-> elem
              (elem/add-attrs ::width (+ width padding-left padding-right)
                              ::height (+ height padding-top padding-bottom)
                              ::ascent ascent ::font font ::color color)
              (assoc :body text)))))))

(defmethod layout-element :image [ctx {:keys [attrs] :as elem}]
  (let [^BufferedImage image (:src attrs)
        width (.getWidth image)
        height (.getHeight image)]
    (elem/add-attrs elem ::width width ::height height ::image image)))

(defmethod layout-element :title [ctx elem]
  (layout-in-block ctx elem))

(defmethod layout-element :inline [ctx elem]
  (layout-in-inline ctx elem))

(defmethod layout-element :lines [ctx elem]
  (layout-in-block ctx elem))

(defmethod layout-element :items [ctx elem]
  (->> (update elem :body #(map (fn [item] [:inline "・" item]) %))
       (layout-in-block ctx)))

(defmethod layout-element :slide [{:keys [width height] :as ctx} elem]
  (let [ctx (assoc ctx ::min-x 0 ::max-x width ::min-y 0 ::max-y height)
        attrs (merge (paddings ctx) (:attrs elem))]
    (layout-in-block ctx (assoc elem :attrs attrs))))
