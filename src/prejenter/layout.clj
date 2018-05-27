(ns prejenter.layout
  (:require [prejenter.alignment :as align]
            [prejenter.element :as elem])
  (:import [java.awt Color Font Graphics2D]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)

(defn- paddings [attrs]
  (let [padding (get attrs :padding 0)]
    (into {} (map (fn [k]
                    (let [k' (keyword "prejenter.layout" (name k))]
                      [k' (get attrs k padding)])))
          [:padding-top :padding-left :padding-bottom :padding-right])))

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
     (elem/->Element tag
                     (as-> (first args) attrs
                       (merge attrs (paddings attrs)))
                     (splice-seqs (rest args)))
     (elem/->Element tag (paddings {}) (splice-seqs args)))))

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
  (map-indexed (fn [i elem]
                 (-> (layout* ctx elem)
                     (elem/add-attrs ::index i)))
               elems))

(defn with-paddings [ctx attrs f]
  (f (-> ctx
         (update ::min-x + (::padding-left attrs))
         (update ::max-x - (::padding-right attrs))
         (update ::min-y + (::padding-top attrs))
         (update ::max-y - (::padding-bottom attrs)))))

(defn position [ctx {:keys [attrs] :as elem}]
  (let [{:keys [top left bottom right]} attrs]
    (update elem :attrs merge
            (when (or top left bottom right)
              (cond-> {::x 0 ::y 0}
                top (assoc ::y top)
                left (assoc ::x left)
                bottom (assoc ::y (- (::max-y ctx) (::min-y ctx)
                                     (::height attrs)
                                     bottom))
                right (assoc ::x (- (::max-x ctx) (::min-x ctx)
                                    (::width attrs)
                                    right)))))))

(defn positioned? [{:keys [attrs]}]
  (boolean (and (::x attrs) (::y attrs))))

(def ^:private inheritable-attrs
  #{:font-size :font-family :font-style :font-weight :color :text-align :vertical-align})

(defn- inject-attrs [ctx attrs]
  (merge ctx (select-keys attrs inheritable-attrs)))

(defn attr-value
  ([ctx attrs attr-name]
   (attr-value ctx attrs attr-name nil))
  ([ctx attrs attr-name default-value]
   (or (get attrs attr-name)
       (get ctx attr-name)
       default-value)))

(defn- layout-with-alignment [ctx {:keys [attrs body] :as elem} align-fn]
  (with-paddings ctx attrs
    (fn [ctx]
      (let [ctx (inject-attrs ctx attrs)
            {positioned true, elems false} (->> (layout-elements ctx body)
                                                (map #(position ctx %))
                                                (group-by positioned?))
            attrs (assoc attrs
                         ::text-align (attr-value ctx attrs :text-align)
                         ::vertical-align (attr-value ctx attrs :vertical-align))]
        (as-> elem elem
          (assoc elem :attrs attrs :body elems)
          (align-fn ctx elem)
          (update elem :body
                  (fn [elems]
                    (->> elems
                         (concat positioned)
                         (sort-by #(elem/attr-value % ::index))))))))))

(defn layout-in-inline [ctx elem]
  (layout-with-alignment ctx elem align/align-in-inline))

(defn layout-in-block [ctx {:keys [attrs body] :as elem}]
  (layout-with-alignment ctx elem align/align-in-block))

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

(defmethod layout-element :text [{:keys [g] :as ctx} {:keys [attrs] :as elem}]
  (with-font ctx attrs
    (fn [font]
      (let [text (apply str (:body elem))
            {:keys [width height ascent]} (text-metrics g text)
            width (+ width (::padding-left attrs) (::padding-right attrs))
            height (+ height (::padding-top attrs) (::padding-bottom attrs))
            color (attr-value ctx attrs :color Color/BLACK)]
        (-> elem
            (elem/add-attrs ::width width ::height height ::ascent ascent
                            ::font font ::color color)
            (assoc :body text))))))

(defn- image-size [^BufferedImage image attrs]
  (let [{:keys [width height] :or {width :auto, height :auto}} attrs]
    (if (and (number? width) (number? height))
      [width height]
      (let [img-width (.getWidth image), img-height (.getHeight image)]
        (cond (number? width)
              [width (* img-height (/ (double width) img-width))]

              (number? height)
              [(* img-width (/ (double height) img-height)) height]

              :else [img-width img-height])))))

(defmethod layout-element :image [ctx {:keys [attrs] :as elem}]
  (let [image (:src attrs)
        [width height] (image-size image attrs)]
    (elem/add-attrs elem
                    ::width (+ width
                               (::padding-left attrs)
                               (::padding-right attrs))
                    ::height (+ height
                                (::padding-top attrs)
                                (::padding-bottom attrs))
                    ::image image)))

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
        attrs (as-> (:attrs elem) attrs
                (merge attrs (paddings (merge ctx attrs))))]
    (layout-in-block ctx (assoc elem :attrs attrs))))
