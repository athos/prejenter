(ns prejenter.layout
  (:require [prejenter.element :as elem]
            [prejenter.utils :as utils])
  (:import [java.awt Graphics2D Font]))

(set! *warn-on-reflection* true)

(defn- normalize-elem [[tag & args :as elem]]
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

(defmulti layout* (fn [ctx elem] (:tag elem)))

(declare paddings apply-paddings wrap-with-paddings)

(defn layout [ctx elem]
  (letfn [(rec [ctx elem]
            (cond (string? elem)
                  (recur ctx [:text {} elem])

                  (vector? elem)
                  (let [[tag & args] elem]
                    (cond (keyword? tag)
                          (let [elem (normalize-elem elem)
                                paddings (paddings ctx (:attrs elem))]
                            (as-> elem elem
                              (update elem :attrs #(-> (merge paddings %) (dissoc :padding)))
                              (layout* (apply-paddings ctx paddings) elem)
                              (wrap-with-paddings elem paddings)))

                          (fn? tag)
                          (rec ctx (apply tag args))

                          :else (throw (ex-info "Tag must be a keyword or fn" {:tag tag}))))
                  :else (throw (ex-info "Element must be a string or vector" {:element elem}))))]
    (rec (assoc ctx ::min-x 0 ::max-x (:width ctx) ::min-y 0 ::max-y (:height ctx))
         elem)))

(defn layout-elems [ctx elems]
  (map #(layout ctx %) elems))

(defn- paddings
  ([ctx attrs] (paddings ctx attrs 0))
  ([ctx attrs default-value]
   (let [padding (utils/attr-value ctx attrs :padding default-value)]
     (into {} (map (fn [k] [k (utils/attr-value ctx attrs k padding)]))
           [:padding-top :padding-left :padding-bottom :padding-right]))))

(defn- apply-paddings [ctx paddings]
  (-> ctx
      (update ::min-x + (:padding-left paddings))
      (update ::max-x - (:padding-right paddings))
      (update ::min-y + (:padding-top paddings))
      (update ::max-y - (:padding-bottom paddings))))

(defn- wrap-with-paddings [elem paddings]
  (-> elem
      (update-in [:attrs ::width] + (:padding-left paddings) (:padding-right paddings))
      (update-in [:attrs ::height] + (:padding-top paddings) (:padding-bottom paddings))))

(defn ^Font attrs-font [ctx attrs]
  (let [font-size (utils/attr-value ctx attrs :font-size)
        font-family (utils/attr-value ctx attrs :font-family)
        font-style (get {:normal Font/PLAIN :italic Font/ITALIC}
                        (utils/attr-value ctx attrs :font-style)
                        Font/PLAIN)
        font-weight (get {:normal Font/PLAIN :bold Font/BOLD}
                         (utils/attr-value ctx attrs :font-weight)
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

(defmethod layout* :text [{:keys [g] :as ctx} {:keys [attrs body] :as elem}]
  (let [text (apply str body)]
    (with-font ctx attrs
      (fn [font]
        (let [{:keys [width height ascent]} (text-metrics g text)]
          (-> elem
              (elem/add-attrs ::width width ::height height ::ascent ascent ::font font)
              (assoc :body text)))))))

(defmethod layout* :title [ctx {:keys [attrs body] :as elem}]
  (let [title (layout ctx (first body))]
    (-> elem
        (elem/add-attrs ::width (- (::max-x ctx) (::min-x ctx))
                        ::height (elem/attr-value title ::height))
        (assoc :body title))))

(defmethod layout* :inline [ctx {:keys [attrs body] :as elem}]
  (loop [elems (layout-elems ctx body), x 0, y 0, acc []]
    (if (empty? elems)
      (-> elem
          (elem/add-attrs ::width x ::height y)
          (assoc :body acc))
      (let [[elem & elems] elems]
        (recur elems
               (+ x (elem/attr-value elem ::width))
               (max y (elem/attr-value elem ::height))
               (conj acc (elem/add-attrs elem ::x x ::y 0)))))))

(defmethod layout* :items [ctx {:keys [attrs body] :as elem}]
  (let [elems (->> body
                   (map (fn [item] [:inline "・" item]))
                   (layout-elems ctx))]
    (loop [elems elems, y 0, acc []]
      (if (empty? elems)
        (-> elem
            (elem/add-attrs ::width (- (::max-x ctx) (::min-x ctx)) ::height y)
            (assoc :body acc))
        (let [[elem & elems] elems]
          (recur elems
                 (+ y (elem/attr-value elem ::height))
                 (conj acc (elem/add-attrs elem ::x 0 ::y y))))))))

(defmethod layout* :slide [{:keys [width height] :as ctx} {:keys [attrs body] :as elem}]
  (let [ctx (dissoc ctx :padding :padding-top :padding-left :padding-bottom :padding-right)]
    (-> elem
        (elem/add-attrs ::width (- width (:padding-left attrs) (:padding-right attrs))
                        ::height (- height (:padding-top attrs) (:padding-bottom attrs)))
        (assoc :body (layout-elems ctx body)))))
