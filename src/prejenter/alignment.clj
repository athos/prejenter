(ns prejenter.alignment
  (:require [prejenter.element :as elem]))

(alias 'layout 'prejenter.layout)

(defn locate-element [{:keys [attrs] :as elem} x y]
  (elem/add-attrs elem
                  ::layout/x (+ x (::layout/padding-left attrs))
                  ::layout/y (+ y (::layout/padding-top attrs))))

(defn align-in-inline [ctx {:keys [attrs body] :as elem}]
  (let [widths (map #(elem/attr-value % ::layout/width) body)
        xs (reductions + 0 widths)
        width (apply +
                     (::layout/padding-left attrs)
                     (::layout/padding-right attrs)
                     widths)
        height (->> (map #(elem/attr-value % ::layout/height) body)
                    (apply max)
                    (+ (::layout/padding-top attrs)
                       (::layout/padding-bottom attrs)))]
    (-> elem
        (elem/add-attrs ::layout/width width ::layout/height height)
        (assoc :body (map #(locate-element %1 %2 0) body xs)))))

(defn align-in-block [ctx {:keys [attrs body] :as elem}]
  (let [heights (map #(elem/attr-value % ::layout/height) body)
        ys (reductions + 0 heights)
        width (+ (- (::layout/max-x ctx) (::layout/min-x ctx))
                 (::layout/padding-left attrs)
                 (::layout/padding-right attrs))
        height (+ (apply + heights)
                  (::layout/padding-top attrs)
                  (::layout/padding-bottom attrs))]
    (-> elem
        (elem/add-attrs ::layout/width width ::layout/height height)
        (assoc :body (map #(locate-element %1 0 %2) body ys)))))
