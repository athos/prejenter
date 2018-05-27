(ns prejenter.alignment
  (:require [prejenter.element :as elem]))

(alias 'layout 'prejenter.layout)

(defn locate-element [{:keys [attrs] :as elem} x y]
  (elem/add-attrs elem
                  ::layout/x (+ x (:padding-left attrs))
                  ::layout/y (+ y (:padding-top attrs))))

(defn align-in-inline [ctx paddings {:keys [attrs body] :as elem}]
  (let [widths (map #(elem/attr-value % ::layout/width) body)
        xs (reductions + 0 widths)
        width (apply + (:padding-left paddings) (:padding-right paddings) widths)
        height (->> (map #(elem/attr-value % ::layout/height) body)
                    (apply max)
                    (+ (:padding-top paddings) (:padding-bottom paddings)))]
    (assoc elem
           :attrs (-> attrs
                      (assoc ::layout/width width ::layout/height height)
                      (merge paddings))
           :body (map #(locate-element %1 %2 0) body xs))))

(defn align-in-block [ctx paddings {:keys [attrs body] :as elem}]
  (let [heights (map #(elem/attr-value % ::layout/height) body)
        ys (reductions + 0 heights)
        width (+ (- (::layout/max-x ctx) (::layout/min-x ctx))
                 (:padding-left paddings)
                 (:padding-right paddings))
        height (+ (apply + heights)
                  (:padding-top paddings)
                  (:padding-bottom paddings))]
    (assoc elem
           :attrs (-> attrs
                      (assoc ::layout/width width ::layout/height height)
                      (merge paddings))
           :body (map #(locate-element %1 0 %2) body ys))))
