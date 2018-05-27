(ns prejenter.alignment
  (:require [prejenter.element :as elem]))

(alias 'layout 'prejenter.layout)

(defn locate-element [{:keys [attrs] :as elem} x y]
  (elem/add-attrs elem
                  ::layout/x (+ x (::layout/padding-left attrs))
                  ::layout/y (+ y (::layout/padding-top attrs))))

(defn align-in-inline [ctx {:keys [attrs body] :as elem}]
  (let [widths (map #(elem/attr-value % ::layout/width) body)
        width (apply + widths)
        delta-x (max (- (:width attrs width) width) 0)
        height (->> (map #(elem/attr-value % ::layout/height) body)
                    (apply max))
        delta-y (max (- (:height attrs height) height) 0)
        offset-x (case (::layout/text-align attrs)
                   :center (/ delta-x 2.0)
                   :right delta-x
                   0)
        offset-y (case (::layout/vertical-align attrs)
                   :middle #(/ (- height (elem/attr-value % ::layout/height)) 2.0)
                   :bottom #(- height (elem/attr-value % ::layout/height))
                   (constantly 0))
        xs (reductions + offset-x widths)]
    (-> elem
        (elem/add-attrs ::layout/width (+ width delta-x
                                          (::layout/padding-left attrs)
                                          (::layout/padding-right attrs))
                        ::layout/height (+ height delta-y
                                           (::layout/padding-top attrs)
                                           (::layout/padding-bottom attrs)))
        (assoc :body (map #(locate-element %1 %2 (offset-y %1)) body xs)))))

(defn align-in-block [ctx {:keys [attrs body] :as elem}]
  (let [heights (map #(elem/attr-value % ::layout/height) body)
        width (- (::layout/max-x ctx) (::layout/min-x ctx))
        delta-x (max (- (:width attrs width) width) 0)
        height (apply + heights)
        delta-y (max (- (:height attrs height) height) 0)
        offset-x (case (::layout/text-align attrs)
                   :center #(/ (- width (elem/attr-value % ::layout/width)) 2.0)
                   :right #(- width (elem/attr-value % ::layout/width))
                   (constantly 0))
        offset-y (case (::layout/vertical-align attrs)
                   :middle (/ delta-y 2.0)
                   :bottom delta-y
                   0)
        ys (reductions + offset-y heights)]
    (-> elem
        (elem/add-attrs ::layout/width (+ width delta-x
                                          (::layout/padding-left attrs)
                                          (::layout/padding-right attrs))
                        ::layout/height (+ height delta-y
                                           (::layout/padding-top attrs)
                                           (::layout/padding-bottom attrs)))
        (assoc :body (map #(locate-element %1 (offset-x %1) %2) body ys)))))
