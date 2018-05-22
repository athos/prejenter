(ns prejenter.element)

(defrecord Element [tag attrs body])

(defn attr-value [elem attr-name]
  (get-in elem [:attrs attr-name]))

(defn add-attrs [elem & attrs]
  (reduce (fn [elem [attr-name attr-value]]
            (assoc-in elem [:attrs attr-name] attr-value))
          elem
          (partition 2 attrs)))
