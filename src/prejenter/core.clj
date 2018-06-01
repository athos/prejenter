(ns prejenter.core)

(defn create-element [f]
  (fn [& args]
    (if (map? (first args))
      (apply f (first args) (rest args))
      (apply f {} args))))
