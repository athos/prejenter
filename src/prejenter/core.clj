(ns prejenter.core)

(defn create-element [f]
  (fn [& args]
    (if (map? (first args))
      (f (first args) (rest args))
      (f {} args))))
