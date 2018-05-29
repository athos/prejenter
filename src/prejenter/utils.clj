(ns prejenter.utils
  (:refer-clojure :exclude [find-var]))

(defn- find-var [ns name]
  (when ns
    (let [kname (keyword (clojure.core/name name))]
      (or (some->> (vals (ns-publics ns))
                   (filter #(get (meta %) kname))
                   first)
          (ns-resolve ns name)))))

(defn with-ns-slides [ns f]
  (require ns :reload)
  (f (find-var ns 'context)
     (find-var ns 'slides)))
