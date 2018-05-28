(ns prejenter.core
  (:require [prejenter.agent :as agent]
            [prejenter.agent.images :as images]))

(def ^:private current-agent (atom nil))

(defn- find-name [ns name]
  (let [kname (keyword (clojure.core/name name))]
    (or (some->> (vals (ns-publics ns))
                 (filter #(get (meta %) kname))
                 first
                 deref)
        (some-> (ns-resolve ns name) deref))))

(defn start! [& {:keys [ns context slides agent]}]
  (when ns
    (require ns :reload))
  (let [context (or (some-> ns (find-name 'context))
                    context)
        slides (or (some-> ns (find-name 'slides))
                   slides)
        agent (or agent (images/make-images-agent context slides))]
    (reset! current-agent agent)
    (agent/start! agent)))

(defn next!
  ([] (next! 1))
  ([n]
   (agent/next-page! @current-agent n)))

(defn prev!
  ([] (prev! 1))
  ([n]
   (agent/prev-page! @current-agent n)))

(defn goto! [page]
  (agent/prev-page! @current-agent page))
