(ns prejenter.repl
  (:refer-clojure :exclude [find-var])
  (:require [prejenter.agent :as agent]
            [prejenter.agent.images :as images]))

(def ^:private state (atom {}))

(defn current-agent []
  (:current-agent @state))

(defn- set-current-agent! [agent]
  (swap! state assoc :current-agent agent))

(defn- find-var [ns name default]
  (or (when ns
        (let [kname (keyword (clojure.core/name name))]
          (or (some->> (vals (ns-publics ns))
                       (filter #(get (meta %) kname))
                       first)
              (ns-resolve ns name))))
      default))

(defn- make-agent [{:keys [ns context slides]}]
  (when ns
    (require ns :reload))
  (let [context (find-var ns 'context context)
        slides (find-var ns 'slides slides)]
    (images/make-images-agent @context @slides)))

(defn start! [& {:as opts}]
  (swap! state assoc :prev-opts opts)
  (let [agent (make-agent opts)]
    (set-current-agent! agent)
    (agent/start! agent)))

(defn reload! []
  (let [page (agent/current-page (current-agent))
        agent (make-agent (:prev-opts @state))]
    (set-current-agent! agent)
    (agent/goto-page! agent page)))

(defn next!
  ([] (next! 1))
  ([n]
   (agent/next-page! (current-agent) n)))

(defn prev!
  ([] (prev! 1))
  ([n]
   (agent/prev-page! (current-agent) n)))

(defn goto! [page]
  (agent/prev-page! (current-agent) page))
