(ns prejenter.repl
  (:require [clojure.java.io :as io]
            [prejenter.agent :as agent]
            [prejenter.agent.images :as images]
            [prejenter.export :as export]
            [prejenter.utils :as utils]))

(def ^:private state (atom {}))

(defn current-agent []
  (:current-agent @state))

(defn- set-current-agent! [agent]
  (swap! state assoc :current-agent agent))

(defn- make-agent [{:keys [ns context slides]}]
  (let [[context slides] (if ns
                           (utils/with-ns-slides ns vector)
                           [context slides])]
    (images/make-images-agent @context @slides)))

(defn start!
  ([ns] (start! :ns ns))
  ([opt-key opt-val & {:as opts}]
   (let [opts (assoc opts opt-key opt-val)]
     (swap! state assoc :prev-opts opts)
     (let [agent (make-agent opts)]
       (set-current-agent! agent)
       (agent/start! agent)))))

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
  (agent/goto-page! (current-agent) page))

(defn export!
  ([ns file] (export! :ns ns :file file))
  ([opt-key opt-val & {:as opts}]
   (let [opts (assoc opts opt-key opt-val)
         [context slides] (if-let [ns (:ns opts)]
                            (utils/with-ns-slides ns vector)
                            [(:context opts) (:slides opts)])]
     (export/export (io/file (:file opts)) @context @slides))))
