(ns prejenter.agent)

(defprotocol Agent
  (get-state [this])
  (update-state! [this args])
  (slide [this page])
  (show [this image]))

(defn current-page [agent]
  (::current-page (get-state agent)))

(defn update-page! [agent f & args]
  (update-state! agent [update ::current-page f args]))

(defn goto-page! [agent page]
  (update-page! agent (constantly page))
  (when-let [image (slide agent page)]
    (show agent image)))

(defn start! [agent]
  (goto-page! agent 0))

(defn next-page!
  ([agent] (next-page! agent 1))
  ([agent n]
   (goto-page! agent (+ (current-page agent) n))))

(defn prev-page!
  ([agent] (prev-page! agent 1))
  ([agent n]
   (goto-page! agent (- (current-page agent) n))))
