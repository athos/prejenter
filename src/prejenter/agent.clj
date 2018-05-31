(ns prejenter.agent)

(defprotocol Agent
  (current-page [this])
  (update-page! [this page])
  (slide [this page])
  (show [this image]))

(defn goto-page! [agent page]
  (update-page! agent page)
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
