(ns prejenter.utils
  (:import [java.awt Font]))

(defn attr-value
  ([ctx attrs attr-name]
   (attr-value ctx attrs attr-name nil))
  ([ctx attrs attr-name default-value]
   (or (get attrs attr-name)
       (get ctx attr-name)
       default-value)))
