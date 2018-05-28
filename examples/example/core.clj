(ns example.core
  (:require [clojure.java.io :as io])
  (:import [java.awt Color]
           [javax.imageio ImageIO]))

(def context
  {:width 640 :height 480
   :padding 20
   :font-family "Gill Sans"
   :font-size 30
   :color Color/BLACK
   :background-color Color/WHITE})

(def slides
  [[:slide
    [:title
     {:font-style :italic :font-weight :bold
      :padding-left 10 :padding-bottom 10
      :height 75 :text-align :center :vertical-align :middle}
     [:inline "Hello, " [:text {:color Color/RED :font-size 40} "World!"]]]
    [:items {:color Color/BLUE}
     [:text {:color Color/RED} "foo"]
     "bar"
     "baz"]
    [:lines {:color Color/GREEN}
     "hoge"
     "fuga"
     "piyo"]
    [:image {:src (ImageIO/read (io/resource "clojure.png"))
             :bottom 10 :right 10 :width 96 :height 96}]]])
