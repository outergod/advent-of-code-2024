(ns day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-6-example")))
(def input (slurp (io/resource "day-6")))

(defn read-input [input]
  (into {} (apply concat
                  (map-indexed (fn [y line]
                                 (keep-indexed (fn [x c]
                                                 (case c \^ [[x y] :guard] \. [[x y] :floor] \# [[x y] :obstacle] \space nil)) line))
                               (s/split-lines input)))))

(read-input example)
