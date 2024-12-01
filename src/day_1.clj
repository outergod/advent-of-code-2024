(ns day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-1-example")))
(def input (slurp (io/resource "day-1")))

(defn solve-1 [input]
  (let [rows (map (fn [line]
                  (map #(Integer/parseInt %) (s/split line #" +")))
                (s/split-lines input))
      left (map first rows)
      right (map second rows)]
  (reduce + (map (comp abs -) (sort left) (sort right)))))

(defn solve-2 [input]
  (let [rows (map (fn [line]
                  (map #(Integer/parseInt %) (s/split line #" +")))
                (s/split-lines input))
      left (map first rows)
      right (into {} (map (fn [[k v]] [k (count v)])
                          (group-by identity (map second rows))))]
  (reduce #(+ %1 (* %2 (or (right %2) 0))) 0 left)))
