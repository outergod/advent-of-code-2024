(ns day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-5-example")))
(def input (slurp (io/resource "day-5")))

(defn parse [s re]
  (map (fn [pair] (mapv #(Integer/parseInt %) (s/split pair re))) s))

(defn ordering-comp [ordering]
  (fn [left right]
    (if-let [order (ordering #{left right})]
      (= order [left right])
      false)))

(defn read-input [input]
  (let [[ordering _ pages] (partition-by #(= "" %) (s/split-lines input))
        ordering (into {} (map (fn [[left right]] [#{left right} [left right]])
                               (parse ordering #"\|")))
        pages (parse pages #",")]
    [(ordering-comp ordering) pages]))

(defn solve-1 [input]
  (let [[comp pages] (read-input input)]
    (reduce + (map #(if (= %1 %2) (nth %1 (int (/ (count %1) 2))) 0)
                   pages
                   (map #(sort comp %) pages)))))

(defn solve-2 [input]
  (let [[comp pages] (read-input input)]
    (reduce + (map #(if (not= %1 %2) (nth %2 (int (/ (count %2) 2))) 0)
                   pages
                   (map #(sort comp %) pages)))))
