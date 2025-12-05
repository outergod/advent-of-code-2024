(ns year-2025.day-5
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-5-example")))
(def input (slurp (io/resource "2025/day-5")))

(defn parse-range [s]
  (let [r (s/split s #"-")]
    (mapv #(Long/parseLong %) r)))

(defn parse [input]
  (let [[ranges _ ids] (partition-by s/blank? (s/split-lines input))]
    {:ranges (mapv parse-range (take-while (complement s/blank?) ranges))
     :ids (mapv #(Long/parseLong %) ids)}))

(defn filter-fresh [database]
  (filter #(some (fn [[from to]] (<= from % to)) (database :ranges)) (database :ids)))

(defn solve-1 [input] (-> input parse filter-fresh count))

(defn merge-ranges [ranges]
  (loop [[range & ranges] ranges acc []]
    (if range
      (let [[range ranges] (reduce (fn [[[l0 r0] ranges] [l1 r1]]
                                     (if (or (<= l0 l1 r0) (<= l0 r1 r0) (<= l1 l0 r1) (<= l1 r0 r1))
                                       [[(min l0 l1) (max r0 r1)] ranges]
                                       [[l0 r0] (conj ranges [l1 r1])]))
                                   [range []] ranges)]
        (recur ranges (conj acc range)))
      acc)))

(defn solve-2 [input]
  (reduce (fn [acc [l r]] (+ acc (- (inc r) l))) 0 (->> input parse :ranges (aoc/until-fixed merge-ranges))))
