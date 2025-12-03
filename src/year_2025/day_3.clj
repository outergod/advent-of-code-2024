(ns year-2025.day-3
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-3-example")))
(def input (slurp (io/resource "2025/day-3")))

(defn parse [s]
  (map (fn [line] (map #(Character/digit % 10) line)) (s/split-lines s)))

(defn merge-batteries [batteries]
  (reduce + (map-indexed (fn [i b] (long (* (Math/pow 10 i) b))) (reverse batteries))))

(defn max-joltage [bank n]
  (merge-batteries
   (reduce (fn [batteries x]
             (let [batteries (conj batteries x)]
               (loop [[x y] batteries [_ & bs] batteries acc []]
                 (cond
                   (nil? y) acc
                   (> y x) (into [] (concat acc bs))
                   :else (recur bs bs (conj acc x))))))
           (repeat n 0) (vec bank))))

(defn solve-1 [input]
  (reduce + (map #(max-joltage % 2) (parse input))))

(defn solve-2 [input]
  (reduce + (map #(max-joltage % 12) (parse input))))
