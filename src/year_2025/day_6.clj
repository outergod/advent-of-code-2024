(ns year-2025.day-6
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-6-example")))
(def input (slurp (io/resource "2025/day-6")))

(defn parse-numbers [s]
  (map #(Integer/parseInt %) (re-seq #"\d+" s)))

(defn parse-ops [s]
  (map (comp resolve symbol) (re-seq #"\S" s)))

(defn parse-1 [input]
  (let [lines (s/split-lines input)]
    (apply map (partial vector) (parse-ops (last lines)) (map parse-numbers (butlast lines)))))

(defn solve [problems]
  (reduce + (map (fn [[op & nums]] (apply op nums)) problems)))

(defn solve-1 [input]
  (-> input parse-1 solve))

(defn parse-2 [input]
  (let [lines (s/split-lines input)]
    (map (partial apply vector)
         (parse-ops (last lines))
         (map (partial map #(Integer/parseInt %))
              (remove (comp s/blank? first)
                      (partition-by s/blank? (apply map (comp s/trim str) (butlast lines))))))))

(defn solve-2 [input]
  (-> input parse-2 solve))
