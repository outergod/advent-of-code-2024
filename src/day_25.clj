(ns day-25
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-25-example"))))
(def input (s/trim-newline (slurp (io/resource "day-25"))))

(defn parse [input]
  (group-by :type
            (map
             (fn [schema]
               {:type (if (= \# (first (first schema))) :lock :key)
                :heights (for [i (range (count (first schema)))]
                           (count (filter #{\#} (map #(nth % i) (drop-last (drop 1 schema))))))})
             (take-nth 2 (partition-by s/blank? (s/split-lines input))))))

(defn force [keys locks]
  (reduce
   (fn [acc {key :heights}]
     (+ acc (reduce (fn [acc {lock :heights}] (+ acc (if (every? #(< % 6) (map + key lock)) 1 0))) 0 locks)))
   0 keys))

(defn solve [input]
  (let [{:keys [key lock]} (parse input)]
    (force key lock)))
