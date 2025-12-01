(ns day-19
  (:require
   [aoc :refer [traced-map fix]]
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-19-example"))))
(def input (s/trim-newline (slurp (io/resource "day-19"))))

(defn parse [input]
  (let [[towels _ & patterns] (s/split-lines input)
        towels (group-by first (s/split towels #", "))]
    [towels patterns]))

(defn solve-1 [input]
  (let [[towels patterns] (parse input)
        solve
        (fn [solve pattern]
          (if (empty? pattern)
            true
            (some #(and (s/starts-with? pattern %)
                        (solve (subs pattern (count %))))
                  (towels (first pattern)))))
        solve' (fix (memoize solve))]
    (count (filter true? (traced-map "solve" solve' patterns)))))

(defn solve-2 [input]
  (let [[towels patterns] (parse input)
        solve
        (fn [solve pattern]
          (if (empty? pattern)
            1
            (reduce (fn [acc x]
                      (if (s/starts-with? pattern x)
                        (+ acc (solve (subs pattern (count x))))
                        acc))
                    0
                    (towels (first pattern)))))
        solve' (fix (memoize solve))]
    (reduce + (traced-map "solve" solve' patterns))))
