(ns year-2025.day-1
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-1-example")))
(def input (slurp (io/resource "2025/day-1")))

(defn convert [input]
  (let [dir (subs input 0 1)
        n (Integer/parseInt (subs input 1))]
    (case dir
      "R" n
      "L" (* n -1))))

(defn solve-1 [input]
  (count (filter #{0} (reductions (fn [acc x] (mod (+ acc x 100) 100))
                                  50 (->> input s/split-lines (map convert))))))

(defn solve-2 [input]
  (reduce (fn [[x0 acc] x]
            (let [y (+ x0 x)]
              [(mod y 100) (+ acc (quot (if (neg? x) (+ (mod (- 100 x0) 100) (* -1 x)) y) 100))]))
          [50 0] (->> input s/split-lines (map convert))))
