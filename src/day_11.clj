(ns day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as math]
            [clojure.walk :as walk]))

(def example (s/trim-newline (slurp (io/resource "day-11-example"))))
(def input (s/trim-newline (slurp (io/resource "day-11"))))

(defn parse [input] (mapv parse-long (s/split input #" ")))

(defn split-stone [stone]
  (let [s (str stone)]
    (mapv (comp parse-long str (partial reduce str)) (split-at (/ (count s) 2) s))))

(defn evolve [stone]
  (cond
    (zero? stone) 1
    (even? (int (inc (math/log10 stone)))) (split-stone stone)
    :else (* stone 2024)))

(def mem-evolve (memoize evolve))

(defn walk [stones] (if (vector? stones) stones (evolve stones)))

(defn solve-1 [input]
  (count (flatten (nth (iterate (partial walk/postwalk walk) (parse input)) 25))))

(defn solve-2 [input]
  (count (flatten (nth (iterate (partial walk/postwalk walk) (parse input)) 75))))

(defn solve-2 [input]
  (count (nth (iterate #(->> % (map mem-evolve) flatten) (parse input)) 75)))

(solve-2 input)
(map count (take 30 (iterate #(->> % (map mem-evolve) flatten) [0])))
