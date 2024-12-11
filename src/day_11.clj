(ns day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as math]))

(def example (s/trim-newline (slurp (io/resource "day-11-example"))))
(def input (s/trim-newline (slurp (io/resource "day-11"))))

(defn parse [input] (mapv parse-long (s/split input #" ")))

(defn split-stone [stone]
  (let [s (str stone)]
    (mapv (comp parse-long str (partial reduce str)) (split-at (/ (count s) 2) s))))

(defn evolve [stone]
  (cond
    (zero? stone) [1]
    (even? (int (inc (math/log10 stone)))) (split-stone stone)
    :else [(* stone 2024)]))

(def mem-evolve (memoize evolve))

(def solve (memoize (fn [n stone]
                      (if (= 1 n)
                        (count (mem-evolve stone))
                        (reduce + (map (partial solve (dec n)) (mem-evolve stone)))))))

(defn solve-1 [input]
  (reduce + (map (partial solve 25) (parse input))))

(defn solve-2 [input]
  (reduce + (map (partial solve 75) (parse input))))
