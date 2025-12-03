(ns year-2018.day-1
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2018/day-1-example")))
(def input (slurp (io/resource "2018/day-1")))

(defn parse [input]
  (map #(Integer/parseInt %) (s/split-lines input)))

(defn solve-1 [input]
  (reduce + (parse input)))

(defn solve-2 [input]
  (loop [freq 0 [change & cs] (cycle (parse input)) seen #{}]
    (let [freq (+ freq change)]
      (if (seen freq) freq (recur freq cs (conj seen freq))))))
