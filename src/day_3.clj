(ns day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-3-example")))
(def example-2 (slurp (io/resource "day-3-example-2")))
(def input (slurp (io/resource "day-3")))

(def pattern-1 #"mul\((\d+),(\d+)\)")
(def pre-pattern #"(?:mul\(\d+,\d+\)|do\(\)|don't\(\))")
(def fn-pattern #"(\w+)\(([^)]*)\)")
(def args-pattern #"^(\d+),(\d+)$")

(defn solve-1 [input]
  (reduce + (map (fn [[_ x y]] (* (Integer/parseInt x) (Integer/parseInt y)))
                 (re-seq pattern-1 input))))

(defn fn-seq [input]
  (map (partial re-find fn-pattern)
       (re-seq pre-pattern input)))

(defn solve-2 [input]
  (loop [do true acc 0 [[_ fn-name args] & rest] (fn-seq input)]
    (case fn-name
      "do" (recur true acc rest)
      "don't" (recur false acc rest)
      "mul" (if do
              (if-let [[_ x y] (re-find args-pattern args)]
                (recur true (+ acc (* (Integer/parseInt x) (Integer/parseInt y))) rest)
                (recur true acc rest))
              (recur false acc rest))
      nil acc)))
