(ns day-13
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-13-example"))))
(def input (s/trim-newline (slurp (io/resource "day-13"))))

(def button-re #"Button .: X\+(\d+), Y\+(\d+)")
(def prize-re #"Prize: X=(\d+), Y=(\d+)")

(defn numbers [match] (mapv parse-long (next match)))

(defn read-machine [lines]
  (let [[a b prize] lines
        a (numbers (re-matches button-re a))
        b (numbers (re-matches button-re b))
        prize (numbers (re-matches prize-re prize))]
    [a b prize]))

(defn read-input [input]
  (map read-machine (partition-all 4 (s/split-lines input))))

(defn solve [machine]
  (let [[[a-x b-x] [a-y b-y] [prize-a prize-b]] machine
        d (- (* a-x b-y) (* b-x a-y))
        d-x (- (* prize-a b-y) (* prize-b a-y))
        d-y (- (* a-x prize-b) (* b-x prize-a))]
    [(/ d-x d) (/ d-y d)]))

(defn solve-1 [input]
  (reduce + (map (fn [[a-n b-n]] (+ (* a-n 3) b-n))
                 (filter (partial every? integer?) (map solve (read-input input))))))

(defn solve-2 [input]
  (reduce + (map (fn [[a-n b-n]] (+ (* a-n 3) b-n))
                 (filter (partial every? integer?)
                         (map (fn [[x y prizes]]
                                (solve [x y (mapv (partial + 10000000000000) prizes)]))
                              (read-input input))))))
