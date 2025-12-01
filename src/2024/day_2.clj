(ns day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-2-example")))
(def input (slurp (io/resource "day-2")))

(defn read-input [input]
  (map (fn [line]
         (map #(Integer/parseInt %) (s/split line #" ")))
       (s/split-lines input)))

(defn pairs [coll]
  (loop [coll coll acc []]
    (if (>= (count coll) 2)
      (recur (rest coll) (conj acc (take 2 coll)))
      acc)))

(defn dampen-report [report]
  (let [report (vec report)]
    (conj (for [i (range 0 (count report))]
           (concat (subvec report 0 i)
                   (subvec report (inc i))))
         report)))

(defn report-safe? [report]
  (let [diffs (map (fn [[x y]] (- x y)) (pairs report))]
    (and (or (every? #(< % 0) diffs) (every? #(> % 0) diffs))
         (every? #(<= 1 (abs %) 3) diffs))))

(defn dampened-report-safe? [report]
  (some true? (map report-safe? (dampen-report report))))

(defn solve-1 [input]
  (count (filter true? (map report-safe? (read-input input)))))

(defn solve-2 [input]
  (count (filter true? (map dampened-report-safe? (read-input input)))))

