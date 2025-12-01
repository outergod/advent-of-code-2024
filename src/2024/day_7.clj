(ns day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-7-example")))
(def input (slurp (io/resource "day-7")))

(defn read-input [input]
  (map
   (fn [line]
     (let [[result ops] (s/split line #": ")]
       [(parse-long result) (mapv parse-long (s/split ops #" "))]))
   (s/split-lines input)))

(defn dts [goal ops fns]
  (letfn [(traverse [acc [op & ops]]
            (cond
              (> acc goal) false
              op (some (fn [f] (traverse (f acc op) ops)) fns)
              :else (= goal acc)))]
    (traverse (first ops) (rest ops))))

(defn solve [input fns]
 (reduce + (map (fn [[goal ops]] (if (dts goal ops fns) goal 0)) (read-input input))))

(defn solve-1 [input] (solve input [+ *]))

(defn || [x y] (parse-long (str x y)))

(defn solve-2 [input] (solve input [+ * ||]))
