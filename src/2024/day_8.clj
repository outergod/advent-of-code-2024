(ns day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :refer [combinations]]))

(def example (slurp (io/resource "day-8-example")))
(def input (slurp (io/resource "day-8")))

(defn read-input [input]
  (let [rows (s/split-lines input)
        extent-y (count rows)
        extent-x (count (first rows))
        coordinates (for [y (range 0 extent-y) x (range 0 extent-x)] [x y])]
    (reduce (fn [acc [x y]]
              (let [c (get-in rows [y x])]
                (if (= \. c)
                  acc
                  (update-in acc [:antennas c] (fn [coll] (conj (or coll #{}) [x y]))))))
            {:positions #{} :extents [extent-x extent-y]}
            coordinates)))

(defn solve-1 [input]
  (let [{:keys [antennas extents]} (read-input input)
        [x0 y0] extents]
    (count (into #{}
                 (filter (fn [[x y]] (and (<= 0 x (dec x0)) (<= 0 y (dec y0))))
                         (mapcat (fn [antenna]
                                   (mapcat (fn [[a0 a1]]
                                             (let [diff (mapv - a0 a1)]
                                               [(mapv + a0 diff) (mapv - a1 diff)]))
                                           (combinations antenna 2)))
                                 (vals antennas)))))))
(defn solve-2 [input]
  (let [{:keys [antennas extents]} (read-input input)
        [x0 y0] extents
        in-bounds? (fn [[x1 y1]] (and (<= 0 x1 (dec x0)) (<= 0 y1 (dec y0))))]
    (count (into #{}
                 (mapcat (fn [antenna]
                           (mapcat (fn [[a0 a1]]
                                     (let [diff (mapv - a0 a1)]
                                       (concat
                                        (take-while in-bounds? (iterate (fn [a] (mapv + a diff)) a0))
                                        (take-while in-bounds? (iterate (fn [a] (mapv - a diff)) a1)))))
                                   (combinations antenna 2)))
                         (vals antennas))))))
