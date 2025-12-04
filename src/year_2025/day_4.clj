(ns year-2025.day-4
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-4-example")))
(def input (slurp (io/resource "2025/day-4")))

(defn parse [input]
  (let [layout (s/split-lines input)
        width (count (first layout))
        height (count layout)]
    {:layout (mapv vec layout) :width width :height height}))

(defn neighbors-of [x y]
  (map (partial mapv + [x y]) aoc/neighbors-8))

(defn iterate-atlas [f atlas]
  (let [{:keys [layout width height]} atlas]
    (for [y (range 0 height) x (range 0 width)]
      (f x y (get-in layout [y x])))))

(defn find-free [atlas]
  (keep identity (iterate-atlas (fn [x y c]
                                  (when (and (= c \@)
                                             (> 4 (count (filter (fn [[x y]] (= \@ (get-in atlas [:layout y x])))
                                                                 (neighbors-of x y)))))
                                    [x y]))
                                atlas)))

(defn solve-1 [input] (count (find-free (parse input))))

(defn solve-2 [input]
  (loop [atlas (parse input) freed 0]
    (let [free (find-free atlas)]
      (if (> (count free) 0)
        (recur (reduce (fn [atlas [x y]] (assoc-in atlas [:layout y x] \.))
                       atlas free)
               (+ freed (count free)))
        freed))))
