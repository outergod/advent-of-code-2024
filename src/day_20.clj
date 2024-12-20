(ns day-20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [aoc :refer [traced-mapcat a* parse-layout]]))

(def example (s/trim-newline (slurp (io/resource "day-20-example"))))
(def input (s/trim-newline (slurp (io/resource "day-20"))))

(defn bresenham-circle [r]
  (filter (fn [[_ d]] (<= d r))
          (map (fn [[x y]] [[x y] (+ (abs x) (abs y))])
               (for [x (range (* -1 r) (inc r)) y (range (* -1 r) (inc r))] [x y]))))

(defn solve [input radius]
  (let [{:keys [floor walls start end extents]} (parse-layout input)
        circle (bresenham-circle radius)
        floor (conj floor end)
        {:keys [goal]} (a* start end walls extents 10000)
        baseline (dec (count goal))
        cache (into {} (map (fn [[pos & rest]] [pos (count rest)])
                            (take-while seq (iterate next goal))))]
    (count
     (filter #(>= % 100)
             (traced-mapcat "solve"
                 (fn [prelude start]
                   (let [cheats (filter (comp floor first) (map (fn [[end d]] [(mapv + start end) d]) circle))]
                     (map (fn [[pos d]] (- baseline (+ prelude d (cache pos)))) cheats)))
               (range (count goal)) goal)))))

(defn solve-1 [input] (solve input 2))
(defn solve-2 [input] (solve input 20))
