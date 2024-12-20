(ns day-20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [aoc :refer [traced-map traced-mapcat a* parse-layout]]))

(def example (s/trim-newline (slurp (io/resource "day-20-example"))))
(def input (s/trim-newline (slurp (io/resource "day-20"))))

(defn solve-1 [input]
  (let [{:keys [walls start end extents]} (parse-layout input)
        {:keys [touched goal]} (a* start end walls extents 10000)
        baseline (dec (count goal))]
    (count
     (filter #(>= % 100)
             (traced-map "solve"
                 #(let [{:keys [goal]} (a* start end (disj walls %) extents 10000)]
                    (- baseline (dec (count goal))))
               touched)))))

(defn bresenham-circle [r]
  (filter (fn [[_ d]] (<= d r))
          (map (fn [[x y]] [[x y] (+ (abs x) (abs y))])
               (for [x (range (* -1 r) (inc r)) y (range (* -1 r) (inc r))] [x y]))))

(def bresenham-circle-20 (bresenham-circle 20))

(defn solve-2 [input]
  (let [{:keys [floor walls start end extents]} (parse-layout input)
        floor (conj floor end)
        {:keys [goal]} (a* start end walls extents 10000)
        baseline (dec (count goal))
        cache (into {} (map (fn [[pos & rest]] [pos (count rest)])
                            (take-while seq (iterate next goal))))]
    (count
     (filter #(>= % 100)
             (traced-mapcat "solve"
                 (fn [prelude start]
                   (let [cheats (filter (comp floor first) (map (fn [[end d]] [(mapv + start end) d]) bresenham-circle-20))]
                     (map (fn [[pos d]] (- baseline (+ prelude d (cache pos)))) cheats)))
               (range (count goal)) goal)))))
