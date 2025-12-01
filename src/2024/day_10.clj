(ns day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-10-example"))))
(def input (s/trim-newline (slurp (io/resource "day-10"))))

(def moves [[-1 0] [0 1] [1 0] [0 -1]])

(defn find-trailheads [topo]
  (let [coords (for [y (range (count topo)) x (range (count (first topo)))] [y x])]
    (reduce (fn [acc coord] (if (= 0 (get-in topo coord)) (conj acc coord) acc)) [] coords)))

(defn read-input [input]
  (let [topo (mapv #(mapv (comp parse-long str) %) (s/split-lines input))]
    {:topo topo :trailheads (find-trailheads topo)}))

(defn legal-moves [topo coord]
  (let [height (get-in topo coord)]
    (filter #(= (inc height) (get-in topo %)) (map (partial mapv + coord) moves))))

(defn is-peak? [topo coord] (= 9 (get-in topo coord)))

(defn trailhead-score-1 [topo coord]
  (loop [open (set (legal-moves topo coord))
         closed #{coord}
         peaks #{}]
    (if-let [coord (first open)]
      (if (is-peak? topo coord)
        (recur (disj open coord) (conj closed coord) (conj peaks coord))
        (let [next (filter (comp not closed) (legal-moves topo coord))]
          (recur (into (disj open coord) next) (conj closed coord) peaks)))
      (count peaks))))

(defn solve-1 [input]
  (let [{:keys [topo trailheads]} (read-input input)]
    (reduce + (map (partial trailhead-score-1 topo) trailheads))))

(defn trailhead-score-2 [topo coord]
  (if (is-peak? topo coord)
    1
    (reduce + (map (partial trailhead-score-2 topo) (legal-moves topo coord)))))

(defn solve-2 [input]
  (let [{:keys [topo trailheads]} (read-input input)]
    (reduce + (map (partial trailhead-score-2 topo) trailheads))))
