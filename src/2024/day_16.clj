(ns day-16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.data.priority-map :refer [priority-map]]))

(def example (s/trim-newline (slurp (io/resource "day-16-example"))))
(def input (s/trim-newline (slurp (io/resource "day-16"))))

(defn parse-layout [layout]
  (let [extent-y (count layout)
        extent-x (count (first layout))]
    (reduce (fn [acc [x y]]
             (case (get-in layout [y x])
               \# (update acc :walls conj [x y])
               \S (assoc acc :start [x y])
               \E (assoc acc :end [x y])
               \. acc))
           {:walls #{}}
           (for [y (range 0 extent-y) x (range 0 extent-x)] [x y]))))

(defn turn [[x y]] [[(* -1 y) x] [y (* -1 x)]])

(defn solve [input]
  (let [{:keys [start end walls]} (parse-layout (vec (s/split-lines input)))]
    (loop [open (priority-map [(list start) [1 0]] 0) closed #{} best nil paths []]
      (if-let [[[path direction] score] (first open)]
        (let [open (dissoc open [path direction])
              pos (first path)
              closed (conj closed [pos direction])]
          (cond
            (or (walls pos) (and best (> score best)))
            (recur open closed best paths)

            (= end pos)
            (if (or (nil? best) (< score best))
              (recur open closed score [path])
              (recur open closed score (conj paths path)))

            :else
            (let [forward [[(conj path (mapv + pos direction)) direction] (inc score)]
                  turns (map #(vector [path %1] %2) (turn direction) (repeat (+ score 1000)))
                  next (filter (fn [[[path direction] _]] (not (closed [(first path) direction]))) (conj turns forward))]
              (recur (into open next) closed best paths))))
        [best paths]))))

(defn solve-1 [input]
  (first (solve input)))

(defn solve-2 [input]
  (count (reduce into #{} (second (solve input)))))
