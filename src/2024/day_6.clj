(ns day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-6-example")))
(def input (slurp (io/resource "day-6")))

(defn read-input [input]
  (let [rows (s/split-lines input)
        extent-y (count rows)
        extent-x (count (first rows))
        coordinates (for [y (range 0 extent-y) x (range 0 extent-x)] [x y])
        direction (cycle [[0 -1] [1 0] [0 1] [-1 0]])]
    (reduce (fn [acc [x y]]
              (case (get-in rows [y x])
                \^ (-> acc (assoc :guard [x y]) (update :floor conj [x y]))
                \# (update acc :obstacles conj [x y])
                \. (update acc :floor conj [x y])
                acc))
            {:obstacles #{} :floor #{} :direction direction} coordinates)))

(defn solve-1 [input]
  (let [{:keys [obstacles floor guard direction]} (read-input input)]
    (loop [guard guard direction direction visited #{guard}]
      (let [new (mapv + guard (first direction))]
        (cond
          (floor new) (recur new direction (conj visited new))
          (obstacles new) (recur guard (next direction) visited)
          :else (count visited))))))

(defn solve-2 [input]
  (let [{:keys [obstacles floor guard direction]} (read-input input)
        solve (fn [floor obstacles]
                (loop [guard guard direction direction visited #{[guard (first direction)]}]
                  (let [new (mapv + guard (first direction))
                        visit [new (first direction)]]
                    (cond
                      (visited visit) true
                      (floor new) (recur new direction (conj visited visit))
                      (obstacles new) (recur guard (next direction) visited)
                      :else false))))]
    (count (filter identity (map (fn [pos] (solve (disj floor pos) (conj obstacles pos)))
                                 (disj floor guard))))))
