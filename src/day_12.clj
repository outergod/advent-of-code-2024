(ns day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def example (s/trim-newline (slurp (io/resource "day-12-example"))))
(def input (s/trim-newline (slurp (io/resource "day-12"))))

(def moves [[-1 0] [0 1] [1 0] [0 -1]])

(defn read-input [input]
  (let [garden (mapv vec (s/split-lines input))]
    {:garden garden :coords (set (for [y (range (count garden)) x (range (count (first garden)))] [y x]))}))

(defn move [garden coord]
  (let [plant (get-in garden coord)]
    (reduce (fn [acc move]
              (let [coord-next (mapv + coord move)]
                (cond
                  (= plant (get-in garden coord-next)) (update acc :plants conj coord-next)
                  (#{[-1 0] [1 0]} move) (update acc :h-fences conj [coord coord-next])
                  (#{[0 -1] [0 1]} move) (update acc :v-fences conj [coord coord-next]))))
            {:plants #{} :v-fences #{} :h-fences #{}}
            moves)))

(defn explore [garden coord]
  (loop [open #{coord}
         closed #{}
         acc-plants #{coord}
         acc-h-fences #{}
         acc-v-fences #{}]
    (if-let [coord (first open)]
      (let [{:keys [plants h-fences v-fences]} (move garden coord)
            closed (conj closed coord)]
        (recur (set/difference (into open plants) closed) closed
               (into acc-plants plants) (into acc-h-fences h-fences) (into acc-v-fences v-fences)))
      {:plants acc-plants :v-fences acc-v-fences :h-fences acc-h-fences})))

(defn solve-1 [input]
  (let [{:keys [garden coords]} (read-input input)]
    (loop [open coords acc 0]
      (if-let [coord (first open)]
        (let [{:keys [plants h-fences v-fences]} (explore garden coord)]
          (recur (set/difference open plants) (+ acc (* (count plants) (+ (count h-fences) (count v-fences))))))
        acc))))

(defn fence-sides [fences moves]
  (loop [open fences sides 0]
    (if-let [fence (first open)]
      (let [fence (mapcat (fn [move] (take-while fences (iterate #(mapv (partial mapv + move) %) fence))) moves)]
        (recur (set/difference open (set fence)) (inc sides)))
      sides)))

(defn solve-2 [input]
  (let [{:keys [garden coords]} (read-input input)]
    (loop [open coords acc 0]
      (if-let [coord (first open)]
        (let [{:keys [plants h-fences v-fences]} (explore garden coord)]
          (recur (set/difference open plants) (+ acc (* (count plants)
                                                        (+ (fence-sides h-fences [[0 1] [0 -1]])
                                                           (fence-sides v-fences [[1 0] [-1 0]]))))))
        acc))))
