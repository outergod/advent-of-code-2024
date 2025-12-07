(ns year-2025.day-7
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2025/day-7-example")))
(def input (slurp (io/resource "2025/day-7")))

(defn parse [input]
  (let [layout (mapv vec (s/split-lines input))]
    (merge {:layout layout}
           (reduce (fn [data [x y]]
                     (case (get-in layout [y x])
                       \^ (update data :splitters conj [x y])
                       \S (assoc data :start [x y])
                       data))
                   {:splitters #{}}
                   (for [x (range 0 (count (first layout))) y (range 0 (count layout))] [x y])))))

(defn extend-beam [beam layout]
  (loop [[x y] beam]
    (case (get-in layout [y x])
      (\S \.) (recur [x (inc y)])
      \^ [x y]
      nil)))

(defn simulate [layout]
  (let [{:keys [layout splitters start]} layout]
    (loop [[beam & beams] [start] splitters (set splitters) activated #{}]
      (if (nil? beam)
        activated
        (if-let [[x y] (splitters (extend-beam beam layout))]
          (recur (conj beams [(dec x) y] [(inc x) y])
                 (disj splitters [x y])
                 (conj activated [x y]))
          (recur beams splitters activated))))))

(defn solve-1 [input] (-> input parse simulate count))

(defn simulate-quantum [layout]
  (let [{:keys [layout start]} layout
        simulate (fn [f beam]
                   (if-let [[x y] (extend-beam beam layout)]
                     (+ (f [(inc x) y]) (f [(dec x) y]))
                     1))
        simulate' (aoc/fix (memoize simulate))]
    (simulate' start)))

(defn solve-2 [input] (-> input parse simulate-quantum))
