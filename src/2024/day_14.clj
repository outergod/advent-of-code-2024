(ns day-14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-14-example"))))
(def example-extents [11 7])
(def input (s/trim-newline (slurp (io/resource "day-14"))))
(def input-extents [101 103])

(def re #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")

(defn parse-robot [s]
  (let [[x0 y0 x1 y1] (map parse-long (next (re-matches re s)))]
    {:position [x0 y0] :velocity [x1 y1]}))

(defn parse-input [s] (map parse-robot (s/split-lines s)))

(defn visualize [state]
  (let [{:keys [extents robots]} state
        [extent-x extent-y] extents
        robots (group-by :position robots)]
    (s/join "\n"
            (map (fn [y]
                   (apply str (map (fn [x]
                                     (let [n (count (robots [x y]))]
                                       (if (zero? n) \. (first (str n)))))
                                   (range extent-x))))
                 (range extent-y)))))

(defn move
  ([robot] (move robot 1))
  ([robot n] (update robot :position (partial mapv #(+ %2 (* %1 n)) (robot :velocity)))))

(defn wrap [robot extents]
  (update robot :position (partial mapv #(mod %2 %1) extents)))

(defn solve-1 [input extents]
  (let [[div-x div-y] (map #(int (/ % 2)) extents)
        robots (map #(wrap (move % 100) extents) (parse-input input))
        quadrants (group-by (fn [{[x y] :position}]
                              [(cond (< x div-x) :left (> x div-x) :right :else :out)
                               (cond (< y div-y) :top (> y div-y) :bottom :else :out)])
                            robots)]
    (println)
    (println (visualize {:robots robots :extents extents}))
    (reduce * (map (comp count quadrants) [[:left :top] [:right :top] [:left :bottom] [:right :bottom]]))))

(defn solve-2 []
  (loop [n 0 robots (parse-input input)]
    (println)
    (println "Iteration " n)
    (spit (str "iteration-" n ".txt") (visualize {:robots robots :extents input-extents}))
    (when (< n 10000)
      (recur (inc n) (map #(wrap (move %) input-extents) robots)))))
