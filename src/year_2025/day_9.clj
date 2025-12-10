(ns year-2025.day-9
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(def example (slurp (io/resource "2025/day-9-example")))
(def input (slurp (io/resource "2025/day-9")))

(defn parse [input]
  (map (fn [line] (mapv #(Integer/parseInt %) (s/split line #","))) (s/split-lines input)))

(defn area [l r]
  (let [[x0 y0] l [x1 y1] r]
    (* (inc (abs (- x0 x1))) (inc (abs (- y1 y0))))))

(defn solve-1 [input]
  (->> (combo/combinations (parse input) 2) (map #(vector % (apply area %))) (sort-by second >) first second))

(defn tile-pairs [tiles]
  (take (count tiles) (partition 2 1 (cycle tiles))))

(defn outline [tiles]
  (reduce
   (fn [acc [[x0 y0] [x1 y1]]]
     (assert (or (= x0 x1) (= y0 y1)))
     (into acc
           (if (= x0 x1)
             (for [y (range (min y0 y1) (inc (max y0 y1)))] [x0 y])
             (for [x (range (min x0 x1) (inc (max x0 x1)))] [x y0]))))
   #{} (tile-pairs tiles)))

(defn extents [tiles]
  (reduce (fn [{[min-x min-y] :min [max-x max-y] :max} [p0 p1]]
            {:min [(min (or min-x p0) p0) (min (or min-y p1) p1)]
             :max [(max (or max-x p0) p0) (max (or max-y p1) p1)]})
          {:min [] :max []} tiles))

(defn prefill [[min-x min-y] [max-x max-y] init]
  (vec (repeat (+ 2 (- max-y min-y))
               (vec (repeat (+ 2 (- max-x min-x)) init)))))

(defn layout [tiles]
  (let [outline (outline tiles)
        {[max-x max-y] :max} (extents tiles)
        layout (prefill [0 0] [max-x max-y] 1)
        closed (prefill [0 0] [max-x max-y] false)]
    (loop [layout layout open [[0 0]] closed closed]
      (if (empty? open)
        layout
        (let [[[p0 p1] & open] open
              neighbors (->> [p0 p1] aoc/neighbors
                             (remove (fn [[p0 p1]] (get-in closed [p1 p0])))
                             (filter (fn [[p0 p1]] (get-in layout [p1 p0])))
                             (remove outline))]
          (recur (assoc-in layout [p1 p0] 0) (into open neighbors) (assoc-in closed [p1 p0] true)))))))

(defn solve-2 [input]
  (let [tiles (vec (parse input))
        mipmap (mapv (fn [[x y]] [(quot x 500) (quot y 500)]) tiles)
        layout (layout mipmap)
        legal? (memoize
                (fn [[p0 p1]]
                  (let [[x0 y0] (nth mipmap p0)
                        [x1 y1] (nth mipmap p1)]
                    (every? (fn [[x y]] (= 1 (get-in layout [y x])))
                            (for [x (range (min x0 x1) (inc (max x0 x1)))
                                  y (range (min y0 y1) (inc (max y0 y1)))]
                              [x y])))))]
    (->> (combo/combinations (range 0 (count tiles)) 2)
         (filter legal?)
         (map (partial map tiles))
         (map #(apply area %))
         (sort >)
         first)))
