(ns day-18
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def example (s/trim-newline (slurp (io/resource "day-18-example"))))
(def input (s/trim-newline (slurp (io/resource "day-18"))))

(defn parse [input]
  (mapv #(mapv parse-long (s/split % #",")) (s/split-lines input)))

(defn euclidian-distance
  ([[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
  ([[x0 y0] [x1 y1]] (euclidian-distance [(- x1 x0) (- y1 y0)])))

(defn in-limits? [[x y] [w h]] (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn neighbors [[x y]]
  (map (fn [[a b]] [(+ x a) (+ y b)]) [[0 1] [1 0] [0 -1] [-1 0]]))

(defn a*-seq [[x0 y0] [x1 y1] [w h] obstacles]
  (let [in-limits? #(in-limits? % [w h])]
    (letfn [(h [[x y]] (euclidian-distance [x y] [x1 y1]))
            (f [[x y] g] (+ g (h [x y])))
            (n [[x y] g parents fringe closed]
              (let [parents (conj parents [x y])]
                (keep (fn [[x y]]
                        (let [g (inc g)
                              cost (f [x y] g)
                              [prev-cost] (fringe [x y])]
                          (when-not (and prev-cost (<= prev-cost cost))
                            [[x y] [cost g parents]])))
                      (filter (fn [[x y]] (in-limits? [x y]))
                              (remove closed (neighbors [x y]))))))]
      (iterate
       (fn [[state fringe closed goal]]
         (if (= state :stop)
           [state fringe closed goal]
           (let [[[x y] [cost distance parents] :as node] (peek fringe)
                 [[x-goal y-goal] [cost-goal _ parents-goal] :as node-goal] (peek goal)]
             (cond (= [x y] [x1 y1]) [:continue (pop fringe) closed (conj goal node)]
                   (and (not-empty node-goal)
                        (or (empty? node)
                            (< cost-goal cost))) [:stop (conj parents-goal [x-goal y-goal])]
                   node [:continue (into (pop fringe) (n [x y] distance parents fringe closed)) (conj closed [x y]) goal]))))
       [:continue
        (priority-map-keyfn first [x0 y0] [(h [x0 y0]) 0 []])
        obstacles
        (priority-map-keyfn first)]))))

(defn a* [start goal obstacles width height limit]
  (let [[state path] (last (take limit (a*-seq start goal [width height] obstacles)))]
    (if (= state :stop) path nil)))

(defn solve-example []
  (dec (count (a* [0 0] [6 6] (set (take 12 (parse example))) 7 7 1000))))

(defn solve-1 []
  (dec (count (a* [0 0] [70 70] (set (take 1024 (parse input))) 71 71 10000))))

(defn solve-2 []
  (some (fn [[path byte]] (when-not path byte))
        (map #(vector (a* [0 0] [70 70] (set %) 71 71 10000) (last %))
         (reductions conj [] (parse input)))))
