(ns year-2018.day-3
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2018/day-3-example")))
(def input (slurp (io/resource "2018/day-3")))

(def claim-re #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-claim [s]
  (let [[claim id left top width height] (re-find claim-re s)]
    {:claim claim
     :id (Integer/parseInt id)
     :pos [(Integer/parseInt left) (Integer/parseInt top)]
     :extents [(Integer/parseInt width) (Integer/parseInt height)]}))

(defn expand-claim [claim]
  (let [{[x y] :pos [w h] :extents id :id} claim]
    (for [x (range x (+ x w)) y (range y (+ y h))] [x y id])))

(defn parse [input]
  (map parse-claim (s/split-lines input)))

(def fabric (vec (repeat 1000 (vec (repeat 1000 0)))))

(defn solve-1 [input]
  (count
   (filter #(> % 1)
           (apply concat
                  (reduce
                   (fn [fabric [x y _]] (update-in fabric [y x] inc))
                   fabric (mapcat expand-claim (parse input)))))))

(defn solve-2 [input]
  (let [claims (parse input)]
    (reduce
     (fn [claims ids]
       (if (> (count ids) 1)
         (reduce dissoc claims ids)
         claims))
     (into {} (map (fn [{id :id :as claim}] [id claim]) claims))
     (apply concat
            (reduce
             (fn [fabric [x y id]] (update-in fabric [y x] conj id))
             fabric (mapcat expand-claim claims))))))
