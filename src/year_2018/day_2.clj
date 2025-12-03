(ns year-2018.day-2
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2018/day-2-example")))
(def input (slurp (io/resource "2018/day-2")))

(defn parse [input]
  (map seq (s/split-lines input)))

(defn check-box [box]
  (let [fqs (vals (frequencies box))]
    [(if (some #{2} fqs) 1 0) (if (some #{3} fqs) 1 0)]))

(defn solve-1 [input]
  (reduce * (reduce (fn [[x1 y1] [x0 y0]] [(+ x1 x0) (+ y0 y1)]) (map check-box (parse input)))))

(defn similarity [box1 box2]
  (keep (fn [[l r]] (when (= l r) l))
        (map (partial vector) box1 box2)))

(defn solve-2 [input]
  (loop [[box & boxes] (parse input)]
    (if-let [id (some (fn [other]
                        (let [id (similarity box other)]
                          (when (= 1 (- (count box) (count id))) id)))
                      boxes)]
      (apply str id)
      (recur boxes))))
