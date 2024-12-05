(ns day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-4-example")))
(def input (slurp (io/resource "day-4")))

(defn read-input [input]
  (into {}
        (apply concat
          (for [[y line] (map-indexed vector (s/split-lines input))]
            (for [[x c] (map-indexed vector line)]
              [[x y] c])))))

(defn make-crossword [s]
  (let [lines (s/split-lines s)]
    [(partial get-in lines) (count lines)]))

(def xmas-lookup
  (let [n (count "xmas")]
    (apply mapv vector (for [a (range 0 n)] [[a 0] [a a] [0 a] [(* -1 a) a] [(* -1 a) 0] [(* -1 a) (* -1 a)] [0 (* -1 a)] [a (* -1 a)]]))))


(defn solve-1 [input]
  (let [[crossword n] (make-crossword input)]
   (reduce + (for [x (range 0 n) y (range 0 n)]
               (count (filter #{"XMAS"} (map #(->> % (map (comp crossword (partial mapv + [x y]))) (apply str)) xmas-lookup)))))))

(defn solve-2 [input]
  (let [extent (count (s/split-lines input))
        crossword (read-input input)]
    (count (filter identity
                   (for [y (range 0 extent) x (range 0 extent)
                         :let [s1 (str (crossword [(dec x) (dec y)])
                                       (crossword [x y])
                                       (crossword [(inc x) (inc y)]))
                               s2 (str (crossword [(inc x) (dec y)])
                                       (crossword [x y])
                                       (crossword [(dec x) (inc y)]))]]
                     (every? #{"MAS" "SAM"} [s1 s2]))))))
