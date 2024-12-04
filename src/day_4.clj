(ns day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "day-4-example")))
(def input (slurp (io/resource "day-4")))

(def xmas-re #"XMAS")

(defn read-input [input]
  (into {}
        (apply concat
          (for [[y line] (map-indexed vector (s/split-lines input))]
            (for [[x c] (map-indexed vector line)]
              [[x y] c])))))

(read-input example)

(defn crossword-strings [crossword extent]
  (concat
   (for [y (range 0 extent)]
     (apply str (for [x (range 0 extent)]
        (crossword [x y]))))
   (for [y (range 0 extent)]
     (apply str (for [x (reverse (range 0 extent))]
        (crossword [x y]))))
   (for [x (range 0 extent)]
     (apply str (for [y (range 0 extent)]
        (crossword [x y]))))
   (for [x (range 0 extent)]
     (apply str (for [y (reverse (range 0 extent))]
        (crossword [x y]))))
   (for [y (range 0 extent)]
     (apply str (for [a (range 0 extent) :let [c (crossword [a (+ y a)])] :while c] c)))
   (for [x (range 1 extent)]
     (apply str (for [a (range 0 extent) :let [c (crossword [(+ x a) a])] :while c] c)))
   (for [y (range 0 extent)]
     (apply str (for [a (reverse (range 0 extent)) :let [c (crossword [(+ x a) a])] :while c] c)))))

(reduce + (map count-xmas (crossword-strings (read-input example) (count (s/split-lines example)))))

(defn count-xmas [s] (count (re-seq xmas-re s)))

(defn solve-1 [input]
  (let [rows (read-input input)
        columns (apply mapv str rows)]
    (reduce + (map count-xmas (concat rows columns (map s/reverse rows) (map s/reverse columns))))))

(let [rows (read-input example)
      columns (apply mapv str rows)]
  (count (concat rows columns (map s/reverse rows) (map s/reverse columns))))

(solve-1 example)
