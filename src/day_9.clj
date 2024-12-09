(ns day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-9-example"))))
(def input (s/trim-newline (slurp (io/resource "day-9"))))

(defn make-disk-1 [input]
  (let [parsed (map #(-> % str parse-long) input)
        disk (int-array (reduce + parsed) -1)]
    (loop [i 0 n 0 flip? false [cur & parsed] parsed]
      (if cur
        (if flip?
          (recur (+ i cur) n false parsed)
          (do
            (doseq [x (range i (+ i cur))]
              (aset-int disk x n))
            (recur (+ i cur) (inc n) true parsed)))
        disk))))

(defn defrag-1 [disk]
  (loop [i 0 j (dec (count disk))]
    (if (< i j)
      (if (= -1 (aget disk i))
        (do
          (aset-int disk i (aget disk j))
          (aset-int disk j -1)
          (recur (inc i) (some #(and (not= -1 (aget disk %)) %)
                               (reverse (range j)))))
        (recur (inc i) j))
      disk)))

(defn solve-1 [input]
  (let [disk (defrag-1 (make-disk-1 input))]
    (reduce + (map * (range) (take-while #(not= % -1) disk)))))

(let [parsed (map #(-> % str parse-long) example)
      disk (mapv #(repeat %2 %1) (range) (take-nth 2 parsed))
      space (take-nth 2 (rest parsed))]
  [disk space])
