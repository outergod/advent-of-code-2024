(ns year-2025.day-2
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as math]))

(def example (slurp (io/resource "2025/day-2-example")))
(def input (slurp (io/resource "2025/day-2")))

(defn parse-range [s]
  (let [[l r] (map #(Long/parseLong %) (s/split s #"-"))]
    (range l (inc r))))

(defn parse [s]
  (mapcat parse-range (-> s s/trim-newline (s/split #","))))

(defn pattern [s n]
  (let [p (subs s 0 n)]
    (apply str (repeat (/ (count s) n) p))))

(defn invalid-1? [n]
  (let [s (str n) p (subs s 0 (quot (count s) 2))]
    (= s (str p p))))

(defn invalid-2? [n]
  (let [s (str n)]
    (some #{s} (map #(pattern s %) (butlast (aoc/factors (count s)))))))

(defn solve-1 [s]
  (reduce + (filter invalid-1? (parse s))))

(defn solve-2 [s]
  (reduce + (filter invalid-2? (parse s))))
