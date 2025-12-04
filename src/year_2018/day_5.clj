(ns year-2018.day-5
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (slurp (io/resource "2018/day-5-example")))
(def input (slurp (io/resource "2018/day-5")))

(defn parse [input]
  (map (fn [c] {:type (-> c str s/lower-case keyword)
                :polarity (if (Character/isLowerCase c) :lower :upper)})
       (s/trim-newline input)))

(defn react [polymer]
  (loop [polymer polymer acc []]
    (let [[{type-a :type polarity-a :polarity :as unit}
           {type-b :type polarity-b :polarity}] polymer]
      (cond
        (empty? polymer) acc
        (and (= type-a type-b) (not= polarity-a polarity-b)) (recur (drop 2 polymer) acc)
        :else (recur (next polymer) (conj acc unit))))))

(defn solve-1 [input] (->> input parse (aoc/until-fixed react) count))

(defn polymer-types [polymer]
  (->> polymer (map :type) sort dedupe))

(defn remove-type [polymer type]
  (remove #(= type (:type %)) polymer))

(defn solve-2 [input]
  (let [polymer (parse input)]
    (apply min (pmap (fn [type] (->> type (remove-type polymer) (aoc/until-fixed react) count))
                     (polymer-types polymer)))))
