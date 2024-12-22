(ns day-22
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [aoc :refer [pairs tuples]]))

(def example (s/trim-newline (slurp (io/resource "day-22-example"))))
(def input (s/trim-newline (slurp (io/resource "day-22"))))

(defn next-secret [secret]
  (let [mix (fn [x f] (bit-xor x (f x)))
        prune (fn [x] (bit-and x 16777215))]
    (-> secret
        (mix #(bit-shift-left % 6)) prune
        (mix #(bit-shift-right % 5)) prune
        (mix #(bit-shift-left % 11)) prune)))

(def secrets-seq (partial iterate next-secret))

(defn solve [secret] (nth (secrets-seq secret) 2000))

(defn solve-1 [input]
  (reduce + (map (comp solve parse-long) (s/split-lines input))))

(defn changesets [secret]
  (let [bananas (map #(mod % 10) (take 2001 (secrets-seq secret)))
        changes (map (fn [[x y]] (- y x)) (pairs bananas))]
    (reduce (fn [acc [k v]] (if (acc k) acc (assoc acc k v))) {} (map vector (map vec (tuples 4 changes)) (drop 4 bananas)))))

(defn search [secrets]
  (let [changesets (map changesets secrets)
        ks (into #{} (mapcat keys changesets))]
    (apply max (map (fn [k] (reduce + (map #(get % k 0) changesets))) ks))))

(defn solve-2 [input]
  (search (map parse-long (s/split-lines input))))
