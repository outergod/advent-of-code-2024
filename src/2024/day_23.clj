(ns day-23
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.set :as set]))

(def example (s/trim-newline (slurp (io/resource "day-23-example"))))
(def input (s/trim-newline (slurp (io/resource "day-23"))))

(defn parse [input]
  (reduce (fn [acc [left right]]
            (let [acc (reduce
                       (fn [acc [left right]] (update-in acc [:networks left] #(conj (or %1 #{}) %2) right))
                       acc [[left right] [right left]])]
              (update acc :pairs conj #{left right})))
          {:pairs #{} :networks {}}
          (map #(s/split % #"-") (s/split-lines input))))

(defn solve-t [{:keys [pairs networks]}]
  (into #{} (mapcat (fn [[k v]] (map #(set (cons k %)) (filter (comp pairs set) (combinations v 2))))
                    (filter (fn [[k _]] (= \t (first k))) networks))))

(def solve-1 (comp count solve-t parse))

(defn reduce-networks [networks tuples]
  (into #{} (mapcat (fn [network] (map #(conj network %) (apply set/intersection (map networks network)))) tuples)))

(defn network-seq [{:keys [pairs networks]}]
  (iterate (partial reduce-networks networks) pairs))

(defn find-biggest [network]
  (first (last (take-while seq (network-seq network)))))

(defn solve-2 [input]
  (->> input parse find-biggest sort (s/join #",")))
