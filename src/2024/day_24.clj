(ns day-24
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as set]
   [aoc :refer [fix]]
   [clojure.math.combinatorics :refer [combinations]]))

(def example (s/trim-newline (slurp (io/resource "day-24-example"))))
(def example-large (s/trim-newline (slurp (io/resource "day-24-example-large"))))
(def input (s/trim-newline (slurp (io/resource "day-24"))))

(defn parse [input]
  (let [[init _ gates] (partition-by #(= "" %) (s/split-lines input))
        init (into {} (map #(let [[wire state] (s/split % #": ")] [wire (= state "1")]) init))
        gates (into {} (map #(let [[in out] (s/split % #" -> ")
                                   [left op right] (s/split in #" ")]
                               [out [left op right]])
                            gates))]
    {:init init :gates gates}))

(defn z-gates [gates] (filter #(s/starts-with? % "z") (keys gates)))

(defn out-state [{:keys [init gates]}]
  (fn [wire]
    (let [f (fn [f wire]
              (if (contains? init wire)
                (init wire)
                (let [[left op right] (gates wire)
                      [left right] (map f [left right])]
                  (case op
                    "AND" (and left right)
                    "OR" (or left right)
                    "XOR" (not= left right)))))
          f' (fix (memoize f))]
      (f' wire))))

(defn solve [{:keys [gates] :as state}]
  (let [outfn (out-state state)]
    (reduce
     (fn [n [i state]] (if state (bit-set n i) (bit-clear n i)))
     0
     (map vector (range) (map outfn (sort (z-gates gates)))))))

(defn solve-1 [input] (solve (parse input)))

(defn find-wire [gates left right op]
  (some (fn [[out [left' op' right']]] (when (and (= op op') (= #{left right} #{left' right'})) out)) gates))

(defn check-half-adder [gates i]
  (let [x-in (format "x%02d" i)
        y-in (format "y%02d" i)
        z-out (find-wire gates x-in y-in "XOR")
        c-out (find-wire gates x-in y-in "AND")]
    {:z-out z-out :c-out c-out}))

(defn check-full-adder [gates i c-in]
  (let [x-in (format "x%02d" i)
        y-in (format "y%02d" i)
        x-xor-y (find-wire gates x-in y-in "XOR")
        c-out-1 (find-wire gates x-in y-in "AND")
        z-out (find-wire gates x-xor-y c-in "XOR")
        z-out-debug (gates (format "z%02d" i))
        c-out-2 (find-wire gates x-xor-y c-in "AND")
        c-out (find-wire gates c-out-1 c-out-2 "OR")]
    {:z-out z-out :c-out c-out :x-xor-y x-xor-y :c-out-1 c-out-1 :c-out-2 c-out-2 :z-out-debug z-out-debug}))

(defn swap-wires [gates left right] (-> gates (assoc left (gates right)) (assoc right (gates left))))

(defn patch-wires [gates]
  (-> gates
      (swap-wires "z06" "vwr")
      (swap-wires "z11" "tqm")
      (swap-wires "z16" "kfs")
      (swap-wires "gfv" "hcm")))

(defn check-wires [gates]
  (let [gates (patch-wires gates)]
   (reduce (fn [acc i]
             (if (zero? i)
               (conj acc (check-half-adder gates i))
               (let [{:keys [c-out]} (last acc)]
                 (conj acc (check-full-adder gates i c-out)))))
           []
           (range (dec (count (z-gates gates)))))))

(def solve-2 (s/join "," (sort ["z06" "vwr" "z11" "tqm" "z16" "kfs" "gfv" "hcm"])))
