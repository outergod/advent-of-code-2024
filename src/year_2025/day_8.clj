(ns year-2025.day-8
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def example (slurp (io/resource "2025/day-8-example")))
(def input (slurp (io/resource "2025/day-8")))

(defn parse [input]
  (map (fn [line] (mapv #(Integer/parseInt %) (s/split line #",")))
       (s/split-lines input)))

(defn distance [p0 p1]
  (let [[x0 y0 z0] p0 [x1 y1 z1] p1]
    (Math/sqrt (+ (Math/pow (- x1 x0) 2)
                  (Math/pow (- y1 y0) 2)
                  (Math/pow (- z1 z0) 2)))))

(defn pairs [boxes]
  (let [box-set (set boxes)]
    (mapcat (fn [p0] (map (fn [p1] [#{p0 p1} (distance p0 p1)]) (disj box-set p0)))
            boxes)))

(defn closest [n boxes]
  (->> boxes (sort-by second) dedupe (take n) (map first)))

(defn connect [boxes]
  (reduce (fn [boxes pair] (let [[p0 p1] (seq pair)]
                             (-> boxes (update p0 #(conj (or %1 #{}) %2) p1) (update p1 #(conj (or %1 #{}) %2) p0))))
          {} boxes))

(defn explore [boxes]
  (loop [[p0 & ps] (keys boxes) circuits []]
    (if (nil? p0)
      circuits
      (let [circuit (loop [[p1 & open] (boxes p0) closed #{}]
                      (if (nil? p1)
                        closed
                        (let [next (remove (apply conj closed open) (boxes p1))]
                          (recur (apply conj open next) (conj closed p1)))))]
        (recur (remove circuit ps) (conj circuits circuit))))))

(defn solve-1 [input n]
  (->> input parse pairs (closest n) connect explore (sort-by count >) (take 3) (map count) (reduce *)))

(defn closest-all [boxes]
  (->> boxes (sort-by second) dedupe (map (comp seq first))))

(defn complete [boxes]
  (let [[[start start'] & pairs] (-> boxes pairs closest-all)]
    (loop [[[p0 p1] & pairs] pairs circuits {start #{start start'}} rev {start start start' start} last [start start']]
      (cond (= (count boxes) (count (circuits (rev start)))) last
            (nil? p0) :failed

            (and (rev p0) (rev p1))
            (if (= (rev p0) (rev p1))
              (recur pairs circuits rev [p0 p1])
              (recur pairs
                     (-> circuits (update (rev p0) set/union (circuits (rev p1))) (dissoc (rev p1)))
                     (into rev (map (fn [c] [c (rev p0)]) (circuits (rev p1))))
                     [p0 p1]))

            (rev p0)
            (recur pairs
                   (update circuits (rev p0) conj p1)
                   (assoc rev p1 (rev p0))
                   [p0 p1])

            (rev p1)
            (recur pairs
                   (update circuits (rev p1) conj p0)
                   (assoc rev p0 (rev p1))
                   [p0 p1])

            :else
            (recur pairs
                   (assoc circuits p0 #{p0 p1})
                   (assoc rev p0 p0 p1 p0)
                   [p0 p1])))))

(defn solve-2 [input]
  (->> input parse complete (map first) (reduce *)))
