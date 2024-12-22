(ns day-21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [aoc :refer [fix pairs]]))

(def example (s/trim-newline (slurp (io/resource "day-21-example"))))
(def input (s/trim-newline (slurp (io/resource "day-21"))))

(def numeric
  {:obstacles #{[0 3]} :buttons {\7 [0 0] \8 [1 0] \9 [2 0] \4 [0 1] \5 [1 1] \6 [2 1] \1 [0 2] \2 [1 2] \3 [2 2] \0 [1 3] \A [2 3]}})

(def directional
  {:obstacles #{[0 0]} :buttons {\^ [1 0] \A [2 0] \< [0 1] \v [1 1] \> [2 1]}})

(def translate {[0 1] \v [0 -1] \^ [1 0] \> [-1 0] \<})

(def path
  (memoize
   (fn [from to {:keys [buttons obstacles]}]
     (let [[x0 y0] (buttons from)
           [x1 y1] (buttons to)
           [x y] (map #(if (= %1 %2) 0 (/ (- %2 %1) (abs (- %2 %1)))) [x0 y0] [x1 y1])
           xs (range x0 x1 x)
           ys (range y0 y1 y)
           xy (concat (map vector xs (repeat y0)) (map vector (repeat x1) ys) [[x1 y1]])
           yx (concat (map vector (repeat x0) ys) (map vector xs (repeat y1)) [[x1 y1]])]
       (conj
        (mapv (comp translate (fn [[x y]] (mapv - y x)))
              (pairs
               (first (filter #(not (some obstacles %)) (if (pos? x) [yx xy] [xy yx])))))
        \A)))))

(defn solve [code n]
  (let [solve (fn [solve code n acc]
                (if (zero? n)
                  (count code)
                  (reduce (fn [acc [from to]]
                            (+ acc (solve (path from to directional) (dec n) 0)))
                          acc (pairs (cons \A code)))))
        solve' (fix (memoize solve))]
    (solve' (mapcat (fn [[from to]] (path from to numeric)) (pairs (str "A" code))) n 0)))


(defn solve-1 [input]
  (reduce + (map (fn [code]
                   (* (solve code 2)
                      (parse-long (apply str (butlast code)))))
                 (s/split-lines input))))

(defn solve-2 [input]
  (reduce + (map (fn [code]
                   (* (solve code 25)
                      (parse-long (apply str (butlast code)))))
                 (s/split-lines input))))
