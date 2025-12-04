(ns aoc
  (:require
   [clojure.string :as s]
   [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]
   [clojure.math :as math]))

(defmacro traced-map [s f & colls]
  `(let [n# (dec (apply max (map count (list ~@colls))))]
     (map (fn [i# & xs#]
            (println "trace" ~s i# "/" n#)
            (apply ~f xs#))
          (range) ~@colls)))

(defmacro traced-mapcat [s f & colls]
  `(let [n# (dec (apply max (map count (list ~@colls))))]
     (mapcat (fn [i# & xs#]
               (println "trace" ~s i# "/" n#)
               (apply ~f xs#))
             (range) ~@colls)))

(defn fix [f] (fn g [& args] (apply f g args)))

(defn parse-layout [input]
  (let [layout (s/split-lines (s/trim-newline input))
        extent-y (count layout)
        extent-x (count (first layout))]
    (reduce (fn [acc [x y]]
              (case (get-in layout [y x])
                \# (update acc :walls conj [x y])
                \S (assoc acc :start [x y])
                \E (assoc acc :end [x y])
                \. (update acc :floor conj [x y])))
            {:walls #{} :floor #{} :extents [extent-y extent-y]}
            (for [y (range 0 extent-y) x (range 0 extent-x)] [x y]))))

(defn euclidian-distance
  ([[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
  ([[x0 y0] [x1 y1]] (euclidian-distance [(- x1 x0) (- y1 y0)])))

(defn in-limits? [[x y] [w h]] (and (>= x 0) (>= y 0) (< x w) (< y h)))

(def neighbors-4 [[0 1] [1 0] [0 -1] [-1 0]])
(def neighbors-8 [[0 1] [1 0] [0 -1] [-1 0] [1 -1] [-1 1] [1 1] [-1 -1]])

(defn neighbors [[x y]]
  (map (fn [[a b]] [(+ x a) (+ y b)]) neighbors-4))

(defn a*-seq [[x0 y0] [x1 y1] [w h] obstacles]
  (let [in-limits? #(in-limits? % [w h])]
    (letfn [(h [[x y]] (euclidian-distance [x y] [x1 y1]))
            (f [[x y] g] (+ g (h [x y])))
            (n [[x y] g parents fringe]
              (let [parents (conj parents [x y])]
                (keep (fn [[x y]]
                        (let [g (inc g)
                              cost (f [x y] g)
                              [prev-cost] (fringe [x y])]
                          (when-not (and prev-cost (<= prev-cost cost))
                            [[x y] [cost g parents]])))
                      (filter in-limits? (neighbors [x y])))))]
      (iterate
       (fn [{:keys [mode fringe closed goal] :as state}]
         (if (#{:finish :end} mode)
           (assoc state :mode :end)
           (let [[[x y] [cost distance parents] :as node] (peek fringe)
                 [[x-goal y-goal] [cost-goal _ parents-goal] :as node-goal] (peek goal)]
             (cond (= [x y] [x1 y1])
                   (-> state (update :fringe pop) (update :goal conj node))

                   (and (not-empty node-goal)
                        (or (empty? node)
                            (< cost-goal cost)))
                   (-> state
                       (assoc :mode :finish)
                       (assoc :goal (conj parents-goal [x-goal y-goal])))

                   node
                   (let [candidates (n [x y] distance parents fringe)]
                     (-> state
                         (assoc :fringe (into (pop fringe) (remove (comp closed first) candidates)))
                         (update :closed conj [x y])
                         (update :touched into (filter obstacles (map first candidates)))))))))
       {:mode :continue
        :fringe (priority-map-keyfn first [x0 y0] [(h [x0 y0]) 0 []])
        :closed obstacles
        :goal (priority-map-keyfn first)
        :touched #{}}))))

(defn a* [start goal obstacles extents limit]
  (last (take limit (take-while (comp #{:continue :finish} :mode)
                                (a*-seq start goal extents obstacles)))))

(defn turn [[x y]] [[(* -1 y) x] [y (* -1 x)]])

(defn path [start end extents obstacles]
  (loop [open (priority-map [(list start) [1 0]] 0) closed #{} best nil paths []]
    (if-let [[[path direction] score] (first open)]
      (let [open (dissoc open [path direction])
            pos (first path)
            closed (conj closed [pos direction])]
        (cond
          (or (not (in-limits? pos extents)) (obstacles pos) (and best (> score best)))
          (recur open closed best paths)

          (= end pos)
          (if (or (nil? best) (< score best))
            (recur open closed score [path])
            (recur open closed score (conj paths path)))

          :else
          (let [forward [[(conj path (mapv + pos direction)) direction] (inc score)]
                turns (map #(vector [path %1] %2) (turn direction) (repeat (+ score 1000)))
                next (filter (fn [[[path direction] _]] (not (closed [(first path) direction]))) (conj turns forward))]
            (recur (into open next) closed best paths))))
      [best paths])))

(defn tuples [n coll]
  (take-while #(= n (count %))
              (map (partial take n)
                   (iterate next coll))))

(def pairs (partial tuples 2))

(def factors
  (memoize
   (fn [n]
     (into (sorted-set)
           (mapcat (fn [x] [x (/ n x)])
                   (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))))))))

(defn digits [n]
  (int (inc (math/log10 n))))
