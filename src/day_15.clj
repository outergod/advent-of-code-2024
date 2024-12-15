(ns day-15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-15-example"))))
(def example-mini (s/trim-newline (slurp (io/resource "day-15-example-mini"))))
(def input (s/trim-newline (slurp (io/resource "day-15"))))

(defn parse-layout-1 [layout]
  (let [extent-y (count layout)
        extent-x (count (first layout))]
    (reduce (fn [acc [x y]]
             (case (get-in layout [y x])
               \# (assoc-in acc [:entities [x y]] :wall)
               \O (assoc-in acc [:entities [x y]] :box)
               \@ (-> acc
                      (assoc :robot [x y])
                      (assoc-in [:entities [x y]] :robot))
               \. acc))
           {:entities {} :extents [extent-x extent-y]}
           (for [y (range 0 extent-y) x (range 0 extent-x)] [x y]))))

(defn parse-movements [movements]
  (map #(case % \^ [0 -1] \v [0 1] \< [-1 0] \> [1 0]) movements))

(defn parse-1 [input]
  (let [[layout _ movements] (partition-by s/blank? (s/split-lines input))
        layout (vec layout)]
    (assoc (parse-layout-1 (vec layout)) :movements (parse-movements (s/join "" movements)))))

(defn visualize-1 [state]
  (let [{:keys [extents robot entities]} state
        [extent-x extent-y] extents]
    (map (fn [y]
           (apply str (map (fn [x] (if (= robot [x y]) \@
                                       (case (entities [x y])
                                         :wall \#
                                         :box \O
                                         \.)))
                           (range extent-x))))
         (range extent-y))))

(defn move-1 [pos movement state]
  (let [next-pos (mapv + pos movement)
        entity (get-in state [:entities pos])]
    (if (#{:box :robot} entity)
      (let [state (move-1 next-pos movement state)]
        (if (get-in state [:entities next-pos])
          state
          (-> state
              (assoc-in [:entities pos] nil)
              (assoc-in [:entities next-pos] entity)
              (update :robot (fn [old] (if (= entity :robot) next-pos old))))))
      state)))

(defn move-all-1 [state]
  (reduce (fn [{:keys [robot] :as state} movement]
            (let [state (move-1 robot movement state)]
              (update state :visuals conj (visualize-1 state))))
          (assoc state :visuals [])
          (state :movements)))

(defn solve-1 [input]
  (reduce (fn [acc [[x y] entity]]
            (if (= :box entity)
              (+ acc x (* y 100))
              acc))
          0
          (:entities (move-all-1 (parse-1 input)))))

(defn parse-layout-2 [layout]
  (let [extent-y (count layout)
        extent-x (count (first layout))]
    (reduce (fn [acc [x y]]
              (case (get-in layout [y x])
                \# (-> acc
                       (assoc-in [:entities (acc :id)] :wall)
                       (assoc-in [:positions [(* x 2) y]] (acc :id))
                       (assoc-in [:positions [(inc (* x 2)) y]] (acc :id))
                       (update :id inc))
                \O (let [id-parent (acc :id) id-left (+ 1 (acc :id)) id-right (+ 2 (acc :id))]
                     (-> acc
                        (assoc-in [:entities id-parent] :box)
                        (assoc-in [:entities id-left] :box-left)
                        (assoc-in [:entities id-right] :box-right)
                        (assoc-in [:positions [(* x 2) y]] id-left)
                        (assoc-in [:positions [(inc (* x 2)) y]] id-right)
                        (update :id (partial + 3))))
                \@ (-> acc
                       (assoc-in [:entities (acc :id)] :robot)
                       (assoc-in [:positions [(* x 2) y]] (acc :id))
                       (assoc :robot [(* x 2) y])
                       (update :id inc))
                \. acc))
            {:entities {} :extents [(* extent-x 2) extent-y] :children {} :parents {} :id 0}
            (for [y (range 0 extent-y) x (range 0 extent-x)] [x y]))))

(defn parse-2 [input]
  (let [[layout _ movements] (partition-by s/blank? (s/split-lines input))]
    (assoc (parse-layout-2 (vec layout)) :movements (parse-movements (s/join "" movements)))))

(parse-2 example-mini)

(defn visualize-2 [state]
  (let [{:keys [extents entities positions]} state
        [extent-x extent-y] extents]
    (map (fn [y]
           (apply str (map (fn [x]
                             (case (entities (positions [x y]))
                               :robot \@
                               :wall \#
                               :box-left \[
                               :box-right \]
                               \.))
                           (range extent-x))))
         (range extent-y))))

(visualize-2 (parse-2 example))

(parse-2 example)
