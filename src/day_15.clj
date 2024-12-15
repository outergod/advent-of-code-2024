(ns day-15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.set :as set]))

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

(defn move-all [state movefn]
  (reduce (fn [{:keys [robot] :as state} movement]
            (let [state (movefn robot movement state)]
              (update state :visuals conj (visualize-1 state))))
          (assoc state :visuals [])
          (state :movements)))

(defn solve-1 [input]
  (reduce (fn [acc [[x y] entity]]
            (if (= :box entity)
              (+ acc x (* y 100))
              acc))
          0
          (:entities (move-all (parse-1 input) move-1))))

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
                        (assoc-in [:parents id-parent] #{id-left id-right})
                        (assoc-in [:children id-left] id-parent)
                        (assoc-in [:children id-right] id-parent)
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

(defn move-collect [pos movement state]
  (let [{:keys [positions entities children parents]} state
        rev-positions (set/map-invert positions)]
    (loop [open #{pos} closed #{} acc #{}]
      (let [pos (first open)
            open (disj open pos)
            entity (positions pos)]
        (if pos
          (case (entities entity)
            :robot (recur (conj open (mapv + pos movement)) (conj closed pos) (conj acc entity))

            (:box-left :box-right)
            (let [walls (parents (children entity))
                  positions (set (map #(mapv + movement (rev-positions %)) walls))]
              (recur (into open (set/difference positions closed))
                     (set/union closed positions)
                     (into acc walls)))

            :wall #{}

            (recur open (conj closed pos) acc))
          acc)))))

(defn move-entities [entities movement state]
  (update state :positions #(into {} (map (fn [[k v]] [(if (entities v) (mapv + k movement) k) v]) %))))

(defn move [pos movement state]
  (let [state (move-entities (move-collect pos movement state) movement state)
        robot-id (some (fn [[k v]] (when (= v :robot) k)) (state :entities))
        rev-positions (set/map-invert (state :positions))]
    (assoc state :robot (rev-positions robot-id))))

(defn solve-2 [input]
  (let [state (move-all (parse-2 input) move)
        rev-positions (set/map-invert (state :positions))]
    (reduce (fn [acc [id type]]
              (if (= :box-left type)
                (let [[x y] (rev-positions id)]
                  (+ acc x (* y 100)))
                acc))
            0
            (:entities state))))
