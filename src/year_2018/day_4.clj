(ns year-2018.day-4
  (:require aoc
            [clojure.java.io :as io]
            [clojure.string :as s]
            [java-time.api :as jt]))

(def example (slurp (io/resource "2018/day-4-example")))
(def input (slurp (io/resource "2018/day-4")))

(def record-re #"\[([^]]+)] (.+)")
(def shift-re #"Guard #(\d+) begins shift")

(defn parse-event [s]
  (case s
    "falls asleep" {:event :sleep}
    "wakes up" {:event :wake}
    {:event :shift :id (Integer/parseInt (second (re-find shift-re s)))}))

(defn parse-record [s]
  (let [[_ date event] (re-find record-re s)]
    [(jt/local-date-time "yyyy-MM-dd HH:mm" date) (parse-event event)]))

(defn parse [input]
  (sort-by first (map parse-record (s/split-lines input))))

(defn analyze [records]
  (:guards
   (reduce
    (fn [state [time {:keys [event id]}]]
      (case event
        :shift (assoc state :active id)
        :sleep (assoc state :sleep (.getMinute time))
        :wake (-> state
                  (update-in [:guards (state :active) :sleep] #(+ (or % 0) (- (.getMinute time) (state :sleep))))
                  (update-in [:guards (state :active) :minutes]
                             #(reduce (fn [minutes minute] (update minutes minute inc))
                                      (or % (vec (repeat 60 0)))
                                      (range (state :sleep) (.getMinute time)))))))
    {} records)))

(defn solve-1 [input]
  (let [[id {:keys [minutes]}] (->> input parse analyze (sort-by (comp :sleep second) >) first)
        minute (->> minutes (map-indexed vector) (sort-by second >) first first)]
    (* id minute)))

(defn max-minutes [analysis]
  (update-vals analysis
               (fn [{:keys [minutes] :as data}]
                 (assoc data :max (->> minutes (map-indexed vector) (sort-by second >) first)))))

(defn solve-2 [input]
  (let [analysis (->> input parse analyze max-minutes (sort-by (comp second :max second) >))
        [id {[minute _] :max}] (first analysis)]
    (* id minute)))
