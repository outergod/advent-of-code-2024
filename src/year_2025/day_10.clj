(ns year-2025.day-10
  (:require aoc
            [clojure.walk :refer [postwalk]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.java.process :as process]))

(def example (slurp (io/resource "2025/day-10-example")))
(def input (slurp (io/resource "2025/day-10")))

(def lights-re #"\[([\.#]+)\]")
(def button-re #"\(((?:\d+,?)+)\)")
(def joltage-re #"\{((?:\d+,?)+)\}")

(defn parser [re] (fn [s] (second (re-find re s))))
(def parse-lights (parser lights-re))
(def parse-button (parser button-re))
(def parse-joltage (parser joltage-re))

(defn lights-int [s]
  (reduce
   (fn [acc [i c]] (case c \. acc \# (+ acc (int (Math/pow 2 i)))))
   0 (map-indexed vector (parse-lights s))))

(defn button-int [s]
  (reduce
   (fn [acc i] (+ acc (int (Math/pow 2 (Integer/parseInt (str i))))))
   0 (s/split (parse-button s) #",")))

(defn button-joltage [n s]
  (reduce
   (fn [acc i] (assoc acc i 1))
   (vec (repeat n 0)) (map #(Integer/parseInt %) (s/split (parse-button s) #","))))

(defn joltages [s]
  (map #(Integer/parseInt %) (s/split (parse-joltage s) #",")))

(defn parse-machine [s]
  (let [[lights & buttons] (s/split s #" ")
        [joltage & buttons] (reverse buttons)
        joltages (joltages joltage)]
    {:lights (lights-int lights)
     :buttons (map button-int (reverse buttons))
     :button-joltages (map (partial button-joltage (count joltages)) (reverse buttons))
     :joltages joltages}))

(defn parse [input]
  (map parse-machine (s/split-lines input)))

(defn probe-all [lights buttons]
  (filter #(= lights (reduce bit-xor 0 %)) (combo/subsets buttons)))

(defn find-best [lights buttons]
  (count (first (sort-by count (probe-all lights buttons)))))

(defn solve-1 [input]
  (reduce + (map (fn [{:keys [lights buttons]}] (find-best lights buttons)) (parse input))))

(defn smt [buttons joltage]
  (let [seqs (repeatedly (count buttons) #(gensym "x"))
        factors (repeatedly (count buttons) #(gensym "m"))]
    `((set-logic ALL)
      (set-option :produce-models true)
      ~@(for [x seqs] `(declare-const ~x (Seq Int)))
      (declare-const joltage (Seq Int))
      ~@(for [x factors] `(declare-const ~x Int))
      (declare-const sum Int)
      ~@(map (fn [button seq]
               `(assert (= ~seq (seq.++ ~@(map (fn [n] `(seq.unit ~n)) button)))))
             buttons seqs)
      (assert (= joltage (seq.++ ~@(map (fn [n] `(seq.unit ~n)) joltage))))
      ~@(for [x (range 0 (count joltage))]
          `(assert (= (seq.nth joltage ~x)
                      (+ ~@(map (fn [seq factor]
                                  `(* ~factor (seq.nth ~seq ~x)))
                                seqs factors)))))
      ~@(for [x factors] `(assert (>= ~x 0)))
      (assert (= sum (+ ~@factors)))
      (minimize sum)
      (check-sat)
      (get-value (sum)))))

(defn strip-ns [form]
  (postwalk #(if (symbol? %) (symbol (name %)) %) form))

(defn solve-z3 [machine]
  (let [{:keys [button-joltages joltages]} machine
        buttons button-joltages
        smt (strip-ns (smt buttons joltages))
        z3 (process/start "z3" "-in")
        stdin (process/stdin z3)
        stdout (process/stdout z3)]
    (doseq [form smt] (.write stdin (.getBytes (println-str form))))
    (.close stdin)
    (let [output (slurp stdout)]
      (.waitFor z3)
      (Integer/parseInt (->> output s/split-lines second (re-find #"\(\(sum (\d+)\)\)") second)))))

(defn solve-2 [input]
  (reduce + (map solve-z3 (parse input))))

(comment
  "too slow"
  (defn overjoltage? [target source]
    (some (fn [[target source]] (> source target)) (map vector target source)))

  (defn find-shortest [buttons joltages]
    (letfn [(search [acc path shortest]
              (cond
                (and shortest (>= (inc (count path)) (count shortest))) shortest
                (= acc joltages) path
                (overjoltage? joltages acc) shortest
                :else (reduce (fn [shortest button]
                                (search (map + acc button)
                                        (conj path button)
                                        shortest))
                              shortest buttons)))]
      (search (repeat (count joltages) 0) [] nil))))
