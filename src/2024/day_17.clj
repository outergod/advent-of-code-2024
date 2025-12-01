(ns day-17
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.math :as math]))

(def example (s/trim-newline (slurp (io/resource "day-17-example"))))
(def input (s/trim-newline (slurp (io/resource "day-17"))))

(def register-re #"Register ([A-C]): (\d+)")
(def program-re #"Program: ((?:\d+,?)*)")

(defn parse [input]
  (reduce
   (fn [acc line]
     (let [[_ register n] (re-matches register-re line)
           [_ program] (re-matches program-re line)]
       (cond
         register (assoc-in acc [:registers register] (biginteger n))
         program (assoc acc :program (mapv biginteger (s/split program #",")))
         :else acc)))
   {:registers {} :ip 0 :output []}
   (s/split-lines input)))

(defn combo [operand registers]
  (case operand
    (0 1 2 3) operand
    4 (registers "A")
    5 (registers "B")
    6 (registers "C")))

(defn xdv [id]
  (fn [operand {:keys [registers] :as state}]
    (-> state
        (assoc-in [:registers id]
                  (biginteger (/ (registers "A")
                                 (math/pow 2 (combo operand registers)))))
        (update :ip (partial + 2)))))

(def adv (xdv "A"))
(def bdv (xdv "B"))
(def cdv (xdv "C"))

(defn bxl [operand {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers "B"]
                (.xor (registers "B") (biginteger operand)))
      (update :ip (partial + 2))))

(defn bst [operand {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers "B"]
                (.and (.mod (combo operand registers) (biginteger 8)) (biginteger 2r111)))
      (update :ip (partial + 2))))

(defn jnc [operand {:keys [registers] :as state}]
  (if (zero? (registers "A"))
    (update state :ip (partial + 2))
    (assoc state :ip operand)))

(defn bxc [_ {:keys [registers] :as state}]
  (-> state
      (update-in [:registers "B"] #(.xor %1 %2) (registers "C"))
      (update :ip (partial + 2))))

(defn out [operand {:keys [registers] :as state}]
  (-> state
      (update :output conj (int (mod (combo operand registers) 8)))
      (update :ip (partial + 2))))

(def opcode-instruction [adv bxl bst jnc bxc out bdv cdv])

(defn run [state]
  (loop [{:keys [ip program output] :as state} state]
    (if (>= ip (count program))
      output
      (let [instruction (opcode-instruction (program ip))
            operand (program (inc ip))]
        (recur (instruction operand state))))))

(defn solve-1 [input]
  (s/join "," (run (parse input))))

(defn shift [coll]
  (reduce (fn [acc [i n]]
            (.add (.shiftLeft (biginteger n) (* 3 i)) acc))
          (biginteger 0)
          (map-indexed vector coll)))

(defn find-program [state]
  (let [{:keys [program]} state
        program (reverse program)]
    (loop [total 0 acc nil output []]
      (cond
        (= output program) total

        :else
        (if-let [[total acc output]
                 (first
                  (filter (fn [[_ _ output]] (every? identity (map = program output)))
                          (map (fn [n]
                                 (let [acc (cons n acc)
                                       candidate (shift acc)]
                                   [candidate acc (reverse (run (assoc-in state [:registers "A"] candidate)))]))
                               (range 1 8))))]
          (recur total acc output)
          (some (fn [[candidate output]] (when (= program output) candidate))
                (for [x (range 0 8) y (range 0 8) z (range 0 8)]
                  (let [acc (concat (list x y z) (rest acc))
                        candidate (shift acc)
                        output (reverse (run (assoc-in state [:registers "A"] candidate)))]
                    [candidate output]))))))))

(defn solve-2 [input]
  (find-program (parse input)))
