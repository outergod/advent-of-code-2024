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
         register (assoc-in acc [:registers register] (parse-long n))
         program (assoc acc :program (mapv parse-long (s/split program #",")))
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
                  (int (/ (registers "A")
                          (math/pow 2 (combo operand registers)))))
        (update :ip (partial + 2)))))

(def adv (xdv "A"))
(def bdv (xdv "B"))
(def cdv (xdv "C"))

(defn bxl [operand {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers "B"]
                (bit-xor (registers "B") operand))
      (update :ip (partial + 2))))

(defn bst [operand {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers "B"]
                (bit-and (mod (combo operand registers) 8) 2r111))
      (update :ip (partial + 2))))

(defn jnc [operand {:keys [registers] :as state}]
  (if (zero? (registers "A"))
    (update state :ip (partial + 2))
    (assoc state :ip operand)))

(defn bxc [_ {:keys [registers] :as state}]
  (-> state
      (update-in [:registers "B"] bit-xor (registers "C"))
      (update :ip (partial + 2))))

(defn out [operand {:keys [registers] :as state}]
  (-> state
      (update :output conj (mod (combo operand registers) 8))
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
            (+ (bit-shift-left n (* 3 i)) acc))
          0
          (map-indexed vector coll)))

(defn find-program [state]
  (let [{:keys [program]} state
        program (reverse program)]
    (loop [total 0 acc nil limit 100]
      (println "current" total acc)
      (cond
        (= acc program) total
        (= (count acc) (count program)) nil
        (zero? limit) :abort

        :else
        (if-let [[total acc]
                 (first
                  (filter (fn [[_ _ output]] (every? identity (map = program output)))
                          (map (fn [n]
                                 (let [acc (cons n acc)
                                       candidate (shift acc)]
                                   (println acc candidate (reverse (run (assoc-in state [:registers "A"] candidate))) "vs" program)
                                   [candidate acc (reverse (run (assoc-in state [:registers "A"] candidate)))]))
                               (range 1 8))))]
          (recur total acc (dec limit))
          :no)))))

(find-program (parse input))

(every? identity (map = [0] '(0)))
