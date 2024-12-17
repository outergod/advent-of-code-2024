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
                  (int (/ (registers"A")
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

(let [{:keys [ip program] :as state} {:ip 0 :registers {"A" 10 "B" 0 "C" 9} :program [5 0 5 1 5 4] :output []}
      instruction (opcode-instruction (program ip))
      operand (program (inc ip))]
  [(program ip) operand (instruction operand state)])

(let [state (parse example)]
  (loop [{:keys [ip program output] :as state} state]
    (if (>= ip (count program))
      output
      (let [instruction (opcode-instruction (program ip))
            operand (program (inc ip))]
        (println state)
        (recur (instruction operand state))))))

(defn run [state]
  (loop [{:keys [ip program output] :as state} state]
    (if (>= ip (count program))
      output
      (let [instruction (opcode-instruction (program ip))
            operand (program (inc ip))]
        (recur (instruction operand state))))))

(defn solve-1 [input]
  (s/join "," (run (parse input))))

(solve-1 input)
