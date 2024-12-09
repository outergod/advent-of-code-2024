(ns day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def example (s/trim-newline (slurp (io/resource "day-9-example"))))
(def input (s/trim-newline (slurp (io/resource "day-9"))))

(defn make-disk-1 [input]
  (let [parsed (map #(-> % str parse-long) input)
        disk (int-array (reduce + parsed) -1)]
    (loop [i 0 n 0 flip? false [cur & parsed] parsed]
      (if cur
        (if flip?
          (recur (+ i cur) n false parsed)
          (do
            (doseq [x (range i (+ i cur))]
              (aset-int disk x n))
            (recur (+ i cur) (inc n) true parsed)))
        disk))))

(defn defrag-1 [disk]
  (loop [i 0 j (dec (count disk))]
    (if (< i j)
      (if (= -1 (aget disk i))
        (do
          (aset-int disk i (aget disk j))
          (aset-int disk j -1)
          (recur (inc i) (some #(and (not= -1 (aget disk %)) %)
                               (reverse (range j)))))
        (recur (inc i) j))
      disk)))

(defn solve-1 [input]
  (let [disk (defrag-1 (make-disk-1 input))]
    (reduce + (map * (range) (take-while #(not= % -1) disk)))))

(defn make-disk-2 [input]
  (let [parsed (map #(-> % str parse-long) input)]
    (dissoc
     (reduce (fn [{:keys [position] :as disk} [i n]]
               (assoc
                (if (even? i)
                  (update disk :files conj {:size n :position position})
                  (update disk :space conj {:size n :position position}))
                :position (+ position n)))
             {:files [] :space [] :position 0} (map vector (range) parsed))
     :position)))

(defn defrag-2 [disk]
  (reduce (fn [{:keys [files space] :as disk} i]
            (let [{file-size :size file-position :position} (get files i)]
              (if-let [x (some (fn [[x {:keys [size]}]] (when (>= size file-size) x))
                               (map vector (range) (take-while (fn [{:keys [position]}] (< position file-position)) space)))]
                (let [{:keys [position size]} (get space x)]
                  {:files (assoc-in files [i :position] position)
                   :space (if (= file-size size)
                            (vec (concat (subvec space 0 x) (subvec space (inc x))))
                            (-> space (update-in [x :size] - file-size) (update-in [x :position] + file-size)))})
                disk)))
          disk (reverse (range (count (disk :files))))))

(defn solve-2 [input]
  (let [disk (defrag-2 (make-disk-2 input))]
   (reduce + (map (fn [i {:keys [position size]}]
                    (reduce + (map (partial * i) (range position (+ position size)))))
                  (range) (disk :files)))))
