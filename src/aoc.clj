(ns aoc)

(defmacro traced-map [s f & colls]
  `(let [n# (dec (apply max (map count (list ~@colls))))]
     (map (fn [i# & xs#]
            (println "trace" ~s i# "/" n#)
            (apply ~f xs#))
          (range) ~@colls)))

(defn fix [f] (fn g [& args] (apply f g args)))
