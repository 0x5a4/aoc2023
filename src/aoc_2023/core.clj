(ns aoc-2023.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (seq args)
    (load (str "aoc_2023/day" (first args)))
    (throw (Exception. "specify a day index to execute"))))
