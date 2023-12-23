(ns aoc-2023.day2)
(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])
(require '[clojure.string :as string])

(def input (slurp (io/resource "day2")))

(defn extractGameIndex [line]
  (edn/read-string (subs line 4 (.indexOf line ":"))))

(defn extractGameInfos [line]
  (string/split (subs line (inc (.indexOf line ":"))) #";"))

(defn extractThrow [s]
  (let [w (.indexOf s " ")]
    {(subs s (inc w)) (edn/read-string (subs s 0 w))}))

; extract a vector filled with maps in the form [{"color" amount "othercolor" amount}]
(defn extractGames [s]
  (reduce merge (map
                 #(extractThrow (string/trim %))
                 (string/split s #","))))

; PART ONE
(defn checkThrow [throws]
  (let [maxThrows {"red" 12 "green" 13 "blue" 14}]
    (every?
     (partial = true)
     (map
      #(<= (last %) (get maxThrows (first %)))
      (seq throws)))))

(defn checkGame [line]
  (let [index (extractGameIndex line)
        games (map extractGames (extractGameInfos line))]
    (if (every? (partial = true) (map checkThrow games))
      index
      0)))

(print "part one:")
(println (reduce + (map checkGame (string/split-lines input))))

(defn calcMinCubes [line]
  (let [games (map extractGames (extractGameInfos line))]
    (reduce
     (fn [cubeMap throws]
       (loop [coll (seq throws) m cubeMap]
         (if (empty? coll)
           m
           (let [entry (first coll)
                 key (first entry)
                 value (last entry)]
             (if (> value (get m key java.lang.Integer/MIN_VALUE))
               (recur (rest coll) (assoc m key value))
               (recur (rest coll) m))))))

     (clojure.lang.PersistentHashMap/EMPTY)
     games)))

(calcMinCubes "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")

(defn getThrowPower [thro]
  (reduce #(* %1 (last %2)) 1 (seq thro)))

(print "part one:")
(println (reduce + (map #(getThrowPower (calcMinCubes %)) (string/split-lines input))))
