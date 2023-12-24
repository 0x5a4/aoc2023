(ns aoc-2023.day4)
(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])
(require '[clojure.string :as string])

(def input (slurp (io/resource "day4")))

(defn buildNumberTable [s]
  (set
   (filter
    (partial not= nil)
    (map
     edn/read-string
     (string/split s #" ")))))

(defn findMatches [winners numbers]
  (filter (partial contains? winners) numbers))

; PART ONE
(defn calculatePoints [matches]
  (if (empty? matches)
    0
    (int (Math/pow 2 (dec (count matches))))))

(defn evaluateLine [line]
  (let [parts (string/split (subs line (inc (.indexOf line ":"))) #"\|")
        winnerString (first parts)
        numberString (last parts)]
    (calculatePoints
     (findMatches
      (buildNumberTable winnerString)
      (buildNumberTable numberString)))))

(print "part one:")
(println (reduce + (map evaluateLine (string/split-lines input))))

; PART TWO

(defn getCardId [line]
  (edn/read-string (subs line 4 (.indexOf line ":"))))

; returns a map from card ids to their matching numbers
(defn buildGameTable [in]
  (loop [m {} lines (string/split-lines in)]
    (if (empty? lines)
      m
      (let [current (first lines)
            cardID (getCardId current)
            parts (string/split (subs current (inc (.indexOf current ":"))) #"\|")
            winnerString (first parts)
            numberString (last parts)]
        (recur
         (assoc
          m
          cardID
          (findMatches
           (buildNumberTable winnerString)
           (buildNumberTable numberString)))
         (rest lines))))))

(defn findTotalWonCards [game cardId]
  (let [winners (get game cardId)]
    (if (not winners)
      1
      (inc (reduce
            +
            (map (partial findTotalWonCards game)
                 (range (inc cardId) (+ 1 cardId (count winners)))))))))

(print "part two:")
(println
 (let [game (buildGameTable input)]
   (reduce
    +
    (map
     (partial findTotalWonCards game)
     (range 1 (inc (count game)))))))
