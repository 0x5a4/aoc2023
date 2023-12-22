(ns aoc-2023.day1)
(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])
(require '[clojure.string :as string])

(def input (slurp (io/resource "day1")))

; PART ONE
(defn calibrationValue [line]
  (let [nums (filter #(.contains "1234567890" (str %)) (seq line))]
    (edn/read-string (str (first nums) (last nums)))))

(print "part one: ")
(println (reduce + (map calibrationValue (string/split-lines input))))

; PART TWO

(def mapping {"one"   1
              "two"   2
              "three" 3
              "four"  4
              "five"  5
              "six"   6
              "seven" 7
              "eight" 8
              "nine"  9
              "zero"  0
              "1"     1
              "2"     2
              "3"     3
              "4"     4
              "5"     5
              "6"     6
              "7"     7
              "8"     8
              "9"     9
              "0"     0})

(defn extractNumbers [line]
  (loop [s line coll []]
    (if (= (count s) 0)
      coll
      (recur
       (subs s 1)
       (let [match (first (re-find #"^(one|two|three|four|five|six|seven|eight|nine|zero|1|2|3|4|5|6|7|8|9|0)" s))]
         (if match
           (conj coll (get mapping match))
           coll))))))

(defn calibrationValueFromLetters [line]
  (let [numbers (extractNumbers line)]
    (edn/read-string (str (first numbers) (last numbers)))))

(print "part two: ")
(println (reduce + (map calibrationValueFromLetters (string/split-lines input))))
