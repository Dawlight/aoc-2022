(ns aoc-2022.day01.solution
  (:require [clojure.string :as str]))

(def input-test (str/trim (slurp "./src/aoc_2022/day01/input_test")))
(def input-real (str/trim (slurp "./src/aoc_2022/day01/input_real")))

(defn sum-group [group]
  (reduce + (map #(Integer/parseInt %) (str/split group #"\n"))))

(defn compare-reverse [x, y]
  (compare y x))

(defn solution-a [input]
  (->> (map sum-group (str/split input #"\n\n"))
       (sort compare-reverse)
       (take 1)))


(defn solution-b [input]
  (->> (map sum-group (str/split input #"\n\n"))
       (sort compare-reverse)
       (take 3)
       (reduce +)))

(solution-a input-real)
(solution-b input-real)