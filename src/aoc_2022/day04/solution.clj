(ns aoc-2022.day04.solution
  (:require [clojure.string :as str]
            [clojure.set]))

(def input-test (str/trim (slurp "./src/aoc_2022/day04/input_test")))
(def input-real (str/trim (slurp "./src/aoc_2022/day04/input_real")))



(defn transpose [matrix]
  (apply mapv vector matrix))

(def pairs (for [pair (str/split input-real #"\n")]
  (into [] (for [sections (str/split pair #",")] sections))))

(def split-pairs (for [pair pairs]
  (map #(str/split % #"-") pair)))

(defn subset-of-other [set-pair]
  (let [[a b] set-pair] 
    (or (clojure.set/subset? a b) (clojure.set/subset? b a))))

(defn overlap-at-all [set-pair]
  (let [[a b] set-pair]
    (= #{} (clojure.set/intersect a b))))

(->> (map #(Integer/parseInt %) (flatten split-pairs))
     (partition 2)
     (transpose)
     (apply map #(range %1 (+ %2 1)))
     (map set)
     (partition 2)
     (filter subset-of-other)
     (count))

(->> (map #(Integer/parseInt %) (flatten split-pairs))
     (partition 2)
     (transpose)
     (apply map #(range %1 (+ %2 1)))
     (map set)
     (partition 2)
     (filter overlap-at-all)
     (count))



