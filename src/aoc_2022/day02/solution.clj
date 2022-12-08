(ns aoc-2022.day02.solution
  (:require [clojure.string :as str]))

(def input-test (str/trim (slurp "./src/aoc_2022/day02/input_test")))
(def input-real (str/trim (slurp "./src/aoc_2022/day02/input_real")))

(def op-mapping {"A" :rock
                 "B" :paper
                 "C" :scissor})

(def me-mapping {"X" :rock
                 "Y" :paper
                 "Z" :scissor})

(def win-rules {:rock :paper
                :paper :scissor
                :scissor :rock})

(def base-scoring {:rock 1
                   :paper 2
                   :scissor 3})



(defn transpose [matrix]
  (apply mapv vector matrix))

(defn map-moves [op, me]
  (vector
   (get op-mapping op)
   (get me-mapping me)))

(defn get-round-score [op-move, me-move]
  (let [winning-move (get win-rules op-move)
        move-base-score (get base-scoring me-move)] 
    (cond
      (= winning-move me-move) (+ move-base-score 6)
      (= op-move me-move) (+ move-base-score 3)
      :else move-base-score
      )))


(defn solve-a [input] 
  (->> (map #(str/split % #" ") (str/split input #"\n"))
       (into [])
       (transpose)
       (apply map map-moves)
       (into [])
       (transpose)
       (apply map get-round-score)
       (reduce +)))

(solve-a input-real)

(defn ring-nth [coll, index]
  (nth coll (mod index (count coll))))

(def hierarchy-offset {"X" -1
                       "Y" 0
                       "Z" 1})

(def hierarchy ["A", "B", "C"])

(def outcome-score {"X" 0
                    "Y" 3
                    "Z" 6})

(defn to-me-move-and-coutcome [op-move, outcome]
  (let [offset (get hierarchy-offset outcome)
        op-move-index (.indexOf hierarchy op-move)]
    (vector (ring-nth hierarchy (+ op-move-index offset)) outcome)))

(defn to-score [me-move, outcome]
  (vector (+ (.indexOf hierarchy me-move) 1) (get outcome-score outcome)))


(defn solve-b [input] 
  (->> (map #(str/split % #" ") (str/split input #"\n"))
       (into [])
       (transpose)
       (apply map to-me-move-and-coutcome)
       (into [])
       (transpose)
       (apply map to-score)
       (into [])
       (transpose)
       (apply map +)
       (reduce +)))

(solve-b input-real)