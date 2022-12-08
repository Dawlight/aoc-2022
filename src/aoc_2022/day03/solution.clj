(ns aoc-2022.day03.solution
  (:require [clojure.string :as str]
            [clojure.set]))

(def input-test (str/trim (slurp "./src/aoc_2022/day03/input_test")))
(def input-real (str/trim (slurp "./src/aoc_2022/day03/input_real")))

(defn get-common-type [& bags] 
  (let [bag-sets (for [bag bags] (set bag))]
    (nth (vec (apply clojure.set/intersection bag-sets)) 0)))

(defn get-priority [type]
  (cond
    (<= 65 (int type) 90 ) (- (int type) 38)
    (<= 97 (int type) 122) (- (int type) 96)))

(defn solve-a [input]
  (let [backpacks (mapv #(split-at (/ (count %) 2) %) (str/split input #"\n"))] 
  (->> (for [backpack backpacks] (apply get-common-type backpack))
       (mapv get-priority)
       (reduce +))))


(solve-a input-test)


(defn solve-b [input]
  (let [groups (mapv #(into [] %) (partition 3 (str/split input #"\n")))]
    (->> (for [group groups] (apply get-common-type group))
         (mapv get-priority)
         (reduce +))))

(solve-b input-real)




