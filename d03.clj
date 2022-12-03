(ns d03
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn- score [c]
  (let [ci (int c)]
    (cond
      (<= (int \a) ci (int \z)) (+ ci (- (int \a)) 1)
      (<= (int \A) ci (int \Z)) (+ ci (- (int \A)) 27)
      :else (assert nil))))

(defn- solve1 [l]
  (let [c (/ (count l) 2)
        ls (map set (split-at c l))]
    (apply set/intersection ls)))

(defn one [s]
  (->> s
    str/split-lines
    (mapcat solve1)
    (map score)
    (apply +)))

(deftest t-1
  (is (= (one t1) 157)))

(defn two [s]
  (->> s
    str/split-lines
    (map set)
    (partition 3)
    (mapcat #(apply set/intersection %))
    (map score)
    (apply +)))

(deftest t-2
  (is (= (two t1) 70)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
