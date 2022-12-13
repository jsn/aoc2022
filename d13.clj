(ns d13
  (:require
    [clojure.string :as str]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn- cmp [l r]
  (match [(number? l) (number? r)]
    [true true] (compare l r)

    [false false]
    (or (first (remove zero? (map cmp l r))) (compare (count l) (count r)))

    [true false] (cmp [l] r)
    [false true] (cmp l [r])))

(defn one [s]
  (->> s
    u/string->vector
    (partition 2)
    (keep-indexed #(when (= -1 (apply cmp %2)) (inc %1)))
    (apply +)))

(deftest t-1
  (is (= (one t1) 13)))

(def ^:private DIVS #{[[2]] [[6]]})

(defn two [s]
  (->> s
    u/string->vector
    (concat DIVS)
    (sort cmp)
    (keep-indexed #(when (DIVS %2) (inc %1)))
    (apply *)))

(deftest t-2
  (is (= (two t1) 140)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
