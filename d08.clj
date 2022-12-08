(ns d08
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"30373
25512
65332
33549
35390")

(defn- parse [s]
  (mapv #(mapv parse-long (re-seq #"\d" %)) (str/split-lines s)))

(defn- hidden? [w [y x]]
  (let [len (count w)
        c (get-in w [y x])
        row (w y)
        col (mapv #(get % x) w)]
    (and
      (some #(>= % c) (subvec row 0 x))
      (some #(>= % c) (subvec row (inc x) len))
      (some #(>= % c) (subvec col 0 y))
      (some #(>= % c) (subvec col (inc y) len)))))

(defn one [s]
  (let [w (parse s)
        len (count w)

        vis
        (for [y (range len)
              x (range len)]
          (hidden? w [y x]))]
    ((frequencies vis) nil)))

(deftest t-1
  (is (= (one t1) 21)))

(defn- count-to [x sq]
  (reduce #(cond-> (inc %1) (>= %2 x) reduced) 0 sq))

(defn scenic [w [y x]]
  (let [len (count w)
        c (get-in w [y x])
        row (w y)
        col (mapv #(get % x) w)]
    (*
      (count-to c (reverse (subvec row 0 x)))
      (count-to c (subvec row (inc x) len))
      (count-to c (reverse (subvec col 0 y)))
      (count-to c (subvec col (inc y) len)))))

(defn two [s]
  (let [w (parse s)
        len (count w)

        scenics
        (for [y (range len)
              x (range len)]
          (scenic w [y x]))]
    (apply max scenics)))

(deftest t-2
  (is (= (two t1) 8)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
