(ns d04
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn- parse-line [l]
  (->> l
    (re-matches #"(\d+)-(\d+),(\d+)-(\d+)")
    have!
    rest
    (mapv parse-long)))

(defn- contained? [[ps pe qs qe]]
  (or (<= ps qs qe pe) (<= qs ps pe qe)))

(defn one [s]
  (->> s str/split-lines (map parse-line) (filter contained?) count))

(deftest t-1
  (is (= (one t1) 2)))

(defn- overlap? [[ps pe qs qe]]
  (or (<= ps qs pe) (<= ps qe pe) (<= qs ps qe)))

(defn two [s]
  (->> s str/split-lines (map parse-line) (filter overlap?) count))

(deftest t-2
  (is (= (two t1) 4)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
