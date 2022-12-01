(ns d01
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn- parse [s]
  (map u/string->vector (str/split s #"\n\s*\n")))

(defn one [s]
  (->> s parse (map #(apply + %)) (apply max)))

(deftest t-1
  (is (= (one t1) 24000)))

(defn two [s]
  (->> s parse (map #(apply + %)) sort reverse (take 3) (apply +)))

(deftest t-2
  (is (= (two t1) 45000)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
