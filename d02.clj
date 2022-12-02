(ns d02
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"A Y
B X
C Z")

(def ^:private S
  {\A 1
   \B 2
   \C 3
   \X 1
   \Y 2
   \Z 3})

(defn- parse [s]
  (->> (re-seq #"\S" s) (map first) (partition 2) (map vec)))

(def ^:private WINS
  {1 3
   2 1
   3 2})

(defn- score1 [moves]
  (let [[they us] (map S moves)]
    (+ us
      (cond
        (= they us) 3
        (= (WINS us) they) 6
        :else 0))))

(defn one [s]
  (->> s parse (map score1) (apply +)))

(deftest t-1
  (is (= (one t1) 15)))

(def ^:private LOSSES
  (into {} (map (juxt val key) WINS)))

(defn- score2 [[them outcome]]
  (let [them (S them)]
    (case outcome
      \X (+ 0 (WINS them))
      \Y (+ 3 them)
      \Z (+ 6 (LOSSES them)))))

(defn two [s]
  (->> s parse (map score2) (apply +)))

(deftest t-2
  (is (= (two t1) 12)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
