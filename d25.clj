(ns d25
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(def ^:private DIG {\0 0 \1 1 \2 2 \- -1 \= -2})
(def ^:private GID (into {} (map (juxt val key) DIG)))

(defn- parse-line [l]
  (apply + (map #(* (DIG %1) %2) (reverse l) (iterate #(* 5 %) 1))))

(def ^:private SEQ
  (->> 20
    range
    (map #(long (* 2 (Math/pow 5 %))))
    (reductions +)
    vec))

(defn- n->snafu [n]
  (loop [n n
         ready []
         top (some #(when (<= (abs n) (SEQ %)) %) (range (count SEQ)))]
    (let [add (if (zero? top) 0 (SEQ (dec top)))
          dp (long (Math/pow 5 top))
          dn (cond-> (quot (+ (abs n) add) dp) (neg? n) (* -1))
          d  (have! (GID dn))
          ready (conj ready d)]
      (if (zero? top)
        (apply str ready)
        (recur (- n (* dp dn)) ready (dec top))))))

(deftest snafu
  (is (= (n->snafu 250) "2000"))
  (is (= (n->snafu 314159265) "1121-1110-1=0")))

(defn one [s]
  (->> s str/split-lines (mapv parse-line) (apply +) n->snafu))

(deftest t-1
  (is (= (one t1) "2=-1=0")))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))))

(comment
  (-main))
