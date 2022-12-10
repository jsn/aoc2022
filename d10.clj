(ns d10
  (:require
    [clojure.string :as str]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse-line [l]
  (let [[cmd & args] (str/split l #"\s+")]
    (into [(have! #{:noop :addx} (keyword cmd))] (map parse-long args))))

(defn- run [cmds]
  (loop [cs [1]
         [cmd & cmds] cmds]
    (if-not cmd cs
      (let [x (peek cs)]
        (match cmd
          [:noop] (recur (conj cs x) cmds)
          [:addx arg] (recur (conj cs x (+ x arg)) cmds))))))

(defn one [s]
  (let [cs (->> s str/split-lines (map parse-line) run)]
    (apply + (map #(* % (cs (dec %))) [20 60 100 140 180 220]))))

(deftest t-1
  (is (= (one (slurp "d10.t")) 13140)))

(defn two [s]
  (let [cs (->> s str/split-lines (map parse-line) run)]
    (doseq [py (range 6)
            px (range 40)]
      (when (zero? px)
        (println))
      (let [x (cs (+ px (* 40 py)))]
        (print (if (<= (dec px) x (inc px)) "#" "."))))))

#_(two (slurp "d10.t"))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (print "2.")
    (two input)))

(comment
  (-main))
