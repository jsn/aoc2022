(ns d12
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [astar.core :as a*]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn- parse [s]
  (let [ls (str/split-lines s)
        h (count ls)
        w (count (first ls))
        chrs (for [y (range h) x (range w)] [(get-in ls [y x]) [y x]])]
    (reduce
      (fn [world [c p]]
        (case c
          \S (assoc world :src p p 0)
          \E (assoc world :dst p p (- (int \z) (int \a)))
          (assoc world p (- (int c) (int \a)))))
      {} chrs)))

(defn- solve1 [{:keys [src dst] :as world}]
  (let [graph
        (fn [p]
          (for [d [[-1 0] [1 0] [0 -1] [0 1]]
                :let [q (mapv + p d)
                      qv (world q)]
                :when (and qv (<= qv (inc (world p))))]
            q))

        dist (fn [p q] (apply + (map #(Math/abs ^long %) (map - p q))))
        h #(dist % dst)]
    (a*/route graph dist h src dst)))

(defn one [s]
  (count (solve1 (parse s))))

(deftest t-1
  (is (= (one t1) 31)))

(defn- solve2 [{:keys [dst] :as world}]
  (let [graph
        (fn [p]
          (for [d [[-1 0] [1 0] [0 -1] [0 1]]
                :let [q (mapv + p d)
                      qv (world q)]
                :when (and qv (<= (world p) (inc qv)))]
            q))]
    (loop [cnt 0
           seen #{}
           border #{dst}]
      (if (->> border (map world) (apply min) zero?) cnt
        (let [border' (->> border (mapcat graph) (remove seen) set)
              seen' (into seen border)
              cnt' (inc cnt)]
          (recur cnt' seen' border'))))))

(defn two [s]
  (solve2 (parse s)))

(deftest t-2
  (is (= (two t1) 29)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
