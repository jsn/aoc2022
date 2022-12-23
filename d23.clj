(ns d23
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
".....
..##.
..#..
.....
..##.
.....")

(def ^:private t2
"....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(defn- parse [s]
  (let [ls (->> s str/trim str/split-lines vec)
        h (count ls)
        w (count (first ls))]
    (set
      (for [y (range h)
            x (range w)
            :when (= \# (get-in ls [y x]))]
        [x y]))))

(def ^:private DIRS
  [[[0 -1] [-1 -1] [1 -1]]
   [[0 1] [-1 1] [1 1]]
   [[-1 0] [-1 -1] [-1 1]]
   [[1 0] [1 -1] [1 1]]])

(defn- neighb [p]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    (mapv + p [dx dy])))

(defn- move1 [w i p]
  (first
    (for [di (range i (+ i (count DIRS)))
          :let [ds (DIRS (mod di (count DIRS)))
                tgts (map #(mapv + p %) ds)]
          :when (not-any? w tgts)]
      (first tgts))))

(defn- round1 [w i]
  (let [movables (filter #(some w (neighb %)) w)
        moves (keep #(when-let [dst (move1 w i %)] [% dst]) movables)
        kosher (->> moves
                 (map second)
                 frequencies
                 (keep #(when (= 1 (val %)) (key %)))
                 set)
        moves (filter #(kosher (second %)) moves)]
    (-> w
      (set/difference (set (map first moves)))
      (into (map second moves)))))

(defn- score [w]
  (let [xs (map first w)
        ys (map second w)]
    (- (*
        (inc (- (apply max xs) (apply min xs)))
        (inc (- (apply max ys) (apply min ys))))
      (count w))))

(defn- w->s [w]
  (let [xs (map first w)
        ys (map second w)
        x0 (apply min xs)
        x1 (apply max xs)
        y0 (apply min ys)
        y1 (apply max ys)]
    (str/join "\n"
      (for [y (range y0 (inc y1))]
        (apply str
          (for [x (range x0 (inc x1))]
            (if (w [x y]) \# \.)))))))

(defn one [s]
  (let [w (parse s)]
    (score (reduce round1 w (range 10)))))

(deftest t-1
  (is (= (one t1) 25))
  (is (= (one t2) 110)))

(defn two [s]
  (let [w (parse s)]
    (->> (range)
      (reductions round1 w)
      (partition 2 1)
      (take-while #(apply not= %))
      count
      inc)))

(deftest t-2
  (is (= (two t2) 20)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
