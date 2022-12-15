(ns d15
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn- parse-line [l]
  (->> l
    (re-matches
      #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
    have!
    rest
    (map parse-long)
    (partition 2)
    (mapv vec)))

(defn- shadow [[[sx sy :as s] b] y]
  (let [r (apply + (map abs (map - s b)))
        dy (abs (- sy y))
        dx (- r dy)]
    (if (< dx 0)
      nil
      [(- sx dx) (+ sx dx)])))

#_(shadow [[8 7] [2 10]] 10)

(defn- add-span [spans [y1 y2 :as span]]
  (loop [ready []
         [[ay1 ay2 :as a] & tail] spans]
    (if-not a
      (conj ready span)
      (if (or (<= (dec ay1) y1 (inc ay2))
            (<= (dec ay1) y2 (inc ay2))
            (<= (dec y1) ay1 (inc y2)))
        (into (conj ready [(min ay1 y1) (max ay2 y2)]) tail)
        (recur (conj ready a) tail)))))

(defn- make-spans [world y]
  (reduce add-span [] (sort-by first (keep #(shadow % y) world))))

(defn- solve1 [s y]
  (let [world (mapv parse-line (str/split-lines s))
        spans-y (make-spans world y)
        beacons (->> world (map second) (filter #(= y (second %))) set)]
    (- (apply + (map #(inc (abs (apply - %))) spans-y)) (count beacons))))

(defn one [s]
  (solve1 s 2000000))

(deftest t-1
  (is (= (solve1 t1 10) 26)))

(defn- cap [[ax1 ax2] [bx1 bx2]]
  (if (or (> bx1 ax2) (< bx2 ax1))
    nil
    [(max ax1 bx1) (min ax2 bx2)]))

(defn- solve2 [lim s]
  (let [world (mapv parse-line (str/split-lines s))]
    (for [y (range (inc lim))
          :let [spans (keep #(cap [0 lim] %) (make-spans world y))]
          :when (> (count spans) 1)
          :let [[[_ ax2] [bx1 _]] spans]]
      [(/ (+ ax2 bx1) 2) y])))

(defn two [s]
  (->> s
    (solve2 4000000)
    (map (fn [[x y]] (+ (* 4000000 x) y)))
    first))

(deftest t-2
  (is (= (first (solve2 20 t1)) [14 11])))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
