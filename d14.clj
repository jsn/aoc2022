(ns d14
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn- parse [s]
  (->> s
    str/split-lines
    (mapv #(->> (str/replace % #"[^\s\d]+" " ")
             u/string->vector 
             (partition 2)
             (mapv vec)))))

(defn- rrange [a b]
  (if (> a b) (rrange b a) (range a (inc b))))

(defn- line->points [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (for [y (rrange y1 y2)] [x1 y])
    (= y1 y2) (for [x (rrange x1 x2)] [x y1])
    :else (assert nil)))

(def ^:private START [500 0])

(defn- fall1 [w]
  (let [my (apply max (map second w))]
    (loop [[x y :as p] START
           w w]
      (if-let [p' (first
                    (for [dx [0 -1 1]
                          :let [p [(+ x dx) (inc y)]]
                          :when (not (w p))]
                      p))]
        (if (> (second p) my)
          w
          (recur p' w))
        (conj w p)))))

(defn one [s]
  (let [lines (parse s)
        w (set (mapcat #(mapcat line->points (partition 2 1 %)) lines))]
    (->> w
      (iterate fall1)
      (partition 2 1)
      (keep-indexed #(when (apply = %2) %1))
      first)))

(deftest t-1
  (is (= (one t1) 24)))

(def ^:private ^:dynamic *floor* nil)

(defn- fall2 [w]
  (loop [[x y :as p] START
         w w]
    (if (= y *floor*)
      (conj w p)
      (if-let [p' (first
                    (for [dx [0 -1 1]
                          :let [p [(+ x dx) (inc y)]]
                          :when (not (w p))]
                      p))]
        (recur p' w)
        (conj w p)))))

(defn two [s]
  (let [lines (parse s)
        w (set (mapcat #(mapcat line->points (partition 2 1 %)) lines))]
    (binding [*floor* (inc (apply max (map second w)))]
      (->> w
        (iterate fall2)
        (partition 2 1)
        (keep-indexed #(when (apply = %2) %1))
        first))))

(deftest t-2
  (is (= (two t1) 93)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
