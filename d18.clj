(ns d18
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(defn- parse [s]
  (->> s u/string->vector (partition 3) (map vec) set))

(defn- neighb [p]
  (for [c [0 1 2] dc [-1 1]]
    (update p c + dc)))

(defn one [s]
  (let [cubes (parse s)]
    (->> cubes (mapcat neighb) (remove cubes) count)))

(deftest t-1
  (is (= (one t1) 64)))

(defn- in? [box p]
  (every? #(<= (first (box %)) (p %) (second (box %))) [0 1 2]))

(defn- flood [cubes box]
  (loop [border #{(mapv first box)}
         inner #{}]
    (if-not (seq border)
      inner
      (let [inner' (into inner border)
            border' (->> border
                      (mapcat neighb)
                      (remove cubes)
                      (filter #(in? box %))
                      (remove inner')
                      set)]
        (recur border' inner')))))

(defn two [s]
  (let [cubes (->> s parse set)
        box (vec
              (for [c [0 1 2]
                    :let [cs (map #(% c) cubes)]]
                [(dec (apply min cs)) (inc (apply max cs))]))
        outer (flood cubes box)]
    (->> cubes (mapcat neighb) (filter outer) count)))

(deftest t-2
  (is (= (two t1) 58)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
