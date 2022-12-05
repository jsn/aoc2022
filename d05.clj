(ns d05
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn- parse-stacks [s]
  (let [stacks (->> s str/split-lines reverse rest)
        cols (->> (range 10)
               (map #(inc (* 4 %)))
               (filter #(< % (count (first stacks)))))]
    (mapv
      (fn [col]
        (->> stacks
          (map #(get % col))
          (remove #{\space})
          vec))
      cols)))

(defn- parse-cmds [s]
  (->> s
    str/split-lines
    (mapv #(->> % (re-seq #"\d+") (mapv parse-long)))))

(defn- parse [s]
  (let [[stacks cmds] (str/split s #"\n\s*\n")
        stacks (parse-stacks stacks)
        cmds (parse-cmds cmds)]
    [stacks cmds]))

(defn- run1 [stacks [cnt & tail]]
  (let [[si di] (map dec tail)
        src (stacks si)
        dst (stacks di)
        [src' moved] (split-at (- (count src) cnt) src)
        dst' (into dst (reverse moved))]
    (-> stacks
      (assoc si (vec src'))
      (assoc di dst'))))

(defn one [s]
  (->> s parse (apply reduce run1) (map peek) (apply str)))

(deftest t-1
  (is (= (one t1) "CMZ")))

(defn- run2 [stacks [cnt & tail]]
  (let [[si di] (map dec tail)
        src (stacks si)
        dst (stacks di)
        [src' moved] (split-at (- (count src) cnt) src)
        dst' (into dst moved)]
    (-> stacks
      (assoc si (vec src'))
      (assoc di dst'))))

(defn two [s]
  (->> s parse (apply reduce run2) (map peek) (apply str)))

(deftest t-2
  (is (= (two t1) "MCD")))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
