(ns d09
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn- parse-line [l]
  (let [[d n] (str/split l #"\s+")]
    [(parse-long n) (have! #{:R :U :L :D} (keyword d))]))

; y x

(def ^:private DIRS {:R [0 1] :L [0 -1] :U [1 0] :D [-1 0]})

(defn- run-step [[h [ty tx :as t]] d]
  (let [[hy hx :as h] (mapv + (have! (DIRS d)) h)
        [^long vy ^long vx :as v] (mapv - h t)
        t (if-not (some #(not (<= -1 % 1)) v)
            t
            (cond
              (= hx tx) [(+ ty (/ vy (Math/abs vy))) tx]
              (= hy ty) [ty (+ tx (/ vx (Math/abs vx)))]
              :else [(+ ty (/ vy (Math/abs vy))) (+ tx (/ vx (Math/abs vx)))]))]
    [h t]))

(defn one [s]
  (->> s
    str/split-lines
    (mapcat #(apply repeat (parse-line %)))
    (reductions run-step [[0 0] [0 0]])
    (map peek)
    set
    count))

(deftest t-1
  (is (= (one t1) 13)))

(defn- pull [[hy hx :as h] [ty tx :as t]]
  (let [[^long vy ^long vx :as v] (mapv - h t)]
    (if-not (some #(not (<= -1 % 1)) v)
      t
      (cond
        (= hx tx) [(+ ty (/ vy (Math/abs vy))) tx]
        (= hy ty) [ty (+ tx (/ vx (Math/abs vx)))]
        :else [(+ ty (/ vy (Math/abs vy))) (+ tx (/ vx (Math/abs vx)))]))))

(defn- run-step2 [[h & ts] d]
  (let [h (mapv + (have! (DIRS d)) h)]
    (vec (reductions pull h ts))))

(defn two [s]
  (->> s
    str/split-lines
    (mapcat #(apply repeat (parse-line %)))
    (reductions run-step2 (vec (repeat 10 [0 0])))
    (map peek)
    set
    count))

(def ^:private t2
"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest t-2
  (is (= (two t2) 36)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
