(ns d20
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"1
2
-3
3
-2
0
4")

(defn- swap [v pos pos2]
  (let [l (count v)]
    (cond
      (= pos pos2) v

      (> pos2 pos)
      (into (subvec v 0 pos)
        (concat
          (subvec v (inc pos) (inc pos2))
          [(v pos)]
          (subvec v (inc pos2) l)))

      (< pos2 pos)
      (into (subvec v 0 pos2)
        (concat
          [(v pos)]
          (subvec v pos2 pos)
          (subvec v (inc pos) l))))))

(defn- move [v vi pos]
  (let [l (count v)
        n (v pos)
        pos2 (mod (+ n pos) (dec l))]
    (cond->> [v vi]
      (not= pos pos2)
      (mapv #(swap % pos pos2)))))

(defn- solve1 [v mul]
  (let [l (count v)
        vi (vec (range l))]
    (loop [v v
           vi vi
           cnt 0]
      (if (= cnt (* l mul)) v
        (let [i (first (keep-indexed #(when (= %2 (mod cnt l)) %1) vi))
              [v' vi'] (move v vi i)]
          (recur v' vi' (inc cnt)))))))

(defn one [s]
  (let [v (solve1 (u/string->vector s) 1)
        zi (first (keep-indexed #(when (zero? %2) %1) v))]
    (apply +
      (map #(v (mod (+ zi %) (count v))) [1000 2000 3000]))))
        
(deftest t-1
  (is (= (one t1) 3)))

(def ^:private KEY 811589153)

(defn two [s]
  (let [v (solve1 (mapv #(* KEY %) (u/string->vector s)) 10)
        zi (first (keep-indexed #(when (zero? %2) %1) v))]
    (apply +
      (map #(v (mod (+ zi %) (count v))) [1000 2000 3000]))))

(deftest t-2
  (is (= (two t1) 1623178306)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
