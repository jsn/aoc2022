(ns d16
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
"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(def ^:private LINE
  #"Valve (\S+) has flow rate=(\d+); tunnels? leads? to valves? (\S.*)")

(defn- parse-line [l]
  (let [[_ src rate dsts] (have! (re-matches LINE l))]
    [(keyword src)
     (parse-long rate)
     (set (map keyword (str/split dsts #"[\s,]+")))]))

(defn- flood [conns start]
  (loop [cnt 1
         seen {start 0}
         border #{start}]
    (let [border' (->> border (keep conns) (apply set/union) (remove seen))]
      (if-not (seq border')
        seen
        (recur
          (inc cnt)
          (into seen (map vector border' (repeat cnt)))
          border')))))

(defn parse [s]
  (let [rooms (mapv parse-line (str/split-lines s))
        rates (into {} (map (juxt first second) rooms))
        conns (into {} (map (juxt first #(% 2)) rooms))
        costs (into {} (map (fn [r] [r (flood conns r)]) (keys rates)))]
    {:rates rates :conns conns :p :AA :costs costs :flow 0 :tick 0 :score 0}))

(def ^:private ^:dynamic *LIMIT* 30)

(defn- advance1 [{:keys [score p rates flow costs tick] :as world} dst]
  (let [cost (get-in costs [p dst] 1000)
        tick' (+ tick cost 1)]
    (if (>= tick' *LIMIT*) nil
      (assoc world
        :p dst
        :rates (assoc rates dst 0)
        :tick tick'
        :score (+ score (* flow (inc cost)))
        :flow (+ flow (rates dst))))))

(defn- stay [{:keys [tick flow score] :as world}]
  (let [dt (- *LIMIT* tick)]
    (assoc world
      :tick *LIMIT*
      :score (+ score (* dt flow)))))

(defn- advance [{:keys [rates] :as world}]
  (into
    [(stay world)] 
    (for [[n r] rates
          :when (> r 0)
          :let [w (advance1 world n)]
          :when w]
      w)))

(defn- visited [w1 w2]
  (set/difference
    (set (for [[n r] (:rates w1) :when (pos? r)] n))
    (set (for [[n r] (:rates w2) :when (pos? r)] n))))

(defn- enumerate-paths [world]
  (loop [border [world]
         stays #{}]
    (if-not (seq border)
      (reduce
        #(let [vs (visited world %2)
               score (max (get % vs 0) (:score %2))]
           (assoc %1 vs score))
        {}
        stays)
      (let [{border' false stayed true}
            (group-by #(= (:tick %) *LIMIT*) (mapcat advance border))]
        (recur border' (into stays stayed))))))

(defn one [s]
  (->> s parse enumerate-paths vals (apply max)))

(deftest t-1
  (is (= (one t1) 1651)))

(defn two [s]
  (binding [*LIMIT* 26]
    (let [paths (vec (enumerate-paths (parse s)))]
      (apply max
        (for [i1 (range (count paths))
              i2 (range (inc i1) (count paths))
              :let [[p1 sc1] (paths i1)
                    [p2 sc2] (paths i2)]
              :when (empty? (set/intersection p1 p2))]
          (+ sc1 sc2))))))

(deftest t-2
  (is (= (two t1) 1707)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
