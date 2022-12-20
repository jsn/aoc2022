(ns d19bis
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.math.combinatorics :as c]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")

(defn- parse-stanza [s]
  (->> s
    (re-seq #"Each (\w+) robot costs ([^\.]+)\.")
    (map (fn [[_ dst src]]
           [(keyword dst)
            (into {}
              (map
                #(let [[_ q m] %]
                   [(keyword m) (parse-long q)])
                (re-seq #"(\d+) (\w+)" src)))]))
    (into {})))

(defn- parse [s]
  (->> (str/split s #"Blueprint (\d+):\s*") rest (mapv parse-stanza)))

(def ^:private T1-BPS
  (parse t1))

(def ^:private INITIAL {:bots {:ore 1} :got {} :tick 0})

(defn- build [got bspec]
  (let [got' (reduce #(update %1 (key %2) (fnil - 0) (val %2)) got bspec)]
    (when-not (some neg? (vals got'))
      got')))

(defn- collect [got bots]
  (reduce #(update %1 (key %2) (fnil + 0) (val %2)) got bots))

(defn- try-bot [{:keys [bots got] :as state} bp bot]
  (let [got' (cond-> got bot (build (bp bot)))]
    (when got'
      (let [got'' (collect got' bots)
            state' (-> state (assoc :got got'') (update :tick inc))]
        (cond-> state'
          bot (update-in [:bots bot] (fnil inc 0)))))))

(def ^:private ^:dynamic *LIMIT* 24)

(defn- do-bot [state bp bot]
  (loop [state state]
    (if (= *LIMIT* (:tick state)) state
      (or
        (try-bot state bp bot)
        (recur (try-bot state bp nil))))))

(defn- limits [bp]
  (->
    (->> bp
      keys
      (map #(->> bp vals (keep %) (apply max 0) (vector %)))
      (into {}))
    (update :ore dec)
    (assoc :geode *LIMIT*)))

#_(limits (T1-BPS 0))

(defn- score [state]
  (get-in state [:got :geode] 0))

(defn- solve6 [state bp lims]
  (if (= *LIMIT* (:tick state))
    (score state)
    (let [moves (keep #(when (pos? (val %)) (key %)) lims)]
      (apply max
        (for [move moves
              :let [lims' (update lims move dec)
                    state' (do-bot state bp move)]]
          (solve6 state' bp lims'))))))

#_(binding [*LIMIT* 24] (solve6 INITIAL (T1-BPS 1) (limits (T1-BPS 1))))

(defn one [s]
  (let [bps (parse s)]
    (apply +
      (for [i (range (count bps))
            :let [bp (bps i)
                  ;_ (prn i)
                  lims (limits bp)]]
        (* (inc i) (solve6 INITIAL bp lims))))))

#_(time (one (slurp "d19.in")))

(deftest t-1
  (is (= (one t1) 33)))

(defn- solve6-2 [bp]
  (let [{:keys [ore] :as lims} (limits bp)
        lims' (-> lims (dissoc :ore))
        mino (dec (apply min (map :ore (vals bp))))]
    (apply max
      (for [o (range mino (inc ore))
            :let [state (reduce #(do-bot %1 bp %2) INITIAL (repeat o :ore))]]
        (solve6 state bp lims')))))

#_(time (binding [*LIMIT* 32] (solve6-2 (T1-BPS 1))))

(defn two [s]
  (let [bps (parse s)]
    (binding [*LIMIT* 32]
      (apply *
        (for [bp (take 3 bps)]
          (solve6-2 bp))))))

#_(time (two (slurp "d19.in")))

(deftest t-2
  (is (= (two t1) (* 56 62))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d19.in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
