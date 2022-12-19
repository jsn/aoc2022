(ns d19
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
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

(defn- flood2 [bp]
  (loop [border #{INITIAL}
         done #{}]
    (let [nexts
          (set
            (for [state border
                  move (keys bp)]
              (do-bot state bp move)))

          {done' true border' false}
          (group-by #(= (:tick %) *LIMIT*) nexts)]
      (if (seq border')
        (recur border' (into done done'))
        (->> done (keep #(get-in % [:got :geode])) (apply max 0))))))

(defn one [s]
  (let [bps (->> (str/split s #"Blueprint (\d+):\s*")
              rest
              (mapv parse-stanza))]
    (binding [*LIMIT* 24]
      (apply +
        (for [i (range (count bps))
              :let [qua (* (inc i) (flood2 (bps i)))
                    ;_ (prn ::here i qua)
                    ]]
          qua)))))

#_(one (slurp "d19.in"))

(deftest t-1
  (is (= (one t1) 33)))

(defn- flood4 [state bp]
  (loop [border #{state}
         done #{}]
    (let [nexts
          (set
            (for [state border
                  move (keys bp)
                  :when (not= move :ore)]
              (do-bot state bp move)))

          {done' true border' false}
          (group-by #(= (:tick %) *LIMIT*) nexts)]
      (if (seq border')
        (recur border' (into done done'))
        (->> done (keep #(get-in % [:got :geode])) (apply max 0))))))

(defn- solve2 [bp]
  (apply max
    (for [ini (take-while #(not= *LIMIT* (:tick %))
                (iterate #(do-bot % bp :ore) INITIAL))
          ini2 (take-while #(not= *LIMIT* (:tick %))
                 (drop 6 (iterate #(do-bot % bp :clay) ini)))]
      (flood4 ini2 bp))))

(defn two [s]
  (let [bps (->> (str/split s #"Blueprint (\d+):\s*")
              rest
              (mapv parse-stanza))]
    (binding [*LIMIT* 32]
      (apply *
        (for [bp (take 3 bps)]
          (solve2 bp))))))

#_(two (slurp "d19.in"))

(deftest t-2
  (is (= (two t1) (* 56 62))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
