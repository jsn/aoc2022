(ns d24
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [clojure.data.priority-map :refer [priority-map-keyfn]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#")

(def ^:private t2
"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(def ^:private DIRS
  {\> [1 0]
   \v [0 1]
   \< [-1 0]
   \^ [0 -1]})

(defn- parse [s]
  (let [ls (->> s str/split-lines vec)
        w (- (count (ls 0)) 2)
        h (- (count ls) 2)
        in (some #(when (= \. (get-in ls [0 (inc %)])) %) (range w))
        out (some #(when (= \. (get-in ls [(inc h) (inc %)])) %) (range w))
        bz
        (vec
          (for [x (range w)
                y (range h)
                :let [dir (DIRS (get-in ls [(inc y) (inc x)]))]
                :when dir]
            [[x y] dir]))]
    {:start [in -1]
     :pos [in -1]
     :goal [out h]
     :bz bz
     :w w
     :h h}))

(def ^:private bz-advance
  (memoize
    (fn [bz w h]
      (mapv
        (fn [[[x y] [dx dy :as d]]]
          [[(mod (+ x dx) w) (mod (+ y dy) h)] d])
        bz))))

(defn- moves [{:keys [pos w h start goal bz] :as state}]
  (let [bz (bz-advance bz w h)
        bzset (set (map first bz))]
    (for [d (into [[0 0]] (vals DIRS))
          :let [[x y :as pos'] (mapv + pos d)]
          :when (or
                  (#{start goal} pos')
                  (and
                    (not (bzset pos'))
                    (<= 0 x (dec w))
                    (<= 0 y (dec h))))]
      (assoc state :pos pos' :bz bz))))

(defn ^:private generate-route [node came-from]
  (loop [route '()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn route
  [graph h start goal?]
  (loop [visited {}
         queue (priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (goal? current)
          (generate-route current visited)
          (recur visited (reduce (fn [queue node]
                                   (let [score (inc current-score)]
                                     (if (and (not (contains? visited node))
                                           (or (not (contains? queue node))
                                             (< score (get-in queue [node 1]))))
                                       (assoc queue node [(+ score (h node)) score current])
                                       queue)))
                           (pop queue)
                           (graph current))))))))

(defn- h [{:keys [pos goal]}]
  (apply + (map (comp abs -) pos goal)))

(defn- goal? [{:keys [pos goal]}]
  (= pos goal))

(defn one [s]
  (count (route moves h (parse s) goal?)))

(deftest t-1
  (is (= (one t2) 18)))

(defn two [s]
  (let [{:keys [start goal] :as state} (parse s)
        route1 (route moves h state goal?)
        state1 (assoc (last route1) :goal start :start goal)
        route2 (route moves h state1 goal?)
        state2 (assoc (last route2) :goal goal :start start)
        route3 (route moves h state2 goal?)]
    (apply + (map count [route1 route2 route3]))))

(deftest t-2
  (is (= (two t2) 54)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
