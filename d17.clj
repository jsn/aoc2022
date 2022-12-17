(ns d17
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def ^:private ROCKS
  [[[0 0] [0 1] [0 2] [0 3]]
   [[2 1] [1 0] [1 1] [1 2] [0 1]]
   [[2 2] [1 2] [0 0] [0 1] [0 2]]
   [[0 0] [1 0] [2 0] [3 0]]
   [[1 0] [1 1] [0 0] [0 1]]])

(def ^:private INITIAL
  {:stops #{}
   :tick 0
   :rock 0})

(defn- fall1 [winds {:keys [stops tick rock] :as world}]
  (let [h (apply max -1 (map first stops))
        ri (mod rock (count ROCKS))
        shape (mapv #(-> % (update 0 + 4 h) (update 1 + 2)) (ROCKS ri))]
    (loop [shape shape
           tick tick]
      (let [wind (get winds (mod tick (count winds)))
            dx (have! ({\> 1 \< -1} wind))
            shape' (mapv #(update % 1 + dx) shape)
            shape' (if (some #(or (stops %) (not (<= 0 (second %) 6))) shape')
                     shape
                     shape')
            shape'' (mapv #(update % 0 dec) shape')]
        (if-not (some #(or (stops %) (neg? (first %))) shape'')
          (recur shape'' (inc tick))
          {:stops (into stops shape')
           :tick (inc tick)
           :rock (inc rock)})))))

(defn- world->string [{:keys [stops tick rock]}]
  (let [h (apply max 0 (map first stops))]
    (str/join "\n"
      (into [(str "tick: " tick)]
        (for [y (range h -1 -1)]
          (apply str (map #(if (stops [y %]) "#" ".") (range 7))))))))

(defn one [s]
  (let [s (str/trim s)
        f #(fall1 s %)]
    (->> INITIAL
      (iterate f)
      (drop 2022)
      first
      :stops
      (map first)
      (apply max)
      inc)))

(deftest t-1
  (is (= (one t1) 3068)))

(def ^:private MUCH 1000000000000)

(defn two [s]
  (let [s (str/trim s)
        f #(fall1 s %)]
    (loop [world INITIAL
           seen {}
           log []
           est nil]
      (let [{:keys [tick rock stops]} world
            ri (mod rock (count ROCKS))
            wi (mod tick (count s))
            h (apply max 0 (map first stops))
            k [ri wi]
            log' (conj log [h wi rock])
            est'
            (if-let [old (seen k)]
              (let [[ho wio rocko] (log old)
                    period (- rock rocko)
                    left (- MUCH rock)]
                (if (zero? (mod left period))
                  (+ h (* (- h ho) (quot left period)))
                  est))
              est)]
        (if (and est (= est est'))
          (inc est)
          (recur (f world) (assoc seen k rock) log' est'))))))

(deftest t-2
  (is (= (two t1) 1514285714288)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
