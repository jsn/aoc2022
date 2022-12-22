(ns d22
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5")

(defn- parse-map [s]
  (let [ls (vec (str/split-lines s))]
    (into {}
      (for [y (range (count ls))
            x (range (count (ls y)))
            :let [c (get-in ls [y x])]
            :when (#{\. \#} c)]
        [[x y] c]))))

(defn- parse-moves [s]
  (->> s
    (re-seq #"(?:(\d+)|([LR]))")
    (mapv
      (fn [[_ move dir]] (have! (if move (parse-long move) (keyword dir)))))))

(defn- parse [s]
  (let [[mapp moves] (str/split s #"\n\s*\n")
        mapp (parse-map mapp)]
    {:mapp mapp
     :moves (parse-moves moves)
     :d [1 0]
     :p [(->> mapp keys (keep #(when (zero? (% 1)) (% 0))) (apply min)) 0]}))

(defn- mulv [v mat]
  (mapv #(apply + (map * v %)) mat))

(def ^:private TURNS
  {:L [[0 1] [-1 0]]
   :R [[0 -1] [1 0]]})

(defn- move1 [{:keys [mapp d p] :as world} move]
  (if-let [turn (TURNS move)]
    (update world :d mulv turn)
    (let [c (->> d (keep-indexed #(when-not (zero? %2) %1)) first)
          cz ([1 0] c)
          points (->> mapp keys (filter #(= (p cz) (% cz))) set)]
      (loop [p p
             cnt move]
        (if (zero? cnt)
          (assoc world :p p)
          (let [p1 (mapv + p d)
                p1 (if (points p1) p1
                     (let [f (if (pos? (d c)) min max)]
                       (assoc p c (apply f (map #(% c) points)))))
                ch (have! (mapp p1))]
            (case ch
              \# (assoc world :p p)
              \. (recur p1 (dec cnt)))))))))

#_(reductions move1 {:d [1 0]} [:L :L :L :L])
#_(reductions move1 {:d [1 0]} [:R :R :R :R])

(def ^:private FACINGS
  {[1 0] 0
   [0 1] 1
   [-1 0] 2
   [0 -1] 3})

(defn one [s]
  (let [{:keys [moves] :as world} (parse s)
        {:keys [p d]} (reduce move1 world moves)]
    (+ (FACINGS d) (* 1000 (inc (p 1))) (* 4 (inc (p 0))))))

(deftest t-1
  (is (= (one t1) 6032)))

(defn- find-faces [{:keys [mapp] :as world} w]
  (for [y (range 4)
        x (range 4)
        :let [x0 (* x w)
              x1 (+ x0 w -1)
              y0 (* y w)
              y1 (+ y0 w -1)]
        :when (some #(and (<= x0 (% 0) x1) (<= y0 (% 1) y1)) (keys mapp))]
      [x y]))

(defn- face-center [w face]
  (mapv #(+ (* % w) (/ (dec w) 2)) face))

(defn- turn3d [src dst]
  ; (prn ::turn3d src dst)
  (let [cz (first (keep-indexed #(when %2 %1) (map #(= 0 %1 %2) src dst)))
        csrc (first (keep-indexed #(when-not (zero? %2) %1) src))
        cdst (first (keep-indexed #(when-not (zero? %2) %1) dst))
        sign (/ (dst cdst) (src csrc))]
    (->
      [[0 0 0]
       [0 0 0]
       [0 0 0]]
      (assoc-in [cz cz] 1)
      (assoc-in [cdst csrc] sign)
      (assoc-in [csrc cdst] (- sign)))))

#_(let [v1 [0 1 0]
        v2 [0 0 -1]]
   (reductions mulv [0 0 -1] (repeat 4 (turn3d v1 v2))))

(defn- fold3d [faces face]
  (let [pos [0 0 1]
        dirs (into {} (for [d (keys FACINGS)] [d (conj d 0)]))]
    (loop [border {face {:pos pos :dirs dirs}}
           inner {}]
      (if-not (seq border)
        inner
        (let [inner' (merge inner border)
              border'
              (into {}
                (for [[face {:keys [pos dirs]}] border
                      d (keys dirs)
                      :let [[fx fy :as face'] (mapv + face d)]
                      :when (and
                              (<= 0 fx 3)
                              (<= 0 fy 3)
                              (not (inner' face'))
                              (faces face'))
                      :let [pos' (dirs d)
                            turn (turn3d pos pos')
                            dirs' (into {}
                                    (for [[d2 d3] dirs]
                                      [d2 (mulv d3 turn)]))]]

                  [face' {:pos pos' :dirs dirs'}]))]
          (recur border' inner'))))))

(defn- warp [{:keys [mapp w cube rcube] :as world} p d]
  (let [face (mapv #(quot % w) p)
        {:keys [pos dirs]} (have! (cube face))
        pos2 (dirs d)
        face2 (:face (rcube pos2))
        {dirs2 :dirs} (cube face2)
        pc (mapv - p (face-center w face))
        pcflip (update pc (if (zero? (d 0)) 1 0) -)
        turn (turn3d pos pos2)
        dir2 (mulv pos2 turn)
        d2 (some #(when (= (val %) dir2) (key %)) dirs2)

        [pcflip' d']
        (loop [p pcflip
               d d]
          (if (= d2 d)
            [p d]
            (recur (mulv p (TURNS :L)) (mulv d (TURNS :L)))))
        p' (mapv long (map + pcflip' (face-center w face2)))]
    [p' d']))

(defn- move2 [{:keys [mapp d p] :as world} move]
  (if-let [turn (TURNS move)]
    (update world :d mulv turn)
    (let [c (->> d (keep-indexed #(when-not (zero? %2) %1)) first)]
      (loop [p p
             d d
             cnt move]
        (if (zero? cnt)
          (assoc world :p p :d d)
          (let [p1 (mapv + p d)
                ch (mapp p1)
                [p1 d1] (if ch [p1 d] (warp world p d))
                ch (have! (mapp p1))]
            (case ch
              \# (assoc world :p p :d d)
              \. (recur p1 d1 (dec cnt)))))))))

(defn two [s]
  (let [{:keys [mapp moves] :as world} (parse s)
        w (long (Math/sqrt (/ (count mapp) 6)))
        faces (find-faces world w)
        f0 (first faces)
        faces (set faces)
        cube (fold3d faces f0)
        rcube (into {} (for [[face {:keys [pos dirs]}] cube]
                         [pos {:face face :dirs dirs}]))
        world (assoc world :w w :faces faces :cube cube :rcube rcube)
        {:keys [p d] :as world'} (reduce move2 world moves)]
    (+ (FACINGS d) (* 1000 (inc (p 1))) (* 4 (inc (p 0))))))

#_(two (slurp "d22.in"))

(deftest t-2
  (is (= (two t1) 5031)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
