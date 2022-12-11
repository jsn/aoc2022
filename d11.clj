(ns d11
  (:require
    [clojure.string :as str]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn- parse-op [op]
  (match (str/split op #"\s+")
    ["+" "old"] #(+ % %)
    ["*" "old"] #(* % %)
    ["+" n] #(+ % (parse-long n))
    ["*" n] #(* % (parse-long n))
    ["-" n] #(- % (parse-long n))))

(defn- parse-stanza [s]
  (let [[_ monkey items op div then else]
        (have!
          (re-matches
            #"Monkey (\d+):\s*Starting items: (\d+(?:, \d+)*)\s*Operation: new = old (.*?\S)\s*Test: divisible by (\d+)\s*If true: throw to monkey (\d+)\s*If false: throw to monkey (\d+)\s*"
            s))]
    {:id (parse-long monkey)
     :nops 0
     :items (u/string->vector items)
     :op (parse-op op)
     :div (parse-long div)
     :then (parse-long then)
     :else (parse-long else)}))

(defn- parse [s]
  (let [w (mapv parse-stanza (str/split s #"\n\s*\n"))]
    (assert (= (map :id w) (range (count w))))
    w))

(defn- turn [w id]
  (let [{:keys [items op div then else]} (w id)]
    (reduce
      #(let [i (quot (op %2) 3)
             dst (if (zero? (mod i div)) then else)]
         (-> %1
           (update-in [dst :items] conj i)
           (update-in [id :nops] inc)
           (update-in [id :items] subvec 1)))
      w items)))

(defn- round [w]
  (reduce turn w (range (count w))))

(defn one [s]
  (->> s
    parse
    (iterate round)
    (drop 20)
    first
    (map :nops)
    sort
    reverse
    (take 2)
    (apply *)))

(deftest t-1
  (is (= (one t1) 10605)))

(def ^:private ^:dynamic *ring* nil)

(defn- turn2 [w id]
  (let [{:keys [items op div then else]} (w id)]
    (reduce
      #(let [i (mod (op %2) *ring*)
             dst (if (zero? (mod i div)) then else)]
         (-> %1
           (update-in [dst :items] conj i)
           (update-in [id :nops] inc)
           (update-in [id :items] subvec 1)))
      w items)))

(defn- round2 [w]
  (reduce turn2 w (range (count w))))

(defn two [s]
  (let [w (parse s)]
    (binding [*ring* (apply * (map :div w))]
      (->> w
        (iterate round2)
        (drop 10000)
        first
        (map :nops)
        sort
        reverse
        (take 2)
        (apply *)))))

(deftest t-2
  (is (= (two t1) 2713310158)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
