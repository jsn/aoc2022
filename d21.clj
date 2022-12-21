(ns d21
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
"root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(defn- parse-term [t]
  (if (re-matches #"-?\d+" t)
    (parse-long t)
    (keyword t)))

(defn- parse-line [l]
  (let [[dst expr] (str/split l #":\s+" 2)]
    [(keyword dst) (mapv parse-term (str/split expr #"\s+"))]))

(defn- parse [s]
  (->> s str/split-lines (map parse-line) (into {})))

(defn- eval1 [env expr]
  (cond
    (number? expr) expr
    (keyword? expr) (recur env (env expr))
    (vector? expr)
    (match expr
      [t] (recur env t)
      [a op b]
      (let [opf (have! ({:- - :+ + :* * :/ /} op))]
        (opf (eval1 env a) (eval1 env b))))))

(defn one [s]
  (eval1 (parse s) :root))

(deftest t-1
  (is (= (one t1) 152)))

(defn- s+ [a b]
  (if-not (vector? a) (recur b a)
    (update a 2 + b)))

(defn- s* [a b]
  (if-not (vector? a) (recur b a)
    (-> a (update 1 * b) (update 2 * b))))

(defn- s- [a b]
  (if-not (vector? a)
    (s* (s- b a) -1)
    (update a 2 - b)))

(defn- sdiv [a b]
  (-> (have! vector? a) (update 1 / b) (update 2 / b)))

(defn- s= [a b]
  [:= a b])

(defn- eval2 [env expr]
  (cond
    (number? expr) expr
    (= expr :humn) [:humn 1 0]
    (keyword? expr) (recur env (env expr))
    (vector? expr)
    (match expr
      [t] (recur env t)

      [a op b]
      (let [ae (eval2 env a)
            be (eval2 env b)

            ops
            (if (and (number? ae) (number? be))
              {:- - :+ + :* * :/ /}
              {:- s- :+ s+ :* s* :/ sdiv := s=})

            opf (have! (ops op))]
        (opf ae be)))))

(defn- solve-for-humn [expr]
  (match expr
    [:= [:humn a b] c]
    (/ (- c b) a)))

(defn two [s]
  (-> s
    parse
    (assoc-in [:root 1] :=)
    (eval2 :root)
    solve-for-humn
    long))

(deftest t-2
  (is (= (two t1) 301)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
