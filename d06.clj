(ns d06
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn- run [s l]
  (some
    #(let [e (+ % l)]
       (when (-> s (subs % e) set count (= l)) e))
    (range (- (count s) l))))

(defn one [s]
  (run s 4))

(deftest t-1
  (is (= (one t1) 7)))

(defn two [s]
  (run s 14))

(deftest t-2
  (is (= (two t1) 19)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
