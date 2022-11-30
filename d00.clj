(ns d00
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"")

(defn one [s]
  "not implemented")

(deftest t-1
  (is (= (one t1) "not implemented")))

(defn two [s]
  "not implemented")

(deftest t-2
  (is (= (two t1) "not implemented")))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
