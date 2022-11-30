(ns util
  (:require [clojure.test :refer :all]))

(defn string->vector [s] (read-string (str \[ s \])))

(deftest everything
  (testing "string->vector"
    (is (= (string->vector "10,20,30,-1") [10 20 30 -1]))))
