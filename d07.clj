(ns d07
  (:require
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [clojure.core.match :refer [match]]
    [clojure.test :refer :all]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn- parse-line [l]
  (match (str/split l #"\s+")
    ["$" "cd" dst] [:cmd :cd dst]
    ["$" "ls"] [:cmd :ls]
    ["dir" dir] [:dir dir]
    [size file] [:file (parse-long size) file]))

(defn- eat1 [[cwd fs] cmd]
  (match cmd
    [:cmd :cd dst]
    [(condp = dst
       ".." (cond-> cwd (seq cwd) pop)
       "/" []
       (conj cwd dst))
     fs]

    [:cmd :ls] [cwd fs]

    [:dir dir] [cwd (update-in fs (conj cwd dir) #(or % {}))]

    [:file size file] [cwd (assoc-in fs (conj cwd file) size)]))

(defn- dir-sizes [fs path]
  (loop [sum 0
         sub []
         [head & tail] (get-in fs path)]
    (if-not head
      (into [[path sum]] sub)
      (let [[nm cont] head]
        (if (number? cont)
          (recur (+ sum ^long cont) sub tail)
          (let [sub1 (dir-sizes fs (conj path nm))
                size1 (get-in sub1 [0 1])]
            (recur (+ sum ^long size1) (into sub sub1) tail)))))))

(defn one [s]
  (let [[cwd fs]
        (->> s str/split-lines (mapv parse-line) (reduce eat1 [[] {}]))]
    (->> (dir-sizes fs [])
      (keep #(when (<= (second %) 100000) (second %)))
      (apply +))))

(deftest t-1
  (is (= (one t1) 95437)))

(defn two [s]
  (let [[cwd fs]
        (->> s str/split-lines (mapv parse-line) (reduce eat1 [[] {}]))

        sizes (mapv second (dir-sizes fs []))
        total (first sizes)
        free (- 70000000 total)
        need (- 30000000 free)]
    (->> sizes (remove #(< % need)) (apply min))))

(deftest t-2
  (is (= (two t1) 24933642)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
