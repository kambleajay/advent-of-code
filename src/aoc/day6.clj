(ns aoc.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-group-answers [answers]
  (let [answers-by-person (str/split answers #"\n")]
    (map seq answers-by-person)))

(defn parse-answers [file-name]
  (let [text (slurp (io/resource file-name))
        answers-by-group (str/split text #"\n{2}")]
    (map parse-group-answers answers-by-group)))

(defn yes-count-anyone [answers]
  (->> answers
       (reduce concat [])
       set
       count))

(defn yes-count-everyone [answers]
  (->> answers
       (map set)
       (apply set/intersection)
       count))

(defn answer [file-name count-fn]
  (let [answers (parse-answers file-name)]
    (->> answers
         (map count-fn)
         (apply +))))
