(ns aoc.day7.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn bag-color [bag-txt]
  (second (first (re-seq #"([a-zA-Z\s]+)\sbags?" bag-txt))))

(defn parse-contained-bags [bags-txt]
  (if (re-find #"no other bags" bags-txt)
    []
    (map (fn [[_ cnt color]] (vector color (Integer/parseInt cnt))) (re-seq #"(\d+)\s([a-zA-Z\s]+)\sbags?" bags-txt))))

(defn parse-rule [bag->contained-bags line]
  (let [[bag-txt contained-bags-txt] (s/split line #"contain")]
    (assoc bag->contained-bags (bag-color bag-txt) (parse-contained-bags contained-bags-txt))))

(defn read-rules []
  (let []
    (with-open [rdr (io/reader (io/resource "day7/rules.txt"))]
      (doall (reduce parse-rule {} (line-seq rdr))))))
