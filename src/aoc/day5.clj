(ns aoc.day5
  (:require [clojure.java.io :as io]))

(defn split-row-col-strs [seat-str]
  (map #(apply str %) (split-at 7 seat-str)))

(defn pick-half [lower-half-char upper-half-char]
  (fn [xs char]
    (let [half (/ (count xs) 2)]
      (condp = char
        lower-half-char (take half xs)
        upper-half-char (drop half xs)))))

(def find-row-iter (pick-half \F \B))

(defn find-row [row-str]
  (reduce find-row-iter (range 128) row-str))

(def find-col-iter (pick-half \L \R))

(defn find-col [col-str]
  (reduce find-col-iter (range 8) col-str))

(defn find-seat-id [seat-str]
  (let [[row-str col-str] (split-row-col-strs seat-str)
        row (first (find-row row-str))
        col (first (find-col col-str))]
    (+ (* row 8) col)))

(defn answer1 []
  (with-open [rdr (io/reader (io/resource "day5.txt"))]
    (let [lines (line-seq rdr)
          max-seat-id (->> lines
                           (map find-seat-id)
                           (apply max))]
      (println max-seat-id))))

(defn answer2 []
  (with-open [rdr (io/reader (io/resource "day5.txt"))]
    (let [lines (line-seq rdr)
          missing-seat (->> lines
                            (map find-seat-id)
                            sort
                            (partition 2 1)
                            (drop-while (fn [[x y]] (= y (inc x)))))]
      (println (first missing-seat) (inc (ffirst missing-seat))))))
