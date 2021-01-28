(ns aoc.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn moves [[right down]]
  (iterate (fn [[x y]] [(+ x down) (+ y right)]) [0 0]))

(defn trees-encountered [territory slope]
  (let [rows-count (count territory)
        column-counts (map count territory)
        valid-square? (fn [[x _]]
                        (< x rows-count))
        adjust-column (fn [[x y]]
                        (if (>= y (nth column-counts x))
                          [x (rem y (nth column-counts x))]
                          [x y]))
        read-square (fn [[x y]]
                      (nth (nth territory x) y))
        tree? (fn [x] (= x \#))]
    (->> (moves slope)
         (take-while valid-square?)
         (map adjust-column)
         (map read-square)
         (filter tree?)
         count)))

(defn read-territory [file-name]
  (let [s (slurp (io/resource file-name))
        lines (str/split s #"\n")]
    (map #(seq %1) lines)))

(defn answer [file-name slopes]
  (let [territory (read-territory file-name)]
    (apply * (map #(trees-encountered territory %) slopes))))
