(ns aoc.day2
  (:require [clojure.java.io :as io]))

(defn count-freq [ch acc next-ch]
  (if (= (str next-ch) ch)
    (inc acc)
    acc))

(defn valid-password? [{:keys [min-occurrence max-occurrence ch password]}]
  (let [ch-freq (reduce (partial count-freq ch) 0 password)]
    (and (>= ch-freq min-occurrence) (<= ch-freq max-occurrence))))

(defn parse-policy-and-password [s]
  (let [pattern #"(\d+)-(\d+)\s(.):\s(.+)"
        [_ min-occ max-occ ch password] (re-matches pattern s)]
    {:min-occurrence (Integer/parseInt min-occ)
     :max-occurrence (Integer/parseInt max-occ)
     :ch             ch
     :password       password}))

(defn nth-char [s index]
  (str (nth s (dec index))))

(defn valid-password-alt? [{:keys [index-1 index-2 ch password]}]
  (let [index-1-ch (nth-char password index-1)
        index-2-ch (nth-char password index-2)]
    (or (and (= index-1-ch ch) (not= index-2-ch ch))
        (and (not= index-1-ch ch) (= index-2-ch ch)))))

(defn parse-policy-and-password-alt [s]
  (let [pattern #"(\d+)-(\d+)\s(.):\s(.+)"
        [_ index-1 index-2 ch password] (re-matches pattern s)]
    {:index-1 (Integer/parseInt index-1)
     :index-2 (Integer/parseInt index-2)
     :ch             ch
     :password       password}))

(defn answer [parse-fn valid-fn]
  (with-open [rdr (io/reader (io/resource "day2.txt"))]
    (let [lines (line-seq rdr)]
      (->> lines
           (map parse-fn)
           (filter valid-fn)
           count))))
