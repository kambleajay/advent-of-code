(ns aoc.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def mandatory-keys #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})

(defn str-between? [s x y]
  (let [n (Integer/parseInt s)]
    (and (>= n x)
         (<= n y))))

(defn valid-year? [s x y]
  (str-between? s x y))

(defn has-unit? [hgt]
  (re-matches #"(\d+)(cm|in)" hgt))

(defn within-bounds? [[_ num unit]]
  (condp = unit
    "cm" (str-between? num 150 193)
    "in" (str-between? num 59 76)))

(defn valid-height? [hgt]
  (let [parts (has-unit? hgt)]
    (if parts
      (within-bounds? parts)
      false)))

(defn valid-hair-color? [hcl]
  (re-matches #"#[0-9a-f]{6}" hcl))

(defn valid-eye-color? [ecl]
  (some #{ecl} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn valid-passport-id? [pid]
  (re-matches #"\d{9}" pid))

(defn valid-values? [{:strs [byr iyr eyr hgt hcl ecl pid]}]
  (and (valid-year? byr 1920 2002)
       (valid-year? iyr 2010 2020)
       (valid-year? eyr 2020 2030)
       (valid-height? hgt)
       (valid-hair-color? hcl)
       (valid-eye-color? ecl)
       (valid-passport-id? pid)))

(defn valid-passport? [passport]
  (let [missing-keys (set/difference mandatory-keys (set (keys passport)))]
    (and
      (empty? missing-keys)
      (valid-values? passport))))

(defn fields [passport]
  (str/split passport #"\s"))

(defn kv-pairs [fields]
  (map #(str/split % #":") fields))

(defn passports [passport-text]
  (let [passport-lines (str/split passport-text #"\n{2}")
        passport-fields (map fields passport-lines)
        passport-kvs (map kv-pairs passport-fields)]
    (map #(into {} %) passport-kvs)))

(defn answer [file-name]
  (let [passport-text (slurp (io/resource file-name))]
    (->> (passports passport-text)
         (filter valid-passport?)
         count)))
