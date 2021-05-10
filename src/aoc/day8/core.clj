(ns aoc.day8.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(def instructions ["nop +0"
                   "acc +1"
                   "jmp +4"
                   "acc +3"
                   "jmp -3"
                   "acc -99"
                   "acc +1"
                   "jmp -4"
                   "acc +6"])

(defn instruction-starts-with [txt]
  (fn [instruction]
    (s/starts-with? instruction txt)))

(def acc? (instruction-starts-with "acc"))

(def jmp? (instruction-starts-with "jmp"))

(def nop? (instruction-starts-with "nop"))

(defn parse-argument [instruction] (-> (s/split instruction #"\s") second Integer/parseInt))

(defn new-accumulator [acc instruction]
  (if (acc? instruction)
    (+ acc (parse-argument instruction))
    acc))

(defn next-instruction-index [idx instruction]
  (cond
    (acc? instruction) (inc idx)
    (nop? instruction) (inc idx)
    (jmp? instruction) (+ idx (parse-argument instruction))))

(defn accumulator-value [instructions]
  (loop [accumulator 0
         idx 0
         executed-instruction-idxs []]
    (let [next-instruction (nth instructions idx)]
      (if (some #{idx} executed-instruction-idxs)
        accumulator
        (recur
          (new-accumulator accumulator next-instruction)
          (next-instruction-index idx next-instruction)
          (conj executed-instruction-idxs idx))))))

(defn read-instructions []
  (with-open [rdr (io/reader (io/resource "day8/input.txt"))]
    (doall (line-seq rdr))))

(defn answer-1 []
  (accumulator-value (read-instructions)))

(defn nop+jmps [instructions]
  (filter (fn [[idx itm]] (or (jmp? itm) (nop? itm))) (map-indexed vector instructions)))

(defn swap-instruction [instruction]
  (cond
    (nop? instruction) (s/replace instruction #"nop" "jmp")
    (jmp? instruction) (s/replace instruction #"jmp" "nop")))

(defn with-nop-and-jmp-changed [instructions]
  (let [nop+jmps (nop+jmps instructions)]
    (map (fn [[idx ins]] (assoc instructions idx (swap-instruction ins))) nop+jmps)))

(defn accumulator+terminate-status [instructions]
  (loop [accumulator 0
         idx 0
         executed-instruction-idxs []]
    (cond
      (some #{idx} executed-instruction-idxs) {:accumulator accumulator :termiates? false}
      (>= idx (count instructions)) {:accumulator accumulator :termiates? true}
      :else (recur
              (new-accumulator accumulator (nth instructions idx))
              (next-instruction-index idx (nth instructions idx))
              (conj executed-instruction-idxs idx)))))

(defn answer-2 [instructions]
  (as-> instructions ins
        (into [] ins)
        (with-nop-and-jmp-changed ins)
        (conj ins instructions)
        (map #(accumulator+terminate-status %) ins)
        (filter #(true? (get % :termiates?)) ins)
        (first ins)))
