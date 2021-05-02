(ns aoc.day7.core)

(def bag->contained-bags {:light-red    [[:bright-white 1] [:muted-yellow 2]]
                          :dark-orange  [[:bright-white 3] [:muted-yellow 4]]
                          :bright-white [[:shiny-gold 1]]
                          :muted-yellow [[:shiny-gold 2] [:faded-blue 9]]
                          :shiny-gold   [[:dark-olive 1] [:vibrant-plum 2]]
                          :dark-olive   [[:faded-blue 3] [:dotted-black 4]]
                          :vibrant-plum [[:faded-blue 5] [:dotted-black 6]]
                          :faded-blue   []
                          :dotted-black []})

;shiny gold bags contain 2 dark red bags.
;dark red bags contain 2 dark orange bags.
;dark orange bags contain 2 dark yellow bags.
;dark yellow bags contain 2 dark green bags.
;dark green bags contain 2 dark blue bags.
;dark blue bags contain 2 dark violet bags.
;dark violet bags contain no other bags.
(def bag->contained-bags2 {:shiny-gold [[:dark-red 2]]
                           :dark-red [[:dark-orange 2]]
                           :dark-orange [[:dark-yellow 2]]
                           :dark-yellow [[:dark-green 2]]
                           :dark-green [[:dark-blue 2]]
                           :dark-blue [[:dark-violet 2]]
                           :dark-violet []})

(defn can-contain? [colors color-to-contain]
  (some #(= % color-to-contain) colors))

(defn bags-that-can-contain-iter [color rules]
  (reduce-kv
    (fn [acc k v]
      (if (can-contain? v color)
        (concat (conj acc k) (bags-that-can-contain-iter k rules))
        acc))
    []
    rules))

(defn bags-that-can-contain [color rules]
  (-> (bags-that-can-contain-iter color rules)
      set
      count))

(defn number-of-bags-contained
  ([color bag->contained-bags]
   (dec (number-of-bags-contained color 1 bag->contained-bags)))
  ([color n bag->contained-bags]
   (let [contained-bags (get bag->contained-bags color)]
     (if (empty? contained-bags)
       n
       (apply + n (map (fn [[_color _count]] (* n (number-of-bags-contained _color _count bag->contained-bags))) contained-bags))))))