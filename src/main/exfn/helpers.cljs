(ns exfn.helpers
  (:require [clojure.string :as str]))

(def valid-instructions #{"mov" "add" "sub" "div" "mul" "call" "ret" "rep" "rza"
                          "prn" "end" "and" "or" "xor" "jmp" "jgz" "push" "pop" "nop"
                          "inc" "dec" "jne" "jge" "jg" "je" "jle" "jl" "rnz" "cer" "rp"
                          "rz" "rlez" "rgz" "rgez" "rlz" "not" "cat" "len" "jz" "cmp"
                          "rem" "inp"})

(defn get-source-line-numbers [source]
  (:line-nos (reduce (fn [{:keys [cur line-nos] :as acc} i]
                       (if (or (= "" i) (clojure.string/starts-with? i ";"))
                         (assoc acc :line-nos (str line-nos "\n"))
                         (-> acc
                             (assoc :line-nos (str line-nos cur "\n"))
                             (update :cur inc))))
                     {:cur 0 :line-nos ""}
                     (->> (str/split source #"\r?\n" -1)
                          (map str/trim)))))

(defn keyed-collection [col]
  (zipmap (iterate inc 0) col))

(defn- deep-merge-with
  "
  Copied here from clojure.contrib.map-utils. The original may have
  been a casualty of the clojure.contrib cataclysm.
  Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}
  "
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn levenshtein-distance
  "Copied from incanter
   https://github.com/incanter/incanter/
  http://en.wikipedia.org/wiki/Levenshtein_distance
  internal representation is a table d with m+1 rows and n+1 columns
  where m is the length of a and m is the length of b.
  In information theory and computer science, the Levenshtein distance
  is a metric for measuring the amount of difference between two sequences
  (i.e., the so called edit distance).
  The Levenshtein distance between two strings is given by the minimum number
  of operations needed to transform one string into the other,
  where an operation is an insertion, deletion, or substitution of a single character.
  For example, the Levenshtein distance between \"kitten\" and \"sitting\" is 3,
  since the following three edits change one into the other,
  and there is no way to do it with fewer than three edits:
   1. kitten → sitten (substitution of 's' for 'k')
   2. sitten → sittin (substitution of 'i' for 'e')
   3. sittin → sitting (insert 'g' at the end).
  The Levenshtein distance has several simple upper and lower bounds that are useful
  in applications which compute many of them and compare them. These include:
    * It is always at least the difference of the sizes of the two strings.
    * It is at most the length of the longer string.
    * It is zero if and only if the strings are identical.
    * If the strings are the same size, the Hamming distance is an upper bound on the Levenshtein distance.
  "
  [a b]
  (let [m (count a)
        n (count b)
        init (apply deep-merge-with (fn [a b] b)
                    (concat
                       ;;deletion
                     (for [i (range 0 (inc m))]
                       {i {0 i}})
                       ;;insertion
                     (for [j (range 0 (inc n))]
                       {0 {j j}})))
        table (reduce
               (fn [d [i j]]
                 (deep-merge-with
                  (fn [a b] b)
                  d
                  {i {j (if (= (nth a (dec i))
                               (nth b (dec j)))
                          ((d (dec i)) (dec j))
                          (min
                           (+ ((d (dec i))
                               j) 1) ;;deletion
                           (+ ((d i)
                               (dec j)) 1) ;;insertion
                           (+ ((d (dec i))
                               (dec j)) 1))) ;;substitution
                      }}))
               init
               (for [j (range 1 (inc n))
                     i (range 1 (inc m))] [i j]))]

    ((table m) n)))

(defn get-suggestions-for-invalid-instruction [invalid-instruction replacements]
  (let [all-distances (->> (map (fn [instruction] [instruction (levenshtein-distance instruction invalid-instruction)]) replacements)
                           (sort-by second <))
        
        closest-distance (second (first all-distances))]
    (->> all-distances
         (take-while #(= closest-distance (second %)))
         (sort-by first)
         (map first))))