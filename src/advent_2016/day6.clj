(ns advent-2016.day6
  (:use clojure.test))

(def sample-input ["eedadn"
                   "drvtee"
                   "eandsr"
                   "raavrd"
                   "atevrs"
                   "tsrnev"
                   "sdttsa"
                   "rasrtv"
                   "nssdts"
                   "ntnada"
                   "svetve"
                   "tesnvt"
                   "vntsnd"
                   "vrdear"
                   "dvrsen"
                   "enarar"])

(def input (clojure.string/split-lines
             (slurp (clojure.java.io/resource "day6.input"))))

(with-test
  (defn transpose [vectors] (apply map vector vectors))
  (is (= (transpose ["abc" "def" "ghi"]) [[\a \d \g] [\b \e \h] [\c \f \i]]))
  (is (= (transpose ["abcx" "defy" "ghiz"]) [[\a \d \g] [\b \e \h] [\c \f \i] [\x \y \z]])))

(with-test
  (defn sorted-column-frequencies [input]
    (map (partial sort-by second)
         (map frequencies
              (transpose input))))
  (is (= [[[\a 1] [\b 2]] [[\c 1] [\a 2]]] (sorted-column-frequencies ["ac" "ba" "ba"]))))

(with-test
  (defn most-frequent-by-column [input]
    (map (comp first last)
         (sorted-column-frequencies input)))
  (is (= [\e \a \s \t \e \r] (most-frequent-by-column sample-input))))

(with-test
  (defn least-frequent-by-column [input]
    (map (comp first first)
         (sorted-column-frequencies input)))
  (is (= [\a \d \v \e \n \t] (least-frequent-by-column sample-input))))

(def part1 (apply str (most-frequent-by-column input)))
part1
(println "Part 1 : " part1)

(def part2 (apply str (least-frequent-by-column input)))
part2
(println "Part 2 : " part2)

(run-tests)
