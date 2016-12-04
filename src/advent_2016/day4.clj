(ns advent-day-three
  (:use clojure.test))

(def input (clojure.string/split-lines
             (slurp (clojure.java.io/resource "day4.input"))))

(with-test
  (defn compare-letters [that this]
    (let [count1 (:count this)
          count2 (:count that)
          key1 (:key this)
          key2 (:key that)]
      (if (= count1 count2)
        (compare key2 key1)
        (if (> count1 count2)
          1
          -1))))
  (is (= -1 (compare-letters {:key \a :count 5} {:key \b :count 3})))
  (is (= -1 (compare-letters {:key \b :count 5} {:key \a :count 3})))
  (is (= 1 (compare-letters {:key \b :count 3} {:key \a :count 5})))
  (is (= -1 (compare-letters {:key \a :count 5} {:key \b :count 5})))
  (is (= 1 (compare-letters {:key \b :count 5} {:key \a :count 5}))))

(with-test
  (defn real-room? [entry]
    (let [parts (clojure.string/split entry #"-")
          sector-and-checksum-str (last parts)
          [sector checksum] (clojure.string/split sector-and-checksum-str #"(\[|\])")
          sorted-name (sort(apply str (pop parts)))
          grouped-letters (group-by identity sorted-name)
          counted-letters (map (fn [[k v]] {:key k :count (count v)}) grouped-letters)
          sorted-grouped-letters (sort compare-letters counted-letters)
          checksum-len (count checksum)
          calculated-checksum (apply str (map :key (take 5 sorted-grouped-letters)))]
      (if (= calculated-checksum checksum)
        (parseInt sector)
        nil)))
  (is (= 987 (real-room? "a-b-c-d-e-f-g-h-987[abcde]")))
  (is (= 123 (real-room? "aaaaa-bbb-z-y-x-123[abxyz]")))
  (is (= 404 (real-room? "not-a-real-room-404[oarel]")))
  (is (= nil (real-room? "a-b-c-d-e-f-g-h-987[hbcde]")))
  (is (= nil (real-room? "totally-real-room-200[decoy]"))))

(defn parseInt "Wrapper for Java Integer.parseInt"
  [arg] (Integer/valueOf arg))

(str "Part 1: " (apply + (map parseInt (remove nil? (map real-room? input)))))

(with-test
  (defn rotate [chr rot]
    (let [infinite-alpha (cycle (map char (range (int \a) (int \z))))]
      (first (drop rot (drop-while #(not (= chr %)) infinite-alpha)))))
  (is (= \a (rotate \z 1)))
  (is (= \b (rotate \a 1)))
  (is (= \d (rotate \a 3)))
  (is (= \c (rotate \z 3)))
  (is (= \a (rotate \a 26))))

  (run-tests)
