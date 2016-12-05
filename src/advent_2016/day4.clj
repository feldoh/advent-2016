(ns advent-2016.day4
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

(defn parseInt "Wrapper for Java Integer.parseInt"
  [arg] (Integer/valueOf arg))

(with-test
  (defn chunk-room [room]
    (let [parts (clojure.string/split room #"-")
            sector-and-checksum-str (last parts)
            [sector checksum] (clojure.string/split sector-and-checksum-str #"(\[|\])")]
      {:sector (parseInt sector) :checksum checksum :name-parts (pop parts)}))
  (is (= (chunk-room "ab-c-defg-987[abcde]") {:sector 987 :checksum "abcde" :name-parts ["ab" "c" "defg"]})))

(with-test
  (defn real-room? [entry]
    (let [{:keys [sector checksum name-parts]} (chunk-room entry)
          joined-name (apply str name-parts)
          grouped-letters (group-by identity joined-name)
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
  (is (= nil (real-room? "totally-real-room-200[decoy]")))
  (is (= 343 (real-room? "qzmt-zixmtkozy-ivhz-343[zimth]"))))

(with-test
  (defn rotate [rot chr]
    (let [infinite-alpha (cycle (map char (range (int \a) (+ 1 (int \z)))))]
      (first (drop rot (drop-while #(not (= chr %)) infinite-alpha)))))
  (is (= \a (rotate 1 \z)))
  (is (= \b (rotate 1 \a)))
  (is (= \d (rotate 3 \a)))
  (is (= \c (rotate 3 \z)))
  (is (= \a (rotate 26 \a))))

(with-test
  (defn rotate-word [rot word]
    (apply str (map (partial rotate rot) word)))
  (is (= "yza" (rotate-word 1 "xyz")))
  (is (= "def" (rotate-word 3 "abc"))))

(with-test
  (defn decrypt-name [entry] (let [{:keys [sector name-parts]} (chunk-room entry)
        real? (real-room? entry)]
    (if real?
      {:sector real? :name (clojure.string/join " " (map (partial rotate-word sector) name-parts))}
      nil)))
  (is (= {:sector 343 :name "very encrypted name"} (decrypt-name "qzmt-zixmtkozy-ivhz-343[zimth]")))
  (is (= nil (decrypt-name "qzmt-zixmtkozy-ivhz-343[aaaaa]"))))

(run-tests)

(println (str "Part 1: " (apply + (map parseInt (remove nil? (map real-room? input))))))
(println (str "Part 2: " (first (filter #(re-matches #"north.*" (:name %)) (remove nil? (map decrypt-name input))))))
