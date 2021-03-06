(ns advent-2016.day7
  (:use clojure.test))

(def samples
      ["abba[mnop]qrst[qwer]"; supports TLS (abba outside square brackets).
       "abcd[bddb]xyyx"; does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
       "aaaa[qwer]tyui"; does not support TLS (aaaa is invalid; the interior characters must be different).
       "ioxxoj[asdfgh]zxcvbn"]); supports TLS (oxxo is outside square brackets, even though it's within a larger string)

(def hypernets-pattern #"\[.*?\]")

(def input (clojure.string/split-lines
             (slurp (clojure.java.io/resource "day7.input"))))

(with-test
  (defn extract-hypernets [ipv7] (re-seq hypernets-pattern ipv7))
  (is (= ["[mnop]" "[qwer]"] (extract-hypernets (first samples))))
  (is (= ["[bddb]"] (extract-hypernets (second samples))))
  (is (= ["[qwer]"] (extract-hypernets (nth samples 2))))
  (is (= ["[asdfgh]"] (extract-hypernets (nth samples 3)))))


(with-test
  (defn extract-supernets [ipv7] (clojure.string/split ipv7 hypernets-pattern))
  (is (= ["abba" "qrst"] (extract-supernets (first samples))))
  (is (= ["abcd" "xyyx"] (extract-supernets (second samples))))
  (is (= ["aaaa" "tyui"] (extract-supernets (nth samples 2))))
  (is (= ["ioxxoj" "zxcvbn"] (extract-supernets (nth samples 3)))))

(with-test
  (defn first-abba [chrs]
    (loop [[first-chr & chr-seq] chrs]
      (let [[second-chr third-chr fourth-chr & remaining-chrs] chr-seq]
        (if (and (= first-chr fourth-chr)
                 (= second-chr third-chr)
                 (not (= first-chr second-chr))
                 (not (= third-chr fourth-chr)))
          (str first-chr second-chr third-chr fourth-chr)
          (if (= (count chr-seq) 0)
            nil
            (recur chr-seq))))))
  (is (= "abba" (first-abba "abba")))
  (is (= "abba" (first-abba "[abba")))
  (is (= "abba" (first-abba "abba]")))
  (is (= "abba" (first-abba "[abba]")))
  (is (= nil (first-abba "[aaaa]")))
  (is (= nil (first-abba "[adbba]"))))

(with-test
  (defn any-abba [words-vec] (some #(not (nil? %)) (map first-abba words-vec)))
  (is (= true (any-abba ["abba"])))
  (is (= true (any-abba ["abba" "lalalala"])))
  (is (= nil (any-abba ["abcba" "lalalala"])))
  (is (= nil (any-abba ["ab"]))))

(with-test
  (defn valid-ipv7? [ipv7] (let [hypernets (extract-hypernets ipv7)
                                  supernets (extract-supernets ipv7)
                                  hypernets-have-abba (any-abba hypernets)
                                  supernets-have-abba (any-abba supernets)]
                              (if (and supernets-have-abba (not hypernets-have-abba))
                                ipv7
                                nil)))
  (is (= (first samples) (valid-ipv7? (first samples))))
  (is (= nil (valid-ipv7? (second samples))))
  (is (= nil (valid-ipv7? (nth samples 2))))
  (is (= (nth samples 3) (valid-ipv7? (nth samples 3)))))

(def part1 (count (remove nil? (map valid-ipv7? input))))
part1
(println "Part 1: " part1)

(def ssl-samples ["aba[bab]xyz" ;supports SSL (aba outside square brackets with corresponding bab within square brackets).
                  "xyx[xyx]xyx" ;does not support SSL (xyx, but no corresponding yxy).
                  "aaa[kek]eke" ;supports SSL (eke in supernet with corresponding kek in hypernet,the aaa sequence is not related, because the interior character must be different).
                  "aaa[ack]eke[kek]" ; supports SSL via the eke/kek set
                  "aaa[cackle]aca[kek]" ; supports SSL via the cac/aca set
                  "zazbz[bzb]cdb"]) ;supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).

(with-test
  (defn extract-abas [chrs]
    (loop [[first-chr & chr-seq] chrs
           abas #{}]
      (let [[second-chr third-chr & remaining-chrs] chr-seq
            possible-aba (str first-chr second-chr third-chr)
            new-abas (if (and (= first-chr third-chr)
                              (not (= first-chr second-chr))
                              (not (= second-chr third-chr)))
                       (conj abas possible-aba)
                       abas)]
        (if (= (count chr-seq) 0)
          new-abas
          (recur chr-seq new-abas)))))
  (is (= #{} (extract-abas "abba")))
  (is (= #{} (extract-abas "bbb")))
  (is (= #{} (extract-abas "abb")))
  (is (= #{"aba"} (extract-abas "aba")))
  (is (= #{"aba"} (extract-abas "aba]")))
  (is (= #{"aba"} (extract-abas "[aba")))
  (is (= #{"aba"} (extract-abas "[aba]")))
  (is (= #{"aba" "bab"} (extract-abas "[ababa]")))
  (is (= #{"aba" "bab"} (extract-abas "abab"))))

(with-test
  (defn ipv7-with-ssl? [ipv7] (let [hypernets (extract-hypernets ipv7)
                                    supernets (extract-supernets ipv7)
                                    supernet-abas (reduce #(into %1 %2) #{} (map extract-abas supernets))
                                    supernet-possible-babs (map aba-to-bab supernet-abas)
                                    hypernet-babs (reduce #(into %1 %2) #{} (map extract-abas hypernets))]
                                (some hypernet-babs supernet-possible-babs)))
  (is (= "bab" (ipv7-with-ssl? (first ssl-samples))))
  (is (= nil (ipv7-with-ssl? (second ssl-samples))))
  (is (= "kek" (ipv7-with-ssl? (nth ssl-samples 2))))
  (is (= "kek" (ipv7-with-ssl? (nth ssl-samples 3))))
  (is (= "cac" (ipv7-with-ssl? (nth ssl-samples 4))))
  (is (= "bzb" (ipv7-with-ssl? (nth ssl-samples 5)))))

(with-test
  (defn aba-to-bab [[c1 c2 c3]] (str c2 c1 c2))
  (is (= "aba" (aba-to-bab "bab"))))

(run-tests)

(def part2 (remove nil? (map ipv7-with-ssl? input)))
part2
(println "Part 2: " (count part2))

