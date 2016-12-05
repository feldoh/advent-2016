(ns advent-2016.day5
  (:use clojure.test))

(def input-door-id "cxdnnyjw")
(def md5-instance (java.security.MessageDigest/getInstance "MD5"))

(with-test
  (defn md5 [input]
    (apply str (map (partial format "%02x")
                    (.digest (doto md5-instance
                               .reset
                               (.update (.getBytes input)))))))
  (is (= "09c225f3c02c75d04627ae2e449891f0" (md5 "abc10"))))

(with-test
  (defn md5-seq [root-str] (map (comp md5 (partial str root-str)) (range)))
  (is (= [(md5 "abc0") (md5 "abc1") (md5 "abc2")] (take 3 (md5-seq "abc")))))

(def part1 (apply str (map #(get % 5) (take 8 (filter #(clojure.string/starts-with? % "00000") (md5-seq input-door-id))))))
part1
(println "Part 1: " part1)

(def part2 (loop [possibles-seq (filter #(clojure.string/starts-with? % "00000") (md5-seq input-door-id))
       remaining-positions #{\0 \1 \2 \3 \4 \5 \6 \7}
       answer {}]
  (let [[_ _ _ _ _ pos chr] (first possibles-seq)]
    (println pos chr)
    (if (contains? remaining-positions pos)
      (if (= 1 (count remaining-positions))
        (assoc answer pos chr)
        (recur (rest possibles-seq)
               (disj remaining-positions pos)
               (assoc answer pos chr)))
      (recur (rest possibles-seq)
               remaining-positions
               answer)))))

part2
(println "Part 2: " (apply str (map #(get part2 %) [\1 \2 \3 \4 \5 \6 \7])))

(run-tests)
