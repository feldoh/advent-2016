(def input (clojure.string/split-lines (slurp (clojure.java.io/resource "day2.input"))))

(def start-num 5)
(def line-len 3)
(def col-len 3)

(def directions {\U :up
                 \D :down
                 \L :left
                 \R :right})

(def transforms {:up #(if (> % line-len)
                       (- % line-len) %)
                 :down #(if (<= % (* line-len (- col-len 1)))
                       (+ % line-len) %)
                 :left #(let [preceding-num (- % 1)
                              start-of-line? (= (mod preceding-num line-len) 0)]
                          (if (and (> preceding-num 0) (not start-of-line?))
                            (- % 1) %))
                 :right #(if (not (= (mod % line-len) 0))
                       (+ % 1) %)})

(defn to-directions [line] (map #(get directions %) line))
(defn to-functions [directions] (map #(% transforms) directions))
(defn to-composite [command-str] (apply comp (reverse (to-functions (to-directions command-str)))))
(defn get-code [start-num input-lines] ((apply juxt (map to-composite input-lines)) start-num))

(def part1 (get-code 5 input))
part1

(def keypad {:1 {:down :3}
             :2 {:right :3 :down :6}
             :3 {:up :1 :right :4 :down :7 :left :2}
             :4 {:down :8 :left :3}
             :5 {:right :6}
             :6 {:up :2 :right :7 :down :A :left :5}
             :7 {:up :3 :right :8 :down :B :left :6}
             :8 {:up :4 :right :9 :down :C :left :7}
             :9 {:left :8}
             :A {:up :6 :right :B}
             :B {:up :7 :right :C :down :D :left :A}
             :C {:up :8 :left :B}
             :D {:up :B}})

(defn get-keypad-number [start-pos direction-str] (reduce (fn [current-pos instruction] (get-in keypad [current-pos instruction] current-pos)) start-pos (to-directions direction-str)))
(defn get-keypad-code [start-pos input-lines] (reduce (fn [code current-line]
          (let [current-pos (if (empty? code) start-pos (last code))]
            (println current-pos)
            (conj code (get-keypad-number current-pos current-line))))
        [] input-lines))


(get-keypad-code :5 ["ULL" "RRDDD" "LURDL" "UUUUD"])
(def part2 (get-keypad-code :5 input))
(apply str (map name part2))
