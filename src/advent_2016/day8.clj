(ns advent-2016.day8
  (:use clojure.test advent-2016.core))

(def input (get-input-lines "day8.input"))

(with-test
  (defn rect
    ([width height] (fn [screen] (loop [remaining-height height
                                        [next-line & rest-screen] screen
                                        new-screen []]
                                   (let [replacement-line (flatten (vector (repeat width :on)
                                                                           (drop width next-line)))
                                         new-screen-with-new-line (conj new-screen replacement-line)]
                                     (if (= remaining-height 1)
                                       (reduce conj new-screen-with-new-line rest-screen)
                                       (recur (- remaining-height 1) rest-screen new-screen-with-new-line))))))
    ([instruction] (let [[width height] (clojure.string/split instruction #"x")]
                     (rect (parseInt width) (parseInt height)))))
  (is (= [[:on :on]
          [:off :on]] ((rect "2x1") [[:off :off]
                                     [:off :on]])))
  (is (= [[:on]] ((rect 1 1) [[:off]])))
  (is (= [[:on :off]] ((rect 1 1) [[:off :off]])))
  (is (= [[:on :off]
          [:off :on]] ((rect 1 1) [[:off :off]
                                   [:off :on]])))
  (is (= [[:on :on]
          [:off :on]] ((rect 2 1) [[:off :off]
                                   [:off :on]])))
  (is (= [[:on :on]
          [:on :on]] ((rect 2 2) [[:off :off]
                                  [:off :off]])))
  (is (= [[:on :on :on :on :on :on :on :on :on :on :on :on :off :on]
          [:on :on :on :on :on :on :on :on :on :on :on :on :off :on]
          [:on :off :off :off :off :off :off :off :off :off :off :off :off :on]]
         ((rect "12x2") [[:off :off :off :off :off :off :off :off :off :off :off :off :off :on]
                         [:off :off :off :off :off :off :off :off :off :off :off :off :off :on]
                         [:on :off :off :off :off :off :off :off :off :off :off :off :off :on]]))))

(with-test
  (defn rot-row [row-num amount]
    (fn [screen] (let [[pre-rows [row & post-rows]] (split-at row-num screen)
                       row-len (count row)
                       cycled-row (reverse (take row-len (drop amount (cycle (reverse row)))))]
                   (reduce conj post-rows (conj pre-rows cycled-row)))))
  (is (= [[:1 :2 :3]] ((rot-row 0 3) [[:1 :2 :3]])))
  (is (= [[:3 :1 :2]] ((rot-row 0 1) [[:1 :2 :3]])))
  (is (= [[:1 :2 :3]
          [:5 :6 :4]] ((rot-row 1 2) [[:1 :2 :3]
                                      [:4 :5 :6]]))))

(with-test
  (defn rot-column [col-num amount]
    (comp transpose (rot-row col-num amount) transpose))
  (is (= [[:1] [:2] [:3]] ((rot-column 0 3) [[:1] [:2] [:3]])))
  (is (= [[:3] [:1] [:2]] ((rot-column 0 1) [[:1] [:2] [:3]])))
  (is (= [[:1 :5 :3]
          [:4 :2 :6]] ((rot-column 1 1) [[:1 :2 :3]
                                         [:4 :5 :6]])))
  (is (= [[:1 :5 :3]
          [:4 :8 :6]
          [:7 :2 :9]] ((rot-column 1 2) [[:1 :2 :3]
                                         [:4 :5 :6]
                                         [:7 :8 :9]]))))
(defn rotate [axis pos _ rotation]
  (let [fun-as-symbol (symbol (str "rot-" axis))
        parsed-position (second (clojure.string/split pos #"="))]
    ((resolve fun-as-symbol) (parseInt parsed-position) (parseInt rotation))))

(with-test
  (defn parse-instruction [instruction]
    (let [[action & args] (clojure.string/split instruction #" ")]
      (apply (resolve (symbol action)) (apply vector args))))
  (is (= [[:on :on :on]] ((parse-instruction "rect 3x1") [[:off :off :off]])))
  (is (= [[:2 :3 :1]] ((parse-instruction "rotate row y=0 by 2") [[:1 :2 :3]])))
  (is (= [[:a :b :c] [:2 :3 :1]] ((parse-instruction "rotate row y=1 by 2") [[:a :b :c] [:1 :2 :3]])))
  (is (= [[:2] [:3] [:1]] ((parse-instruction "rotate column x=0 by 2") [[:1] [:2] [:3]]))))

(with-test
  (defn make-screen [width height] (repeat height (repeat width :off)))
  (is (= [[:off :off :off] [:off :off :off]] (make-screen 3 2))))

(with-test
  (defn apply-instructions [screen instructions]
    (reduce (fn [acc inst] ((parse-instruction inst) acc)) screen instructions))
  (is (= [[:off :on :off :off :on :off :on]
          [:on :off :on :off :off :off :off]
          [:off :on :off :off :off :off :off]] (apply-instructions
                                                 (make-screen 7 3) ["rect 3x2"
                                                                    "rotate column x=1 by 1"
                                                                    "rotate row y=0 by 4"
                                                                    "rotate column x=1 by 1"]))))

(def screen-50-by-6 (make-screen 50 6))
(def part-1-screen (apply-instructions screen-50-by-6 input))
(count (filter #(= :on %) (flatten part-1-screen)))

(run-tests)
