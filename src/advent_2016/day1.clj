(def input "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3")
(def instructions (clojure.string/split input #", "))
(def directions [:north :east :south :west])

(defn parse-instruction [instruction cur-direction-index]
  (let [[rot & steps-chars] instruction
        steps (Integer/parseInt (apply str steps-chars))
        rot-function (case rot
                       \L -
                       \R +)
        new-direction-index (rot-function cur-direction-index 1)
        normalised-direction (mod new-direction-index 4)]
    {:direction-index normalised-direction :steps steps}))

(defn translate-instructions [instructions] (reduce (fn [offset-acc instruction]
                                                      (let [last-direction (:direction-index (last offset-acc))
                                                            parsed-instruction (parse-instruction instruction last-direction)]
                                                        (conj offset-acc parsed-instruction)))
                                                    [{:direction-index 0 :steps 0}]
                                                    instructions))

(defn net-offsets [instructions] (reduce (fn [offset-acc instruction] (let [{:keys [direction-index steps]} instruction
                                           direction (get directions direction-index)]
                                       (update-in offset-acc [direction] + steps)))
        {:north 0 :east 0 :south 0 :west 0}
        (translate-instructions instructions)))

(defn minify-net-offsets [offsets] (let [{:keys [north east south west]} offsets]
  (into {} [(if (>= north south)
              {:north (- north south)}
              {:south (- south north)})
            (if (>= east west)
              {:east (- east west)}
              {:west (- west east)})])))

(defn distance [{:keys [north east south west]}]
  (+ (Math/abs (- north south))
     (Math/abs (- east west))))

(defn add-all-directions [position] (reduce (fn [acc direction] (update-in acc [direction] #(if (nil? %1) 0 %1))) position directions))

(def part1 (net-offsets instructions))
(distance part1)

(defn make-intermediaries [complete-last-pos direction steps]
  (map minify-net-offsets (map #(update-in complete-last-pos [direction] + %1) (range 1 (+ steps 1)))))

(defn find-first
         [f coll]
         (first (filter f coll)))

(def part2 (loop [visited #{}
       last-visited {:north 0 :east 0}
       last-direction :north
       [{:keys [direction-index steps]} & remaining-instructions] (translate-instructions instructions)]
  (let [complete-last-pos (add-all-directions last-visited)
        direction (get directions direction-index)
        tidy-new-locs (make-intermediaries complete-last-pos direction steps)
        first-revisited (find-first #(contains? visited %) tidy-new-locs)]
    (if first-revisited
      first-revisited
      (recur (into visited tidy-new-locs) (last tidy-new-locs) direction remaining-instructions)))))

part2
(distance (add-all-directions part2))