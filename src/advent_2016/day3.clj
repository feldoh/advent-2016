(def input (clojure.string/split-lines (slurp (clojure.java.io/resource "day3.input"))))

(defn read-input-line [line] (filter (comp not empty?) (clojure.string/split line #" ")))

(defn to-int-seq [str-seq] (map #(Integer/valueOf %) str-seq))

(def triangles (map (comp to-int-seq read-input-line) input))

(defn valid-triangle? [[a b c]] (and (> (+ a b) c) (> (+ a c) b) (> (+ b c) a)))

(defn count-valid [triangles] (count (filter true? (map valid-triangle? triangles))))

(def part1 (count-valid triangles))
part1

(defn transpose [vectors] (apply map vector vectors))

(def transposed-triangles (loop [[tri-a tri-b tri-c & remaining-triangles] triangles
       transposed-triangles []]
  (let [newly-transposed (transpose [tri-a tri-b tri-c])
        merged-transposed (into transposed-triangles newly-transposed)]
    (if (empty? remaining-triangles)
      merged-transposed
      (recur remaining-triangles merged-transposed)))))

(def part2 (count-valid transposed-triangles))
part2
