(ns Day09)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day09/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("2199943210"
                "3987894921"
                "9856789892"
                "8767896789"
                "9899965678"))

(defn buildValuesArrays
  "Build up the value arrays from the list of strings"
  [inputs]
  (reduce (fn [state line]
            (conj state (vec (map Day01/parse-int (clojure.string/split line #""))))) [] inputs))
(get-in (buildValuesArrays testInput) '(5 0))

(defn getPositionValue
  "Will get a position value and return it; if nil will return 10"
  [values position offset] (let [newpos (map + position offset)
                                 value (get-in values newpos)]
                      (cond (= nil value) 10
                            :else value)))


(defn isLowPosition
  "Determines if the given position is the lowest of neighbors"
  [values position]
  (let [curValue (get-in values position)]
    (and (< curValue (getPositionValue values position '(1 0)))
         (< curValue (getPositionValue values position '(-1 0)))
         (< curValue (getPositionValue values position '(0 1)))
         (< curValue (getPositionValue values position '(0 -1))))))

(defn findLowPositions
  "Given vectors of vectors of ints representing height maps, find low positions"
  [values]
    (for [x (range (count values))
          y (range (count (values 0)))
          :when (isLowPosition values (list x y))]
      [x y]))

(defn getRiskLevels
  "Given a list of low points calculate their risk levels"
  [values lows]
  (map (fn [pos] (+ (get-in values pos) 1)) lows))

(defn calculateSumOfRiskLevelsOfLows
  "Given an input array, calculate the sum of the risks of the low points"
  [inputs]
  (let [values (buildValuesArrays inputs)]
    (reduce + (getRiskLevels values
                             (findLowPositions values)))
))

(calculateSumOfRiskLevelsOfLows testInput)
(calculateSumOfRiskLevelsOfLows puzzleInput)
