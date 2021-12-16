(ns Day05)

(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day05/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("0,9 -> 5,9"
                 "8,0 -> 0,8"
                 "9,4 -> 3,4"
                 "2,2 -> 2,1"
                 "7,0 -> 7,4"
                 "6,4 -> 2,0"
                 "0,9 -> 2,9"
                 "3,4 -> 1,4"
                 "0,0 -> 8,8"
                 "5,5 -> 8,2"))

(defn parseLine
  "Parse an input line into the line segment coordinates"
  [inputLine]
  (let [[full, x1, y1, x2, y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" inputLine)]
    {:from (list (Day01/parse-int x1) (Day01/parse-int y1)) :to (list (Day01/parse-int x2) (Day01/parse-int y2))}
    ))

(def testValues (map parseLine testInput))

(range 5 10)
(defn addVerticalLineToField
  "Add a vertical line to the field"
  [field column y1 y2]
  (let [placeFunc (fn [state y] (conj state (list column y)))]
    (cond
      (< y1 y2) (reduce placeFunc field (range y1 (+ 1 y2)))
      :else     (reduce placeFunc field (range y2 (+ 1 y1)))
      )
    )
  )

(defn addHorizontalLineToField
  "Add a horizontal line to the field"
  [field row x1 x2]
  (let [placeFunc (fn [state x] (conj state (list x row)))]
    (cond
      (< x1 x2) (reduce placeFunc field (range x1 (+ 1 x2)))
      :else     (reduce placeFunc field (range x2 (+ 1 x1)))
    )
    )
  )
(addHorizontalLineToField () 1 1 10)
(first (:from (first testInput)))

(addVerticalLineToField () 1 1 10)
(defn addStraightLineInputToField
  "Adds an input to the field"
  [state input]
  (cond
    (= (first (:from input)) (first (:to input))) (addVerticalLineToField state (first (:from input)) (last (:from input)) (last (:to input)))
    (= (last  (:from input)) (last  (:to input))) (addHorizontalLineToField state (last (:from input)) (first (:from input)) (first (:to input)))
    :else state
    )
  )

(defn buildField
  "Build a field out of input values"
  [inputs]
  (reduce addStraightLineInputToField () inputs)
  )

(range 4 (+ 10 1) 1)
(range 10 (- 4 1) -1)
(defn addDiagonalLineToField
  "Add a diagonal line to the field"
  [field x1 x2 y1 y2]
  (reduce (fn [state pos] (conj state pos)) field
  (apply map list (list (cond (< x1 x2) (range x1 (+ x2 1) 1) :else (range x1 (- x2 1) -1))
                        (cond (< y1 y2) (range y1 (+ y2 1) 1) :else (range y1 (- y2 1) -1))))))

(addDiagonalLineToField () 0 10 10 0)
(defn addAllLineInputToField
  "Adds an input to the field"
  [state input]
  (cond
    (= (first (:from input)) (first (:to input))) (addVerticalLineToField state (first (:from input)) (last (:from input)) (last (:to input)))
    (= (last  (:from input)) (last  (:to input))) (addHorizontalLineToField state (last (:from input)) (first (:from input)) (first (:to input)))
    :else (addDiagonalLineToField state (first (:from input)) (first (:to input)) (last (:from input)) (last (:to input)))
    ))

(defn buildAllField
  "Build a field out of input value"
  [inputs]
  (reduce addAllLineInputToField () inputs))

(buildField testValues)

(defn countInstances
  "Count the number of instances a value appears in set"
  [list]
  (reduce (fn [state value] (update state value (fn [val] (cond (= nil val) 1 :else (inc val))))) {} list))
(countInstances (buildField testValues))

(defn countNumGreaterThanOne
  "Count the number of instances that have a value greater than one"
  [fieldValues]
  (reduce (fn [curCount [_ value]] (cond (> value 1) (inc curCount) :else curCount )) 0 fieldValues))
(countNumGreaterThanOne (countInstances (buildField testValues)))
(countNumGreaterThanOne (countInstances (buildField (map parseLine puzzleInput))))
(countNumGreaterThanOne (countInstances (buildAllField testValues)))
(countNumGreaterThanOne (countInstances (buildAllField (map parseLine puzzleInput))))