(ns Day08)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day08/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                 "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                 "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                 "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                 "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                 "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                 "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                 "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                 "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                 "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(defn readValues
  "Read the inputs and build values that can be used"
  [lines]
  (reduce (fn [state line]
            (let [[inputs outputs] (clojure.string/split line #" \| ")]
              (conj state {:inputs  (reduce conj [] (clojure.string/split inputs #" "))
                           :outputs (reduce conj [] (clojure.string/split outputs #" "))}))) [] lines))
(def testValues (readValues testInput))

(defn countNumOfOutputs1478
  "Counts the number of outputs that would represent 1, 4, 7, 8"
  [values]
  (reduce (fn [outersum object]
            (+ outersum
               (reduce (fn [innersum outputPart]
                         (+ innersum
                            (cond (= 2 (count outputPart)) 1 ; Represents a 1
                                  (= 3 (count outputPart)) 1 ; Represents a 7
                                  (= 4 (count outputPart)) 1 ; Represents a 4
                                  (= 7 (count outputPart)) 1 ; Represents a 8
                                  :else                      0 ; Anything non-unique
                                  )))0 (:outputs object)))) 0 values))
(countNumOfOutputs1478 (readValues testInput))
(countNumOfOutputs1478 (readValues puzzleInput))

; Determine the values for the given input sequences
; acedgfb (8 - all)
; cdfbe   ((2,3,5))
; gcdfa   ((2,3,5))
; fbcad   ((2,3,5))
; dab     (7, d is top, based on 1 being made of {a,b})
; cefabd  ((0,6,9))
; cdfgeb  ((0,6,9))
; eafb    (4, (e,f)=(left top, middle))
; cagedb  ((0,6,9))
; ab      (1, (a,b)=(right top, right bottom))
; of the 5-segmments (2,3,5) 3 will have (1=a,b) in its set, so (fbcad) is 3
; 4 had (e,f) as possible middle, f is in 3, so f is middle
; Of 6 segments: (cagedb doesn't have middle letter (f)) so this is 0
; 9 has (ab,f) of the 6 segments left, so (cefabd) is 9
; 6 is only 6 segment left (cdfgeb)
; Comparing 6 and 9, 6 has 'g' and 9 has 'a': g is bottom left, a is top right
; Of the remaining 5 segments (cdfbe,gcdfa), 2 will have g and a; 2 is gcdfa
; cdfbe is 5

(defn buildStateToStart
  "Based on the input values, setup the initial state"
  [values]
  (reduce (fn
            [state inputValue]
            (cond (= 2 (count inputValue)) (update state :1 (constantly (set inputValue)))
                  (= 3 (count inputValue)) (update state :7 (constantly (set inputValue)))
                  (= 4 (count inputValue)) (update state :4 (constantly (set inputValue)))
                  (= 7 (count inputValue)) (update state :8 (constantly (set inputValue)))
                  (= 5 (count inputValue)) (update state :fivesegments conj (set inputValue))
                  (= 6 (count inputValue)) (update state :sixsegments conj  (set inputValue))))
          {:fivesegments () :sixsegments ()} values))

(determineTop (buildStateToStart (:inputs (first testValues))))

(defn determineTop
  "Based on the state, determine and set the top"
  [state]
  (update state :top (constantly (clojure.set/difference (:7 state) (:1 state)))))

(defn determineThree
  "Of the unknown five-segments, determine which represents the value 3"
  [state]
  (let [newstate
   (update state :3
           (constantly
            (reduce (fn [substate value]
                      (cond (not= nil substate) substate
                            (= 3 (count (clojure.set/difference value (:1 state)))) value
                            :else nil)) nil (:fivesegments state))))]
    (update newstate :fivesegments (fn [values] (remove #(= (:3 newstate) %) values)))))

(defn determineMiddle
  "Take the set values for 4, 1, and 3 and determine the middle segment value"
  [state]
  (update state :middle (constantly (clojure.set/intersection (clojure.set/difference (:4 state) (:1 state)) (:3 state)))))

(defn determineZero
  "Of the uknown six-segments determine which represents the 0 value by not having the middle"
  [state]
  (let [newstate
        (update state :0
                (constantly
                 (reduce (fn [substate value]
                           (cond (not= nil substate) substate
                                 (= 0 (count (clojure.set/intersection value (:middle state)))) value
                                 :else nil)) nil (:sixsegments state))))]
    (update newstate :sixsegments (fn [values] (remove #(= (:0 newstate) %) values)))
    ))

(defn determineNineAndSix
  "Of the unknown six-segments left (6,9) 9 will have all of (1) in it"
  [state]
  (reduce (fn [substate value]
                           (cond (= 2 (count (clojure.set/intersection value (:1 state)))) (update substate :9 (constantly value))
                                 :else                                                     (update substate :6 (constantly value))))
    state (:sixsegments state)))

(defn determineTwoAndFive
  "Of the unknown five-segments left (5,2), 5 will have all of its segments in 9 or 6"
  [state]
  (reduce (fn [substate value]
            (cond (= 5 (count (clojure.set/intersection value (:9 state)))) (update substate :5 (constantly value))
                  :else                                                     (update substate :2 (constantly value))))
    state (:fivesegments state)))

  
(defn setupSetToValueMap
  "Set up the set to value mapping"
  [state]
  {(:0 state) 0
   (:1 state) 1
   (:2 state) 2
   (:3 state) 3
   (:4 state) 4
   (:5 state) 5
   (:6 state) 6
   (:7 state) 7
   (:8 state) 8
   (:9 state) 9}
  )  

(defn determineSetValuesFromInputs
  "Determine the set values from the input values"
  [objects]
  (->>
   (buildStateToStart (:inputs objects))
   (determineTop)
   (determineThree)
   (determineMiddle)
   (determineZero)
   (determineNineAndSix)
   (determineTwoAndFive)
   (setupSetToValueMap)))

(defn calculateOutputValue
  "Given an input value map and output segments, calculate output value"
  [inputMap object]
  (reduce (fn [substate digit] (+ (get inputMap (set digit)) (* 10 substate))) 0 (:outputs object)))

(determineSetValuesFromInputs (first testValues))
(:outputs (first testValues))
(calculateOutputValue (determineSetValuesFromInputs (first testValues)) (first testValues))
(first testValues)

(defn sumAllInputValues
  "given some input data, determine number inputs and calculate outputs, multiplying together"
  [inputs]
  (reduce (fn [value oneBank]
            (+ value (calculateOutputValue (determineSetValuesFromInputs oneBank) oneBank))) 0 inputs))

(sumAllInputValues testValues)
(sumAllInputValues (readValues puzzleInput))