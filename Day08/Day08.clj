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
               (conj state {:inputs  (reduce conj () (clojure.string/split inputs #" "))
                            :outputs (reduce conj () (clojure.string/split outputs #" "))}
                     ))) () lines)
   )
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
                                  )) ) 0 (:outputs object))
             )) 0 values))
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
; Comparing 6 and 9, 6 has g and 9 has a g is bottom left, a is top right
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
  (update 
    (update state :3
            (constantly
             (reduce (fn [substate value]
                       (cond (not= nil substate) substate
                             (= 3 (count (clojure.set/difference value (:1 state)))) value
                             :else nil)) nil (:fivesegments state))))
     :fivesegments (fn [values] (remove #(= (:3 state) %) values))))
(determineThree *2)
(defn determineSetValuesFromInputs
  "Determine the set values from the input values"
  [objects]
  (->>
   (buildStateToStart (:inputs objects))
   (determineTop)
   (determineThree)))