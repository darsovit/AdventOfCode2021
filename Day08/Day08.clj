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
