(ns Day14)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day14/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("NNCB"
                 ""
                 "CH -> B"
                 "HH -> N"
                 "CB -> H"
                 "NH -> C"
                 "HB -> C"
                 "HC -> B"
                 "HN -> C"
                 "NN -> C"
                 "BH -> H"
                 "NC -> B"
                 "NB -> B"
                 "BN -> B"
                 "BB -> N"
                 "BC -> B"
                 "CC -> N"
                 "CN -> C"))

(defn buildPuzzleState
  "Interpret the puzzle inputs into a usable state structure"
  [lines]
  (reduce (fn [state line]
            (let [templatematch (re-matches #"[A-Z]+" line)
                  insertmatch   (re-matches #"([A-Z][A-Z]) -> ([A-Z])" line)]
              (cond (not= nil templatematch) (update state :template (constantly (clojure.string/split templatematch #"")))
                    (not= nil insertmatch)   (let [[full pair toInsert] insertmatch
                                                   key (clojure.string/split pair #"")] (update-in state [:insertionRules key] (constantly toInsert)))
                    :else                    state))) {:template [] :insertionRules {}} lines))

(re-matches #"[A-Z]+" "NNCB")

(buildPuzzleState testInput)
(defn stepState
  [rules template]
  (reduce (fn [newtemplate key] (conj newtemplate (get rules key) (second key))) [(first template)] (for [x (range (dec (count template)))]
             [(get template x) (get template (inc x))])))

(defn constructPolymer
  "Constructs a polymer with the given input over the number of steps desired"
  [input steps]
  (let [puzzleState  (buildPuzzleState input)
        initTemplate (:template puzzleState)
        rules        (:insertionRules puzzleState)]
    (loop [x 0 template initTemplate]
      (cond (= x steps) template
            :else (recur (inc x) (stepState rules template))))))

(defn numLargestElementMinusNumSmallestElement
  "Given a sequence, will calculate the difference between the number of instances of the most frequent element and the number of instances of the least frequent element"
  [sequence]
  (let [instanceCounters (Day05/countInstances sequence)
        maxElement (apply max-key second instanceCounters)
        minElement (apply min-key second instanceCounters)] (- (second maxElement) (second minElement)))
  )
(numLargestElementMinusNumSmallestElement (constructPolymer puzzleInput 10))