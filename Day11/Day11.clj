(ns Day11)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day11/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("5483143223"
                 "2745854711"
                 "5264556173"
                 "6141336146"
                 "6357385478"
                 "4167524645"
                 "2176841721"
                 "6882881134"
                 "4846848554"
                 "5283751526"))

(def testInput2 '("11111"
                  "19991"
                  "19191"
                  "19991"
                  "11111"))

(def testInput2ResultOneDay '("34543"
                              "40004"
                              "50005"
                              "40004"
                              "34543"))

(def testInput2ResultTwoDays '("45654"
                               "51115"
                               "61116"
                               "51115"
                               "45654"))


(defn buildPowerArrays
  "given list of strings for input, build vector of vector of integers"
  [lines]
  (reduce (fn [outputs line] (conj outputs
                                   (reduce (fn [lineContents letter]
                                             (conj lineContents (Day01/parse-int letter)))
                                           [] (clojure.string/split line #"")))) [] lines))

(defn determinePowerForDay
  "given a vector of vector of power levels, add power for the day"
  [powerValues]
  (mapv #(mapv inc %) powerValues))

(def testInput2PowerAfterOneDay (determinePowerForDay (buildPowerArrays testInput2)))

(defn determineSetOfFlashers
  "given a vector of vector of power levels, determine the set of flashing entries"
  [powerValues]
  (for [x (range (count powerValues))
        y (range (count (get-in powerValues '(0))))
        :when (> (get-in powerValues (list x y)) 9)]
    (list x y)))


(defn calculateFlasherNeighbors
  "Given a known flasher, identify its neighbors to increment and potentially make a new flasher"
  [pos]
  (map (fn [val] (map + pos val)) '((1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1))))


(defn applyFlasherState
  "given a calculated daypower, apply a given flasher's state"
  [state flasher]
  (let [oldvalue (get-in state (list :dayPower (first flasher) (last flasher)))]
    (cond
      (= nil oldvalue) state
      (= 0 oldvalue)   state
      (< oldvalue 9)   (update-in state                                    (list :dayPower (first flasher) (last flasher)) inc)
      (= oldvalue 9)   (update-in (update state :newflashers conj flasher) (list :dayPower (first flasher) (last flasher)) (constantly 0))
      (= oldvalue 10)  (update-in (update state :newflashers conj flasher) (list :dayPower (first flasher) (last flasher)) (constantly 0)))))

(defn handleFlashingResult
  "Given the result of determining power for the day build a set of flashing coordinates, increment neighbor values and build final result"
  [dayPower]
  (loop [currentState dayPower
         flashers (determineSetOfFlashers dayPower)
         appliedFlashers #{}]
    (cond
      (= 0 (count flashers)) (list dayPower appliedFlashers)
      :else
      (let [[firstFlasher & restFlashers] flashers
            removedFlasherDayPower (update-in currentState firstFlasher (constantly 0))
            nextState (reduce applyFlasherState {:newflashers #{} :dayPower removedFlasherDayPower} (calculateFlasherNeighbors firstFlasher))
            nextFlashers (clojure.set/union (set restFlashers) (:newflashers nextState))
            nextAppliedFlashers (conj appliedFlashers firstFlasher)]
        (cond (= 0 (count nextFlashers)) (list (:dayPower nextState) nextAppliedFlashers)
              :else (recur (:dayPower nextState) nextFlashers nextAppliedFlashers)))))
  )
testInput2PowerAfterOneDay
(handleFlashingResult testInput2PowerAfterOneDay)

(defn countFlashes
  "Given a start state, will loop for the number of days and count number of flashes"
  [startState numLoops]
  (loop [x 0
         [nextState setOfFlashes] (handleFlashingResult (determinePowerForDay startState))
         countOfFlashes 0
         previousState startState]
    (cond (< x numLoops) (recur (+ x 1)
                                (handleFlashingResult (determinePowerForDay nextState))
                                (+ countOfFlashes (count setOfFlashes))
                                nextState)
          :else (list countOfFlashes previousState) 
          )
    )
  )

(countFlashes (buildPowerArrays puzzleInput) 100)

(defn determineWhenAllFlash
  "Given a start state, will loop until all octopi flash at once"
  [startState]
  (let [totalNumberOfOctopi (* (count startState) (count (first startState)))]
    (loop [x 0
         [nextState setOfFlashes] (handleFlashingResult (determinePowerForDay startState))]
      (cond (= totalNumberOfOctopi (count setOfFlashes)) (list (+ x 1) nextState)
          :else (recur (+ x 1)
                       (handleFlashingResult (determinePowerForDay nextState)))
          ))))

(determineWhenAllFlash (buildPowerArrays puzzleInput))