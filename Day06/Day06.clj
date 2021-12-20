(ns Day06)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day06/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput "3,4,3,1,2")

(defn getValues
  "Get input values as list"
  [input]
  (map Day01/parse-int (clojure.string/split input #",")))

(def testValues (getValues testInput))

(defn incrementValuesOneDay
  "increment the lanternfish by one day of processing"
  [values]
  (let [nextVal (reduce (fn [next val]
                          (cond (= val 0) {:newVals (conj (:newVals next) 8) :value (conj (:value next) 6)}
                                :else     {:newVals (:newVals next)          :value (conj (:value next) (dec val))})) {:newVals [] :value []} values)]
    (concat (:value nextVal) (:newVals nextVal))))
(defn incrementValuesSevenDays
  "increment the lanternfish by 7 days of processing"
  [values]
  (let [nextVal (reduce (fn [next val]
                          (cond (= val 8) {:newVals (:newVals next) :value (conj (:value next) 1)}
                                (= val 7) {:newVals (:newVals next) :value (conj (:value next) 0)}
                                :else     {:newVals (conj (:newVals next) (- 8 (- 6 val)))
                                           :value (conj (:value next) val)})) {:newVals [] :value []} values)]
    (concat (:value nextVal) (sort (:newVals nextVal)))))

(defn increment80Days
  "increment the pattern of lanternfish 80 days"
  [values]
  (->>
   (incrementValuesOneDay values)
   (incrementValuesOneDay)
   (incrementValuesOneDay)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)))

(defn increment256Days
  "increment the pattern of lanternfish 256 days"
  [values]
  (->>
   (incrementValuesOneDay values)
   (incrementValuesOneDay)
   (incrementValuesSevenDays)
   (incrementValuesSevenDays)
   (increment80Days)
   (increment80Days)
   (increment80Days)))

(count (increment80Days testValues))
(count (increment80Days (getValues (first puzzleInput))))
(count (increment256Days testValues))
(count (increment256Days '(1)))

(defn resultAfter8Days
  "increment a pattern for 8 days"
  [value]
  (incrementValuesSevenDays (incrementValuesOneDay (list value))))

(resultAfter8Days 0)
(resultAfter8Days 1)
(resultAfter8Days 2)
(resultAfter8Days 3)
(resultAfter8Days 4)
(resultAfter8Days 5)
(resultAfter8Days 6)
(resultAfter8Days 7)
(resultAfter8Days 8)


(defn buildAnswerDetails
  "builds a map of value to number of instances from the value list presented"
  [values] (Day05/countInstances values))


(defn incrementLifecyclesOneDay
  "Increments a set of lifecycles by one day"
  [lifecycleCounts]
  (reduce (fn [newCounts oneLifeCycle]
            (let [[lifecycle newval] oneLifeCycle]
              (cond (= 0 lifecycle) (update (update newCounts 8 (fn [_] newval)) 6 (fn [oldval] (cond (= oldval nil) newval :else (+ oldval newval))))

                    (= 7 lifecycle) (update newCounts 6 (fn [oldval] (cond (= oldval nil) newval :else (+ oldval newval))))
                    :else           (update newCounts (- lifecycle 1) (fn [_] newval)))))
          {} lifecycleCounts))

(let [testValue '{3 2 4 1 1 1 2 1}]
  (incrementLifecyclesOneDay testValue))
(incrementLifecyclesOneDay '{3 2 4 1 1 1 2 1})
(incrementLifecyclesOneDay *1)

(defn CalculateStateOfLanternfishForNumDays
  "Given an input value and a number of days to calculate for, produces the state of lanternfish"
  [inputValues countOfDays]
  (loop [x 0 state (buildAnswerDetails (getValues inputValues))]
    (cond (< x countOfDays) (recur (+ x 1) (incrementLifecyclesOneDay state))
          :else state)))

(defn CalculateCountOfLanternfishFromState
  "Given a state of lanternfish lifecylces with counts, produce the total count of lanternfish"
  [state]
  (reduce (fn [total oneLifeCycleAndCount] (+ total (last oneLifeCycleAndCount))) 0 state))

(CalculateCountOfLanternfishFromState (CalculateStateOfLanternfishForNumDays testInput 80))
(CalculateCountOfLanternfishFromState (CalculateStateOfLanternfishForNumDays testInput 256))

(CalculateCountOfLanternfishFromState (CalculateStateOfLanternfishForNumDays (first puzzleInput) 80))
(CalculateCountOfLanternfishFromState (CalculateStateOfLanternfishForNumDays (first puzzleInput) 256))
