(ns Day03)
(require '[clojure.string])

(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day03/input.txt")
(def testval "101011110101")
(def readvals (Day01/lazy-file-lines filename))

(defn buildVal [c]
  (- c (int '\0)))

(defn buildSequenceOfInts [s]
  (->>
   (seq s)
   (map int)
   (map buildVal)))

(defn addTwoSequences [a b]
  (map + a b))

(buildSequenceOfInts testval)

(defn evaluateDiagnostic [state s]
  (cond
    (= state nil) {:vals s :count 1}
    :else {:vals (map + (:vals state) s) :count (inc (:count state))}))
(defn buildGammaDigitFunc [count]
  (fn [i] (if (< (/ count 2) i) 1 0)))

(defn buildGamma [state]
  (map (buildGammaDigitFunc (:count state)) (:vals state)))

(defn buildEpsilon [state]
  (->>
   (buildGamma state)
   (map (fn [val] (if (= 1 val) 0 1)))))


(defn buildFromBinary [val]
  (reduce (fn [cur next] (+ (* cur 2) next)) 0 val))

(defn buildGammaAndEpsilon [state]
  {:gamma (buildFromBinary (buildGamma state)) :epsilon (buildFromBinary (buildEpsilon state))})

(defn calcValue [state]
  (* (:gamma state) (:epsilon state )))
(->>
 (Day01/lazy-file-lines filename)
 (map buildSequenceOfInts)
 (reduce evaluateDiagnostic nil)
 (buildGammaAndEpsilon)
 (calcValue))

(defn buildMsgSet [ourList]
(reduce (fn [state first & rest]
          (cond
            (= 0 first) {:msg0 (conj (:msg0 state) rest) :msg1 (:msg1 state)}
            :else       {:msg0 (:msg0 state) :msg1 (conj (:msg0 state) rest)}))
        [:msg0 () :msg1 ()] ourList))
(buildMsgSet (Day01/lazy-file-lines filename))

(def quickTest (buildSequenceOfInts testval))
((fn [state first & rest]
  (cond
    (= 0 first) {:msg0 (cons (:msg0 state) rest) :msg1 (:msg1 state)}
    :else       {:msg0 (:msg0 state) :msg1 rest})) {:msg0 () :msg1 ()} quickTest) 

(defn evaluateMessages
  "build list of messages starting with 0 or 1 up to state"
  [state [onemsg & rest]]
    (if (= state nil)
      (if (= 0 onemsg)
        {:msg0 (list rest) :msg1 ()}
        {:msg0 () :msg1 (list rest)})
      (if (= 0 onemsg)
        {:msg0 (conj (:msg0 state) rest) :msg1 (:msg1 state)}
        {:msg0 (:msg0 state) :msg1 (conj (:msg1 state) rest)})
    )
)
(def quickTest2 '(1 1 1))
(evaluateMessages nil quickTest)
(evaluateMessages *1 quickTest2)
(def quickTest3 '(1))
(evaluateMessages *2 quickTest3)
(def emptyList (conj () nil))
(empty? emptyList)
(defn allValuesNil [values]
  (= (set values) (set (list nil))))
(allValuesNil emptyList)
(defn buildMsgAndSetValues [gtOrLt oldmsg newmsg]
  (cond
    (gtOrLt (count (:msg0 newmsg)) (count (:msg1 newmsg))) {:msg (conj (:msg oldmsg) 0) :values (:msg0 newmsg)}
    (gtOrLt (count (:msg1 newmsg)) (count (:msg0 newmsg))) {:msg (conj (:msg oldmsg) 1) :values (:msg1 newmsg)}
    (gtOrLt 0 1)                                           {:msg (conj (:msg oldmsg) 0) :values (:msg0 newmsg)}
    :else                                                  {:msg (conj (:msg oldmsg) 1) :values (:msg1 newmsg)}
    )
  )

(buildMsgAndSetValues < {:msg [1] :values (list (list 1 1))} {:msg1 (list nil) :msg0 ()})

(defn evaluateDiagnosticLifeSupport
  "Function used to help build life support values"
  [gtOrLt listOfBinaryDigitLists]
  (loop [iter {:msg [] :values listOfBinaryDigitLists}]
    (if (allValuesNil (:values iter))
      (:msg iter)
      (recur
        (let [newstate (reduce evaluateMessages nil (:values iter))]
          (buildMsgAndSetValues gtOrLt iter newstate)
          )
       )
      )
    )
  )
(def inputs (map buildSequenceOfInts (Day01/lazy-file-lines filename)))
(def oxygenRating (buildFromBinary (evaluateDiagnosticLifeSupport > inputs)))
#_(def co2ScrubberRating (buildFromBinary (evaluateDiagnosticLifeSupport < inputs)))
#_(* oxygenRating co2ScrubberRating)
#_(do
  (let [inputs (map buildSequenceOfInts (Day01/lazy-file-lines filename))]
    (let [oxygenRating (buildFromBinary (evaluateDiagnosticLifeSupport > inputs))]
      (let [co2ScrubberRating (buildFromBinary (evaluateDiagnosticLifeSupport < inputs))]
      (* oxygenRating co2ScrubberRating)))))
#_ (Day01/lazy-file-lines filename)
#_  (map buildSequenceOfInts)

#_(evaluateDiagnosticLifeSupport >)
#_(reduce evaluateMessages nil)
#_(buildMsgAndSetValues < {:msg [] :values ()})
