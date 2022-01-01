(ns Day07)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day07/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("16,1,2,0,4,2,7,1,2,14"))
(def testValues (Day06/getValues (first testInput)))
(def test1 {:input testInput :problem1 {:loc 2 :fuelCost 37}})


(defn findMode
  "From a sorted map with arity counts, determine the mode value"
  [arities]
  (loop [[firstNode & others] arities
         modeNode nil]
   (cond (= nil firstNode) modeNode
         (= nil modeNode) (recur others firstNode)
         (> (last firstNode) (last modeNode)) (recur others firstNode)
         :else (recur others modeNode))))
  
(defn findWeight
  "Find the weight of the specified side of the mode -arity"
  [ltOrGt modeValue arities]
  (reduce (fn [weight aNode] (if (ltOrGt (first aNode) modeValue) (inc weight) weight)) 0 arities))

(defn shiftLeftMost
  "Take the first element of the arity and shift it in with the 2nd left-most, returning the new arity and fuel cost"
  [arities]
  (let [[firstNode secondNode & restNodes] arities
        [firstNodeValue firstNodeCount] firstNode
        [secondNodeValue secondNodeCount] secondNode
        costPerEntry (- secondNodeValue firstNodeValue)
        fuelCost     (* costPerEntry firstNodeCount)
        newSecondNode [secondNodeValue (+ firstNodeCount secondNodeCount)]]
        [(vec (concat [newSecondNode] restNodes)) fuelCost]
    )
  )

(defn shiftRightMost
  "Take the last node of the arity and shift it to the second last, returning the new arity and fuel cost"
  [arities]
  (let [lastNode (peek arities)
        nextLastNode (peek (pop arities))
        restNodes (pop (pop arities))
        [lastNodeValue lastNodeCount] lastNode
        [nextLastNodeValue nextLastNodeCount] nextLastNode
        costPerEntry (- lastNodeValue nextLastNodeValue)
        fuelCost     (* costPerEntry lastNodeCount)
        newLastNode [nextLastNodeValue (+ lastNodeCount nextLastNodeCount)]]
    [(conj restNodes newLastNode) fuelCost])
  )
(vec (into (sorted-map) (Day05/countInstances testValues)))
(let [[newArity addedFuelCost] (shiftRightMost (vec (into (sorted-map) (Day05/countInstances testValues))))]
  newArity)

(defn calculateCheapestAlignmentCost
  "Given an input set, identify the cheapest alignment cost and the location"
  [inputs]
  (let [values (Day06/getValues (first inputs))]
    (loop [arity (vec (into (sorted-map) (Day05/countInstances values)))
           fuelCost 0]
      (cond
        (= 1 (count arity)) {:loc (first (first arity)) :fuelCost fuelCost}
        :else (let [modeNode (findMode arity)
                    modeValue (first modeNode)
                    leftWeight (findWeight < modeValue arity)
                    rightWeight (findWeight > modeValue arity)
                    shifter     (if (> leftWeight rightWeight) shiftLeftMost shiftRightMost)
                    [newArity addedFuelCost] (shifter arity)]
                (recur newArity (+ addedFuelCost fuelCost)))))))

(= (:problem1 test1) (calculateCheapestAlignmentCost (:input test1)))
(calculateCheapestAlignmentCost puzzleInput)