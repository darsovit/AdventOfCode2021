(ns Day07)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day07/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("16,1,2,0,4,2,7,1,2,14"))
(def testValues (Day06/getValues (first testInput)))
(def test1 {:input testInput :problem1 {:loc 2 :fuelCost 37} :problem2 {:loc 5 :fuelCost 168}})


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
    [(vec (concat [newSecondNode] restNodes)) fuelCost]))

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
    [(conj restNodes newLastNode) fuelCost]))

(defn calculateCheapestLinearAlignmentCost
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
(calculateCheapestLinearAlignmentCost puzzleInput)


(let [[leftNode & rhsNodes] (vec (into (sorted-map) (Day05/countInstances testValues)))]
     rhsNodes)

(defn buildInitialIncreasingCostState
  "Given an input set, build the state object for calculating increasing cost"
  [inputs]
  (let [values (Day06/getValues (first inputs))
        [leftNode & rhsNodes] (into (sorted-map) (Day05/countInstances values))
        rhsNodesVec (vec rhsNodes)
        rightNode (peek rhsNodesVec)
        remainderShips (pop rhsNodesVec)]
    {:left {:pos (first leftNode)   :ships (second leftNode)  :movecost (second leftNode)}
                    :right {:pos (first rightNode) :ships (second rightNode) :movecost (second rightNode)}
                    :fuelcost 0 :additionalShips remainderShips}
  ))

(comment
(buildInitialIncreasingCostState (:input test1))
)


(defn updateSide
  "Given an increasing cost state, side definition, and the values to replace update the given side"
  [state side newPos newShips newMoveCost]
  (update state side (constantly {:pos newPos :ships newShips :movecost newMoveCost})))

(defn shiftLeftSide
  "Provided an increasing cost state, shift the left side in one position"
  [state]
  (let [leftNewPos (inc (get-in state [:left :pos]))
        numLeftShips (get-in state [:left :ships])
        [leftNode & rhsNodes] (get state :additionalShips)
        fuelSpent (get-in state [:left :movecost])
        nextFuelCost (+ numLeftShips fuelSpent)]
    (update
     (cond (= nil leftNode)                (updateSide state :left leftNewPos numLeftShips nextFuelCost)
           (= leftNewPos (first leftNode)) (updateSide (update state :additionalShips (constantly (vec rhsNodes)))
                                                       :left leftNewPos (+ numLeftShips (last leftNode)) (+ nextFuelCost (last leftNode)))
           :else                           (updateSide state :left leftNewPos numLeftShips                     (+ fuelSpent numLeftShips))
          )
     :fuelcost
     + fuelSpent)))

(defn shiftRightSide
  "Provided an increasing cost state, shift the right side in one position"
  [state]
  (let [rightNewPos (dec (get-in state [:right :pos]))
        numRightShips (get-in state [:right :ships])
        fuelSpent (get-in state [:right :movecost])
        rightNode (peek (get state :additionalShips))
        nextFuelCost (+ numRightShips fuelSpent)]
    (update
     (cond (= nil rightNode) (updateSide state :right rightNewPos numRightShips nextFuelCost)
           (= rightNewPos (first rightNode)) (updateSide (update state :additionalShips pop)
                                                         :right rightNewPos (+ numRightShips (last rightNode)) (+ nextFuelCost (last rightNode)))
           :else             (updateSide state :right rightNewPos numRightShips nextFuelCost))
     :fuelcost
     + fuelSpent)))

(defn calculateCheapestIncreasingCost
  "Given an input set, identify the cheapest alignment cost when the cost of moving increases"
  [inputs]
  (loop [state (buildInitialIncreasingCostState inputs)]
    (cond (= (get-in state [:left :pos])      (get-in state [:right :pos]))      {:loc (get-in state [:left :pos]) :fuelCost (get state :fuelcost)}
          (< (get-in state [:left :movecost]) (get-in state [:right :movecost])) (recur (shiftLeftSide state))
          :else                                                                  (recur (shiftRightSide state))
)))

(= (:problem2 test1) (calculateCheapestIncreasingCost (:input test1)))
(calculateCheapestIncreasingCost puzzleInput)
