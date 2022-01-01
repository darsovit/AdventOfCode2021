(ns Day12)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day12/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))

(def testInput1 '("start-A"
                  "start-b"
                  "A-c"
                  "A-b"
                  "b-d"
                  "A-end"
                  "b-end"))
(def test1 {:input testInput1 :problem1 10 :problem2 36})

(defn updateGraphWithEdge
  "Update graph with the given directed edge"
  [graph p1 p2]
  (cond (contains? (:edges graph) p1) (update-in graph (list :edges p1) concat (list p2))
        :else                         (update-in graph (list :edges p1) (constantly (list p2)))))

(defn updateGraphWithLargeNode
  "Determines if the node is a large node and adds to a special :largenode set"
  [graph node]
  (cond (= (clojure.string/upper-case node) node) (update graph :largenodes conj node)
        :else                                     graph))

(updateGraphWithLargeNode {:largenodes #{}} "A")
(defn updateGraphWithNodes
  "Update graph with nodes"
  [graph nodes]
  (reduce (fn [inner node]
            (update (updateGraphWithLargeNode inner node) :nodes conj node)) graph nodes))

(comment  ; These represent quick tests of functionality
  (updateGraphWithLargeNode {:nodes #{} :largenodes #{}} "A")
  (updateGraphWithLargeNode *1 "start")
  (updateGraphWithLargeNode *1 "KN")
  (updateGraphWithNodes {:nodes #{} :largenodes #{}} '("A" "start" "KN")))

(defn updateGraphWithStart
  "Update the given state object representing a graph with a transition from start"
  [state pt1 pt2]
  (let [updateStateWithTransitionFromStart (fn [state other] (updateGraphWithEdge (updateGraphWithNodes state (list "start" other)) "start" other))]
    (cond (= "start" pt1) (updateStateWithTransitionFromStart state pt2)
          :else           (updateStateWithTransitionFromStart state pt1))))

(defn updateStateWithEnd
  "Update the given state object representing a graph with a transition to the end"
  [state pt1 pt2]
  (let [updateGraphWithTransitionToEnd (fn [graph other] (updateGraphWithEdge (updateGraphWithNodes graph (list other "end")) other "end"))]
    (cond (= "end" pt1)  (updateGraphWithTransitionToEnd state pt2)
          :else          (updateGraphWithTransitionToEnd state pt1))))

(defn updateStateWithConnection
  "Update the given state object representing a graph with a two way transition"
  [state pt1 pt2]
  (updateGraphWithEdge (updateGraphWithEdge (updateGraphWithNodes state (list pt1 pt2)) pt1 pt2) pt2 pt1))

(comment
  (updateStateWithConnection {:edges {} :nodes #{} :largenodes #{}} "A" "b"))

(defn buildInputState
  "Interpret the input lines turning into a graph"
  [lines]
  (reduce (fn [state line]
            (let [[pt1 pt2] (clojure.string/split line #"-")]
              (cond (contains? (set (list pt1 pt2)) "start") (updateGraphWithStart      state pt1 pt2)
                    (contains? (set (list pt1 pt2)) "end")   (updateStateWithEnd        state pt1 pt2)
                    :else                                    (updateStateWithConnection state pt1 pt2)))) {:nodes #{} :edges {} :largenodes #{}} lines))

(defn validNextNodes
  "Determine which of the next nodes are valid based on details in the graph and path taken so far"
  [graph pathProgress]
  (let [nodes (get-in graph (list :edges (last (:path pathProgress))))]
    (for [node nodes
          :let [isLargeNode (contains? (:largenodes graph) node)
                isRepeatedSmall (and (= false isLargeNode) (= node (some #{node} (:path pathProgress))))
                repeatedSmallAllowed (:canRevisitSmall pathProgress)]
          :when (or isLargeNode
                    (= false isRepeatedSmall)
                    repeatedSmallAllowed)]
      (list node isRepeatedSmall))))

(let [qt (buildInputState (:input test1))]
  (validNextNodes qt {:path ["start" "A" "c" "A"] :canRevisitSmall true}))

(defn subtraverseGraph
  "Given a graph and path, traverse the next steps possible"
  [graph pathProgress]
  (cond (= (last (:path pathProgress)) "end") (list pathProgress)
        :else
        (let [nextnodes (validNextNodes graph pathProgress)]
          (loop [[firstnext & rightnodes] nextnodes
                 results ()]
            (cond (= nil firstnext) results
                  :else (recur rightnodes (concat results (subtraverseGraph graph (update (cond (= false (last firstnext)) pathProgress
                                                                                                :else (update pathProgress :canRevisitSmall (constantly false))) :path conj (first firstnext))))))))))

(defn traverseGraph
  "Given a graph representing a network and a value for canRevisitSmall, calculate all of the traversals from start to end following the rules that only large nodes can be visited more than once"
  [graph canRevisitSmall]
  (let [paths (subtraverseGraph graph {:path '["start"] :canRevisitSmall canRevisitSmall})]
    {:num (count paths) :paths paths}))

(traverseGraph (buildInputState (:input test1)) false)
(defn validateTest
  "Given a test set (input and expected count of paths) ensure that traversing the graph results in the expected count"
  [testInput]
  (= (:problem1 testInput) (:num (traverseGraph (buildInputState (:input testInput)) false))))

(def testInput2 '("dc-end"
                  "HN-start"
                  "start-kj"
                  "dc-start"
                  "dc-HN"
                  "LN-dc"
                  "HN-end"
                  "kj-sa"
                  "kj-HN"
                  "kj-dc"))
(def test2 {:input testInput2 :problem1 19 :problem2 103})
(def testInput3 '("fs-end"
                  "he-DX"
                  "fs-he"
                  "start-DX"
                  "pj-DX"
                  "end-zg"
                  "zg-sl"
                  "zg-pj"
                  "pj-he"
                  "RW-he"
                  "fs-DX"
                  "pj-RW"
                  "zg-RW"
                  "start-pj"
                  "he-WI"
                  "zg-he"
                  "pj-fs"
                  "start-RW"))
(def test3 {:input testInput3 :problem1 226 :problem2 3509})

(validateTest test1)
(validateTest test2)
(validateTest test3)

(:num (traverseGraph (buildInputState puzzleInput) false))

(defn validateTest2
  "Given a test set, ensure that traversing the graph results in the expected count"
  [testInput]
  (= (:problem2 testInput) (:num (traverseGraph (buildInputState (:input testInput)) true))))

(validateTest2 test1)
(validateTest2 test2)
(validateTest2 test3)

(:num (traverseGraph (buildInputState puzzleInput) true))