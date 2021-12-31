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
(def test1 (list testInput1, 10))

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

(updateStateWithConnection {:edges {} :nodes #{} :largenodes #{}} "A" "b")

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
  [graph path]
  (let [nodes (get-in graph (list :edges (last path)))]
    (for [node nodes
          :when (or (contains? (:largenodes graph) node) (not= node (some #{node} path)))] node)))

(defn subtraverseGraph
  "Given a graph and path, traverse the next steps possible"
  [graph path]
  (cond (= (last path) "end") (list path)
        :else
        (let [nextnodes (validNextNodes graph path)]
          (loop [[firstnext & rightnodes] nextnodes
                 results ()]
            (cond (= nil firstnext) results
                  :else (recur rightnodes (concat results (subtraverseGraph graph (conj path firstnext)))))
            )
          )
        )
  )

(defn traverseGraph
  "Given a graph representing a network, calculate all of the traversals from start to end following the rules that only large nodes can be visited more than once"
  [graph]
  (let [paths (subtraverseGraph graph '["start"])]
    {:paths paths :num (count paths)}
    ))

(defn validateTest
  "Given a test set (input and expected count of paths) ensure that traversing the graph results in the expected count"
  [testInput]
  (= (last testInput) (:num (traverseGraph (buildInputState (first testInput)))))
  )

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
(def test2 (list testInput2, 19))
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
(def test3 (list testInput3, 226))

(validateTest test1)
(validateTest test2)
(validateTest test3)
(:num (traverseGraph (buildInputState puzzleInput)))
