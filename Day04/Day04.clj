(ns Day04)
(require '[clojure.string])

(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day04/input.txt")
(def testInput '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
                 ""
                 "22 13 17 11  0"
                 " 8  2 23  4 24"
                 "21  9 14 16  7"
                 " 6 10  3 18  5"
                 " 1 12 20 15 19"
                 ""
                 " 3 15  0  2 22"
                 " 9 18 13 17  5"
                 "19  8  7 25 23"
                 "20 11 10 24  4"
                 "14 21 16 12  6"
                 ""
                 "14 21 17 24  4"
                 "10 16 15  9 19"
                 "18  8 23 26 20"
                 "22 11 13  6  5"
                 " 2  0 12  3  7"
                 ""))

(defn splitAndInterpretInputLine
  "Used to split the line of inputs into a list of integers"
  [inputLine]
  (map Day01/parse-int (clojure.string/split inputLine #",")))

(defn readLineForBoard
  "Used to split a single line of board into numbers"
  [inputLine]
  (keep Day04/parse-int (clojure.string/split inputLine #"\W+")))

(defn parse-int
  [aStr]
  (if (empty? aStr)
    nil
    (Day01/parse-int aStr)))

(defn buildBoard
  "Takes input board and builds the board as arrays of sets of numbers representing horizontal and vertical lines"
  [board]
  {:horizontal (map set board) :vertical (map set (apply map list board))})

(buildBoard '[(14 21 17 24 4) (10 16 15 9 19) (18 8 23 26 20) (22 11 13 6 5) (2 0 12 3 7)])

(defn buildInputData
  "Used to build up a map containing the input sequence of numbers and boards by adding the line to the state one at a time"
  [state line]
  (cond
    (= nil state) {:inputs (splitAndInterpretInputLine line) :boards () :currentBoard []}
    (and (empty? line) (empty? (:currentBoard state))) state
    (empty? line) {:inputs (:inputs state) :boards (conj (:boards state) (buildBoard (:currentBoard state))) :currentBoard []}
    :else {:inputs (:inputs state) :boards (:boards state) :currentBoard (conj (:currentBoard state) (readLineForBoard line))}))

(defn readInput
  "Read the input data and interpret it into the sequence of numbers and boards"
  [lines]
  (let [state (reduce buildInputData nil lines)]
    (if (empty? (:currentBoard state))
      {:inputs (:inputs state) :boards (:boards state)}
      {:inputs (:inputs state) :boards (conj (:boards state) (buildBoard (:currentBoard state)))})))

(def testValues (readInput testInput))
(def puzzleValues (readInput (Day01/lazy-file-lines filename)))
(defn playNumberAgainstBoard
  "Play out a number against a board"
  [number board]
  {:horizontal (reduce (fn [state line] (conj state (disj line number))) () (:horizontal board))
   :vertical   (reduce (fn [state line] (conj state (disj line number))) () (:vertical board))})

(defn calcBoardWin
  "Calculates the value of a board that has won"
  [board number]
  (* number (reduce (fn [total boardLine] (+ total (reduce + boardLine))) 0 (:horizontal board))))

(apply map list (first (:boards testValues)))
(defn boardHasWon
  "Test if a board has won on a horizontal line"
  [board]
   (reduce (fn [state value]
             (cond
               (= true state) true
               (= #{} value) true
               :else false))
           false (concat (:horizontal board) (:vertical board))))

(def testboard (buildBoard '[(1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25)]))



(defn playNumberAgainstBoardIfWonAddToWonState
  "Play out a number against a board, if it won, add it to the wonboard state, if lost move to board list"
  [state board value]
    (let [newBoardState (playNumberAgainstBoard value board)]
      (cond
        (boardHasWon newBoardState) {:wonBoards (conj (:wonBoards state) {:boardState newBoardState :winningValue value}) :boards (:boards state)}
        :else                       {:wonBoards (:wonBoards state)                                                        :boards (conj (:boards state) newBoardState)}
        )
    )
  )

(playNumberAgainstBoard 13 (first (:boards boardStateBeforeThirteen)))
(boardHasWon shouldHaveWonBoardState)

(def shouldHaveWonBoardState *2)
(defn playInputValueAgainstBoards
  "Plays a given input value against the state of boards not yet won"
  [state value]
  (reduce (fn [nextstate board] (playNumberAgainstBoardIfWonAddToWonState nextstate board value)) {:wonBoards (:wonBoards state) :boards ()} (:boards state))
)

(defn calculateSolution
  "Given some parsed input values and first or last to pick first or last item in the result set, produce the answer to puzzle"
  [inputValues firstOrLast]
  (let [results (reduce playInputValueAgainstBoards {:wonBoards [] :boards (:boards inputValues)} (:inputs inputValues))
        firstResult (firstOrLast (:wonBoards results))]
      (calcBoardWin (:boardState firstResult) (:winningValue firstResult))))

(calculateSolution testValues first)
(calculateSolution puzzleValues first)
(calculateSolution testValues last)
(calculateSolution puzzleValues last)
