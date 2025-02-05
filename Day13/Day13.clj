(ns Day13)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day13/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("6,10"
                "0,14"
                "9,10"
                "0,3"
                "10,4"
                "4,11"
                "6,0"
                "6,12"
                "4,1"
                "0,13"
                "10,12"
                "3,4"
                "3,0"
                "8,4"
                "1,10"
                "2,14"
                "8,10"
                "9,0"
                ""
                "fold along y=7"
                "fold along x=5"))

(defn buildStateFromInput
  "Build the starting state from the puzzle input"
  [lines]
  (reduce (fn [state line]
            (let [dotmatches (re-matches #"^(\d+),(\d+)$" line)
                  foldmatches (re-matches #"^fold along ([xy])=(\d+)$" line)]
              (cond (not= nil dotmatches)  (let [[full xstr ystr] dotmatches
                                                 x (Day01/parse-int xstr)
                                                 y (Day01/parse-int ystr)]       (update state :dots conj [x y]))
                    (not= nil foldmatches) (let [[full xy linestr] foldmatches
                                                 line (Day01/parse-int linestr)] (update state :folds conj (list xy line)))
                    :else  state
              ))) {:dots #{} :folds []} lines))

(comment ; Just a quick test of the testInput and state that is built
(buildStateFromInput testInput)
)

(defn foldOnX
  "Given a dotset and an x-line, fold the dotset"
  [dotset xline]
  (reduce (fn [ds pos] (cond (> xline (first pos)) (conj ds pos)
                             :else                 (conj ds [(- xline (- (first pos) xline)) (last pos)])
                             )
            ) #{} dotset))

(defn foldOnY
  "Given a dotset and a y-line, fold the dotset"
  [dotset yline]
  (reduce (fn [ds pos] (cond (> yline (last pos)) (conj ds pos)
                             :else (conj ds [(first pos) (- yline (- (last pos) yline))])               
                             )
            ) #{} dotset))

(defn makeFoldOfTheDots
  "Given a fold line, fold the dot field"
  [dotset fold]
  (let [[xOrY line] fold]
    (cond (= "x" xOrY) (foldOnX dotset line)
          (= "y" xOrY) (foldOnY dotset line)
          :else dotset)))

(defn CountDotsAfterOneFold
  "Given an input, process the input and present the number of dots visible after first fold instruction"
  [inputs]
  (let [puzzleState (buildStateFromInput inputs)
        dotset (:dots puzzleState)
        firstfold (first (:folds puzzleState))]
    (count (makeFoldOfTheDots dotset firstfold))))
(comment
(= 17 (CountDotsAfterOneFold testInput))
)
(CountDotsAfterOneFold puzzleInput)

(defn performAllFolds
  "Given an input, process the input and present the dotset after all folds are completed"
  [inputs]
  (let [puzzleState (buildStateFromInput inputs)
        dotset (:dots puzzleState)
        folds (:folds puzzleState)]
    (reduce makeFoldOfTheDots dotset folds)))

(sort (performAllFolds testInput))
(defn visualizeDots
 "Produce a visual representation of the dotset"
 [dotset]
 (let [maxX (+ 1 (first (apply max-key first dotset)))
       maxY (+ 1 (second (apply max-key second dotset)))]
   (reduce (fn [state value] (println value)) nil 
   (loop [x 0 y 0 lines [] line []]
     (cond (and (= y maxY) (= x maxX)) lines
           (= x maxX) (recur 0 (inc y) (conj lines (clojure.string/join "" (conj line (if (contains? dotset [x y]) "#" " ")))) [])
           :else      (recur (inc x) y lines (conj line (if (contains? dotset [x y]) "#" " "))))))
 ))

(apply max-key second (sort (performAllFolds testInput)))
(visualizeDots (performAllFolds puzzleInput))