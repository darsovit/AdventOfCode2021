(ns Day10)
(def filename "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day10/input.txt")
(def puzzleInput (Day01/lazy-file-lines filename))
(def testInput '("[({(<(())[]>[[{[]{<()<>>"
                 "[(()[<>])]({[<{<<[]>>("
                 "{([(<{}[<>[]}>{[]{[(<()>"
                 "(((({<>}<{<{<>}{[]{[]{}"
                 "[[<[([]))<([[{}[[()]]]"
                 "[{[{({}]{}}([{[{{{}}([]"
                 "{<[[]]>}<{[{[{[]{()[[[]"
                 "[<(<(<(<{}))><([]([]()"
                 "<{([([[(<>()){}]>(<<{{"
                 "<{([{{}}[<[[[<>{}]]]>[]]"))

(defn closesOpener
  "Determine if the given start character and close character are matches"
  [startLetter endLetter]
  (or (and (= startLetter '\[) (= endLetter '\]))
      (and (= startLetter '\() (= endLetter '\)))
      (and (= startLetter '\{) (= endLetter '\}))
      (and (= startLetter '\<) (= endLetter '\>))))

(closesOpener '\( '\))
(closesOpener '\< '\>)
(closesOpener '\{ '\})
(closesOpener '\[ '\])


(defn scoreLetter
  "Determines the score for the input letter"
  [letter]
  (cond (= letter '\)) 3
        (= letter '\]) 57
        (= letter '\}) 1197
        (= letter '\>) 25137))

(defn determineIfMatch
  "Determine if the given input letter matches the top-most element on the list, pop it if so, score if not"
  [state letter]
  (cond (closesOpener (first (:seq state)) letter) (update state :seq rest)
        :else (update state :score (constantly (scoreLetter letter)))))

(defn scoreSyntaxCorruption
  "Determine the score for the syntax corruption in the given line"
  [line]
  (let [openers (set (seq "[(<{"))]
  (reduce (fn [state letter] 
            (cond (not= 0 (:score state))     state
                  (contains? openers letter)  (update state :seq conj letter)
                  :else                       (determineIfMatch state letter)
                  )) {:score 0 :seq ()} (seq line))))

(defn determineSyntaxCorruptionScoreForInputLines
  "Determines the sum of the score across the input lines"
  [inputLines]
  (reduce (fn [score line] (+ score (:score (scoreSyntaxCorruption line)))) 0 inputLines))

(scoreSyntaxCorruption (first testInput))
(determineSyntaxCorruptionScoreForInputLines testInput)
(determineSyntaxCorruptionScoreForInputLines puzzleInput)

(defn calculateAutoCompleteScore
  "Calculate the auto-complete score for a syntax-checked string remainder"
  [letters]
  (let [lettermapping {'\( 1 '\[ 2 '\{ 3 '\< 4}]
    (reduce (fn [score letter] (+ (lettermapping letter) (* 5 score))) 0 letters)))

(defn determineAutoCorrectScoreForInputLines
  "Determines the average autocorrect score for input lines"
  [inputLines]
  (reduce (fn [scores line]
            (let [syntaxScoreState (scoreSyntaxCorruption line)]
              (cond (= 0 (:score syntaxScoreState)) (conj scores (calculateAutoCompleteScore (:seq syntaxScoreState)))
                    :else scores))
            ) [] inputLines))

(defn getMiddleOfSortedScores
  "Get the middle value of the sorted scores"
  [scores]
  (nth scores (quot (count scores) 2)))

(getMiddleOfSortedScores (sort (determineAutoCorrectScoreForInputLines testInput)))
(getMiddleOfSortedScores (sort (determineAutoCorrectScoreForInputLines puzzleInput)))