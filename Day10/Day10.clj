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
  ((reduce (fn [state letter] 
            (cond (not= 0 (:score state))     state
                  (contains? openers letter)  (update state :seq conj letter)
                  :else                       (determineIfMatch state letter)
                  )) {:score 0 :seq ()} (seq line)) :score)))

(defn determineScoreForInputLines
  "Determines the sum of the score across the input lines"
  [inputLines]
  (reduce (fn [score line] (+ score (scoreSyntaxCorruption line))) 0 inputLines))

(scoreSyntaxCorruption (first testInput))
(determineScoreForInputLines testInput)
(determineScoreForInputLines puzzleInput)
(set (seq "[(<{"))