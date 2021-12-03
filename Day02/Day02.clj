(ns Day02)

(count (Day01/lazy-file-lines "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day02/input.txt"))

(defn InterpretSubDirection [s]
  (cond
    (re-matches #"forward \d+" s) {:x (Day01/parse-int s) :y 0}
    (re-matches #"down \d+" s)    {:x 0                   :y (Day01/parse-int s)}
    (re-matches #"up \d+" s)      {:x 0                   :y (- 0 (Day01/parse-int s))}))


(InterpretSubDirection "forward 10")
(InterpretSubDirection "up 5")
(InterpretSubDirection "down 3")
(not= nil (re-matches #"forward \d+" "up 10"))
(Day01/parse-int "forward 10")

(defn MoveSubmarine [position adjustments]
  {:x (+ (:x position) (:x adjustments)) :y (+ (:y position) (:y adjustments))})

(defn MultiplyHorizontalByVertical [pos]
  (* (:x pos) (:y pos)))
(->>
 (Day01/lazy-file-lines "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day02/input.txt")
 (map InterpretSubDirection)
 (reduce MoveSubmarine {:x 0 :y 0})
 (MultiplyHorizontalByVertical))

(defn InterpretComplexSubDirection [s]
  (cond
    (re-matches #"forward \d+" s) {:forward (Day01/parse-int s) :aim 0}
    (re-matches #"up \d+" s) {:forward 0 :aim (- 0 (Day01/parse-int s))}
    (re-matches #"down \d+" s) {:forward 0 :aim (Day01/parse-int s)}))
(defn MoveComplexSubmarine [position adjustments]
  (cond
    (= 0 (:aim adjustments)) {:x (+ (:x position) (:forward adjustments))
                              :y (+ (:y position) (* (:aim position) (:forward adjustments)))
                             :aim (:aim position)}
    (= 0 (:forward adjustments)) {:x (:x position) :y (:y position) :aim (+ (:aim position) (:aim adjustments))}
))
(->>
 (Day01/lazy-file-lines "C:/Users/jonrc/Documents/repos/AdventOfCode2021/Day02/input.txt")
 (map InterpretComplexSubDirection)
 (reduce MoveComplexSubmarine {:x 0 :y 0 :aim 0})
 (MultiplyHorizontalByVertical))