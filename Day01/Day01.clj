(ns Day01)
(require '(clojure.java.io))
(defn lazy-file-lines [file]
  (letfn [(helper [rdr]
                 (lazy-seq
                  (if-let [line (.readLine rdr)]
                    (cons line (helper rdr))
                    (do (.close rdr) nil))))]
    (helper (clojure.java.io/reader file))))

(count (lazy-file-lines "c:/Users/jonrc/Documents/repos/AdventOfCode2021/Day01/input.txt"))

(defn parse-int [s]
  (Integer/valueOf (re-find #"\d+" s)))

(defn countOfIncreasesAndLastVal [state newVal]
  (cond
    (= nil state) {:last newVal :count 0}
    (< (:last state) newVal) {:last newVal :count (inc (:count state))}
    :else {:last newVal :count (:count state)}))
(->>
(lazy-file-lines "c:/Users/jonrc/Documents/repos/AdventOfCode2021/Day01/input.txt")
(map parse-int)
(reduce countOfIncreasesAndLastVal nil)
 )

(defn threeWayState [state newVal]
  (cond
    (= nil state) {:first newVal :second nil :third nil :count 0}
    (= nil (:second state)) {:first (:first state) :second newVal :third nil :count 0}
    (= nil (:third state))  {:first (:first state) :second (:second state) :third newVal :count 0}
    (< (+ (:first state) (:second state) (:third state)) (+ (:second state) (:third state) newVal)) {:first (:second state) :second (:third state) :third newVal :count (inc (:count state))}
    :else {:first (:second state) :second (:third state) :third newVal :count (:count state)}
  
  ))

(->>
 (lazy-file-lines "c:/Users/jonrc/Documents/repos/AdventOfCode2021/Day01/input.txt")
 (map parse-int)
 (reduce threeWayState nil))