(ns kic.examples.sudoku
  (:require [kic.core :as kic])
  (:require [kic.rel :as rel])
  (:require [clojure.java.io :refer (reader)])
  (:require [clojure.set :as set])
)

(defn ctoi
  "Returns the int of given char, 0 if char is '.'."
  [char]
  (let [i0 (int \0)]
    (if (= \. char) 0 (- (int char) (int \0)))))

;; A puzzle is represented as a vector of integers
(def puzzle (map ctoi ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))

puzzle

;; Structure of the Sudoku world
(def sudoku (kic/structure (vec (range 1 10))  ; atoms of the univers: integers 1..9
              {:grid 3, :region1 1, :region2 1, :region3 1}))

;; The Sudoku world has as universe the integers 1..9.

;; :grid is a ternary relation representing the cells with their value, a tuple of
;; :grid [x y v] means: the cell index with x and y of the puzzle has value n

;; :region1..:region3 are unary relations, used as helpers to express the blocks
;; of the puzzle

;; Expressing the rules of Sudoku:

;; rules
(defn- cells
  "Expression for the cells in the grid given by rows and cols."
  [rows cols]
  (kic/dotjoin cols (kic/dotjoin rows (kic/relvar sudoku :grid))))

(defn- complete
  "Formula whether all numbers 1..9 are in the given cells."
  [rows cols]
  (kic/in kic/UNIV (cells rows cols)))

(def reg-blocks
  "Each block contains all numbers."
  (let [r1   (kic/relvar sudoku :region1)
        r2   (kic/relvar sudoku :region2)
        r3   (kic/relvar sudoku :region3)]
    (apply kic/and
           (for [x [r1 r2 r3] y [r1 r2 r3]]
             (complete x y)))))

(def rules
  (let [x    (kic/variable :x)
        y    (kic/variable :y)
        n    kic/UNIV

        f1   (kic/forall [x y] (kic/one (cells x y)))
        ; "all x, y | one y.(x.grid), i.e. Each cell holds exactly one number."
        f2   (kic/forall [x] (complete x n))
        ; "all x | n in n.(x.grid), i.e. Each row contains all numbers."
        f3   (kic/forall [y] (complete n y))
        ; "all y | n in y.(n.grid), i.e. Each column contains all numbers.
        ]
    (kic/and f1 f2 f3 reg-blocks)))

(def rules'
  (let [x    (kic/variable :x)
        y    (kic/variable :y)
        n    kic/UNIV

        f1   (kic/forall [x y] (kic/one (cells x y)))
        ; "all x, y | one y.(x.grid), i.e. Each cell holds exactly one number."
        f2   (kic/forall [x y] (kic/no (kic/intersection
                                         (cells x y)
                                         (cells x (kic/difference n y)))))
        f3   (kic/forall [x y] (kic/no (kic/intersection
                                         (cells x y)
                                         (cells (kic/difference n x) y))))
        ]
    (kic/and f1 f2 f3 reg-blocks)))

;; Constructing the bounds for the puzzle

; In order to solve the constraint problem, we have to define the lower and the upper bound
; of the relation in the structure of our problem at hand.

; The upper bound are the relations for the relation variables, that contains every _possible_
; tuple of the solution.

; The lower bound are the relations containing the tuples that _have to be_ part of the
; solution. The lower bound is a partial solution of the problem.

; For the three relation variables region1 to region 3 the lower and the upper bound are
; identical, since we want i.e. region1 just have the value #{[1], [2], [3]}.

; In kic the bounds are defined for each relation variable by a hash map with rel var as key and
; a vector of relations for upper and lower bound as value.

; bounds for :region1 .. :region3

(let [reg1 (rel/unary-rel (range 1  4))
      reg2 (rel/unary-rel (range 4  7))
      reg3 (rel/unary-rel (range 7 10))]
  (def reg1-b {:region1 [reg1, reg1]})
  (def reg2-b {:region2 [reg2, reg2]})
  (def reg3-b {:region3 [reg3, reg3]}))

; bounds for the given puzzle

(defn- row
  "Returns the row of the number at idx."
  [idx]
  (inc (quot idx 9)))

(defn- col
  "Returns the col of the number at idx."
  [idx]
  (inc (rem idx 9)))

(defn- upper-tuples
  "Returns the tuples for the upper bound for number n at idx."
  [^Integer n ^Integer idx]
  (let [row (row idx) col (col idx) univ (kic/univ sudoku)]
    (if-not (zero? n)
      #{[row col n]}
      (rel/crossjoin #{[row col]} univ))))

(defn- lower-tuple
  "Returns the tuple for the lower bound for number n at idx."
  [^Integer n ^Integer idx]
  (let [row (row idx) col (col idx)]
    (if-not (zero? n)
      #{[row col n]}
      nil)))

(defn- grid-bounds
  "Returns lower and upper bounds of :grid for the given puzzle."
  [puzzle]
  (loop [s puzzle, i 0, lower #{}, upper #{}]
    (if (empty? s)
      [lower upper]
      (recur (next s) (inc i) (set/union lower (lower-tuple (first s) i))
             (set/union upper (upper-tuples (first s) i))))))

(defn sudoku-bounds
  "Returns bounds for the given puzzle."
  [puzzle]
  (kic/bounds sudoku (merge reg1-b reg2-b reg3-b {:grid (grid-bounds puzzle)})))

(defn solve
  "Returns the solution of the puzzle."
  [puzzle]
  (let  [sol (kic/solve rules' (sudoku-bounds puzzle))
         sol-grid (sort (((kic/model sol) 1) :grid))]
    (map #(% 2) sol-grid)))

; kic/model is a vector whose second entry is a map with key the rel vars and values the
; relation of the solution

; we sort the relation of the :grid rel var
; the solution of the puzzle is the value at index 2 of the tuples of sol-grid


(defn pretty-print
  "Prints puzzle of order 3 decoded as a vector of integers."
  [puzzle]
  (let [rule "+-------+-------+-------+\n"]
    (doseq [[row col dch] (map-indexed #(vector (inc (quot %1 9)) (inc (rem %1 9)) %2) puzzle)]
      (let [ch (if (zero? dch) \. dch)]
        (if (and (= 1 col) (= 1 (mod row 3))) (print rule))
        (cond (= 1 (mod col 3)) (print (str "| " ch ))
              (= 2 (mod col 3)) (print (str " " ch ))
              (= 0 (mod col 3)) (print (str " " ch " ")))
        (if (= 9 col) (print "|\n"))))
    (print rule)))

(pretty-print puzzle)

(pretty-print (solve puzzle))

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .
;; Other lines in the file are ignored

(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into () (map #(map ctoi %) (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

;; ## Benchmarks
(defn bench
  [puzzles]
  (time
    (do
      (dorun (map solve puzzles))
      :done)))

;; easy50.txt
(def easy50 (parse "resources/sudoku/easy50.txt"))

easy50

(bench easy50)
;=> 8.8 secs

;; top95.txt
(def top95 (parse "resources/sudoku/top95.txt"))

top95

(bench top95)
;=> 91 secs

;; hardest.txt
(def hardest (parse "resources/sudoku/hardest.txt"))

hardest

(bench hardest)
;=> 5 secs
