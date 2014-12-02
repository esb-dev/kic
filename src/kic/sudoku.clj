(ns kic.sudoku
  (:require [kic.core :as kic])
  (:require [kic.rel :as rel])
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
)  

(def puzzle "600200050018060020003000400000607800402050000000908000504090300020000014300005007")

;; Structure of the Sudoku world
(def sudoku (kic/structure 
              (vec (range 1 10))  ; universe atoms are integers 1..9
              {:grid 3, :region1 1, :region2 1, :region3 1}
))

; reicht es grid als Relation zu haben?
; oder: kann man reg<n> als comprehension ausdrücken?

;; Constructing the bounds for the puzzle
(def reg1 (rel/unary-rel (range 1  4)))
(def reg2 (rel/unary-rel (range 4  7)))
(def reg3 (rel/unary-rel (range 7 10)))

; bounds (independent from puzzle)
(def reg1-b {:region1 [reg1, reg1]})
(def reg2-b {:region2 [reg2, reg2]})
(def reg3-b {:region3 [reg3, reg3]})

; functions to calculate bounds for grid from given puzzle as sudoku-string
(defn- row 
  "Returns the row of the number at idx."
  [idx]
  (inc (quot idx 9)))

(defn- col 
  "Returns the col of the number at idx."
  [idx]
  (inc (rem idx 9)))

(defn- ctoi
  "Returns the int of given char."
  [char]
  {:pre [(Character/isDigit char)]}
  (- (int char) (int \0)))

(defn- upper-tuples 
  "Returns the tuples for the upper bound for number c (as char)."
  [^Character c ^Integer idx]
  (let [n (ctoi c) row (row idx) col (col idx) univ (kic/univ sudoku)]
    (if-not (= n 0)
      #{[row col n]}
      (rel/crossjoin #{[row col]} univ))))

(defn- lower-tuple
  "Returns the tuple for the lower bound for number c."
  [^Character c ^Integer idx]
  (let [n (ctoi c) row (row idx) col (col idx)]
    (if-not (= n 0)
      #{[row col n]}
      nil)))
      
(defn- grid-bounds 
  "Returns lower and upper bounds of :grid for the given puzzle."
  [^String puzzle]
  (loop [s puzzle, i 0, lower #{}, upper #{}]
    (if (empty? s)
      [lower upper]
      (recur (next s) (inc i) (set/union lower (lower-tuple (first s) i))
                              (set/union upper (upper-tuples (first s) i))))))

(defn sudoku-bounds
  "Returns bounds for the given puzzle."
  [puzzle]
  (kic/bounds sudoku (merge reg1-b reg2-b reg3-b {:grid (grid-bounds puzzle)})))

;; rules
(defn- cells 
  "Returns the expression for the cells in the grid given by rows and cols.
   Uses var sudoku!"
  [rows cols]
  (kic/dotjoin cols (kic/dotjoin rows (kic/relvar sudoku :grid))))
  
(defn- complete 
  "Returns the formula whether all numbers are in the given cells.
   Uses var sudoku!"
  [rows cols]
  (kic/in kic/UNIV (cells rows cols)))

(def reg-rules
  (let [r1   (kic/relvar sudoku :region1)
        r2   (kic/relvar sudoku :region2)
        r3   (kic/relvar sudoku :region3)]
    (apply kic/and 
      (for [x [r1 r2 r3] y [r1 r2 r3]]
        (complete x y)))))

(def rules
  (let [x    (kic/variable :x)
        y    (kic/variable :y)
        d    (kic/decls (kic/decl x) (kic/decl y))
        n    kic/UNIV
        
        f1   (kic/all d (kic/one (cells x y)))
        ; "all x, y | one y.(x.grid)"
        f2   (kic/all (kic/decl x) (complete x n))
        ; "all x | n in n.(x.grid)"
        f3   (kic/all (kic/decl y) (complete n y))
        ; "all y | n in y.(n.grid)"
        ]
    (kic/and f1 f2 f3 reg-rules)))

(defn solve-sudoku
  "Returns the solution of the puzzle."
  [puzzle]
  (let  [sol (kic/solve rules (sudoku-bounds puzzle))
         sol-grid (sort (((kic/inst-from-solution sol) 1) :grid))]
    (apply str (map #(% 2) sol-grid))))

; helper functions for pretty printing sudoku
(defn- cchar
  "Returns formatted character"
  [i c]
  (let [out (if (= c \0) \space c)]
    (if (= (mod i 3) 0)
      (str \space out \space \|)
      (str \space out))))

(defn- cline
  "Returns a formatted row"
  [row-string]
  (loop [s row-string i 1 result "|"]
    (if (empty? s)
      (str result "\n")
      (recur (next s) (inc i) (str result (cchar i (first s)))))))

(defn- ssplit
  "Returns vec of 9 substrings."
  [sudoku-string]
  (let [matcher (re-matcher #".{9}" sudoku-string)]
    (loop [match (re-find matcher) result []]
      (if-not match
        result
        (recur (re-find matcher) (conj result match))))))

(defn pretty-print-sudoku
  "(pretty-print-sudoku sudoku-string) -> string containing the readable grid."
  [^String sudoku-string]
  {:pre [(= (count sudoku-string) 81)]}
  (let [hline "+-------+-------+-------+\n"
        hrow (fn [i s]
               (if (= (mod i 3) 0)
                 (str hline (cline s))
                 (cline s)))]
  (loop [v (ssplit sudoku-string) i 0 result ""]
    (if (empty? v)
      (str result hline)
      (recur (next v) (inc i) (str result (hrow i (first v))))))))
                  
(print (pretty-print-sudoku (solve-sudoku puzzle)))
                   
    


