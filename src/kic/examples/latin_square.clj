(ns kic.examples.latin-square
  (:require [kic.core :as kic])
  (:require [kic.rel :as rel])
  (:require [clojure.set :as set])
)

;; Structure of the Latin Square world
(defn ls-world
  [size]
  (kic/structure (vec (range 1 (inc size)))
                 {:grid 3}))

;; The world has as universe the integers 1..size.

;; :grid is a ternary relation representing the cells with their value, a tuple of
;; :grid [x y v] means: the cell index with x and y of the square has value v

;; Expressing the constraints for a latin square:

(defn- cells
  "Expression for the cells in the grid given by rows and cols."
  [structure rows cols]
  (kic/dotjoin cols (kic/dotjoin rows (kic/relvar structure :grid))))

(defn rules
  [structure]
  (let [x    (kic/variable :x)
        y    (kic/variable :y)
        u    kic/UNIV

        f1   (kic/forall [x y] (kic/one (cells structure x y)))
        ; "all x, y : one y.(x.grid), i.e. Each cell holds exactly one number."
        f2   (kic/forall [x y] (kic/no (kic/intersection
                                         (cells structure x y)
                                         (cells structure x (kic/difference u y)))))
        ; "all x, y : no y.(x.grid) ∩ (u-y).(x.grid), i.e. the number in a cell
        ; does not occur in another cell of the same row."
        f3   (kic/forall [x y] (kic/no (kic/intersection
                                         (cells structure x y)
                                         (cells structure (kic/difference u x) y))))
        ; "all x, y : no y.(x.grid) ∩ y.((u-x).grid), i.e. the number in a cell
        ; does not occur in another cell of the same column."
        ]
    (kic/and f1 f2 f3)))

;; Example:
;; Latin square of size 4

(def structure (ls-world 4))

(def constraints (rules structure))

; defining uper and lower bounds
(defn- row [idx] (inc (quot idx 4)))
(defn- col [idx] (inc (rem idx 4)))

(defn- upper-tuples
  [^Integer n ^Integer idx]
  (let [row (row idx) col (col idx) univ (kic/univ structure)]
    (if-not (zero? n)
      #{[row col n]}
      (rel/crossjoin #{[row col]} univ))))

(defn- lower-tuple
  [^Integer n ^Integer idx]
  (let [row (row idx) col (col idx)]
    (if-not (zero? n)
      #{[1 col col]}
      nil)))

(defn- grid-bounds
  [givens]
  (loop [s givens, i 0, lower #{}, upper #{}]
    (if (empty? s)
      [lower upper]
      (recur (next s) (inc i) (set/union lower (lower-tuple (first s) i))
             (set/union upper (upper-tuples (first s) i))))))
  

(defn ls-bounds
  [givens]
  (kic/bounds structure {:grid (grid-bounds givens)}))

(defn solve
  [givens]
  (let  [sol (kic/solve constraints (ls-bounds givens))
         sol-grid (sort (:grid (kic/model sol)))]
    (map #(% 2) sol-grid)))

; we sort the relation of the :grid rel var
; the solution of the puzzle is the value at index 2 of the tuples of sol-grid

(defn pretty-print
  [n givens]
  (let [line (apply str (repeat (inc (* 2 n)) "-"))
        ruler (str "+" line "+\n")]
    (doseq [[row col dch] (map-indexed #(vector (inc (quot %1 n)) (inc (rem %1 n)) %2) givens)]
      (let [ch (if (zero? dch) \. dch)]
        (if (and (= 1 col) (= 1 row)) (print ruler))
        (if (= 1 col) (print (str "| " ch " "))
                      (print (str ch " ")))
        (if (= n col) (print "|\n"))))
    (print ruler)))

(def example [1 2 3 4 0 0 0 0 0 0 0 0 0 0 0 0])

(pretty-print 4 example)

(pretty-print 4 (solve example))

; we can make this example more explicit by giving upper und lower bound literal:

(def lower
  #{[1 1 1]
    [1 2 2]
    [1 3 3]
    [1 4 4]})

(def upper
  #{[1 1 1]
    [1 2 2]
    [1 3 3]
    [1 4 4]
    [2 1 1] [2 1 2] [2 1 3] [2 1 4]
    [2 2 1] [2 2 2] [2 2 3] [2 2 4]
    [2 3 1] [2 3 2] [2 3 3] [2 3 4]
    [2 4 1] [2 4 2] [2 4 3] [2 4 4]
    [3 1 1] [3 1 2] [3 1 3] [3 1 4]
    [3 2 1] [3 2 2] [3 2 3] [3 2 4]
    [3 3 1] [3 3 2] [3 3 3] [3 3 4]
    [3 4 1] [3 4 2] [3 4 3] [3 4 4]
    [4 1 1] [4 1 2] [4 1 3] [4 1 4]
    [4 2 1] [4 2 2] [4 2 3] [4 2 4]
    [4 3 1] [4 3 2] [4 3 3] [4 3 4]
    [4 4 1] [4 4 2] [4 4 3] [4 4 4]})

(def bounds 
  (kic/bounds structure {:grid [lower upper]}))

(defn solve-ls4 []
 (let  [sol (kic/solve constraints bounds)
        sol-grid (sort (:grid (kic/model sol)))]
    (map #(% 2) sol-grid)))

(pretty-print 4 (solve-example))

