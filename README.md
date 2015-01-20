# kic -- Kodkod in Clojure

kic is an ultrathin wrapper for the Kodkod constraint solver.

Kodkod is an efficient SAT-based constraint solver for relational
logic, developed by Emina Torlak, see [Kodkod, a constraint solver 
for relational logic](http://alloy.mit.edu/kodkod/).

kic is in no way complete, by now, it's more a playground - work in
progress. E.g., Integer expressions, evaluations in models and extraction 
of unsat cores are not supported yet. 

Furthermore, it's highly desirable to make kic as a
Clojure API to Kodkod much more elegant.

## Ingredients of a kic specification

### The structure of our world

A kic specification is based on a fixed structure,
comprising a finite universe of "things" and a set
of relation variables.

The structure of the world for the problem to
be solved is in Clojure represented as a map. It is
build by the function `structure`:

`structure things relvars` where `things` is a vector of
objects for our world, and `relvars` is a map of keys and arities,
one for each relation variable in the structure.

As an example let's define a structure for a latin square of
order 4:

```clojure
(def ls-world
  (kic/structure [1 2 3 4] {:grid 3}))
  
ls-world
;=> {:univ #<Universe [1, 2, 3, 4]>, :grid #<Relation grid>}  
```
                 
The structure is `ls-world` with the universe `:univ` of the
integers 1, 2, 3, 4 and a ternary relation variable `:grid` 
that represents the latin square to be build. Each tuple `[x y d]`
denotes the value `d` in the cell with coordinates `x` and `y` of 
the latin square.

### The constraints on the relvars of the structure
 
The constraints on the relation variables of the structure are
formulated as formulae with expressions of the relational algebra.

The abstract syntax together with the semantic of Kodkod is described
in figure 2-1 on page 29 of [_Emina Torlak_ A Constraint Solver for 
Software Engineering: Finding Models and Cores of Large Relational 
Specifications](http://homes.cs.washington.edu/~emina/pubs/kodkod.phd.pdf)

kic does not much more than providing the same language in a lispy prefix
syntax.

Continuing our example for a latin square, we have to specify that each
cell has just one digit and that the digits in the rows and the columns 
of the latin square are pairwise different:

```clojure
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
```    

### Partial solutions defined by upper and lower bounds on the relation variables

In Kodkod one defines partial solutions for the specification by giving relations
for the lower bound and the upper bound of allowed assignments to the relation
variables.

The lower bound for a relation variable is a relation that must be a subset of any
solution relation for the relation variable. The upper bound for a relation variable
is a superset of any solution.

If we want to specify a latin square of order 4 with the first row given as
`1 2 3 4`, the bounds for the specification are:

```clojure
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
```

### Remark

In Kodkod the relation variables are called 'relation' and for the
relations the term 'tupleset' is used. I prefer 'relation variable' 
(or short relvar) and 'relation' as used in the theory of relational 
algebra.
## Running the solver 

The Kodkod constraint solver translates the specification into a (huge)
proposition and delegates the task to find a model to a SAT solver. If
the SAT solver finds a satisfying valuation for the proposition it is translated
back into relations that are assigned to the relation variables.

Let's generate a latin square from the specification above: 

```clojure
(defn solve-ls4 []
 (let  [sol (kic/solve constraints bounds)
        sol-grid (sort (:grid (kic/model sol)))]
    (map #(% 2) sol-grid)))
```

Given the partial latin square

```
+---------+
| 1 2 3 4 |
| . . . . |
| . . . . |
| . . . . |
+---------+
```
 The result is:
```
+---------+
| 1 2 3 4 |
| 4 3 1 2 |
| 2 1 4 3 |
| 3 4 2 1 |
+---------+
```



## License

Copyright (C) 2014 by Burkhardt Renz, Technische Hochschule Mittelhessen (THM).

Distributed under the Eclipse Public License, the same as Clojure.

kic is based on Kodkod licensed under the MIT license. Kodkod can be combined with a couple
of SAT solvers that may have different licenses. The default SAT solver in kic is 
[SAT4J](http://www.sat4j.org), licensed under both the Eclipse Public License and the 
GNU LGPL licence.

