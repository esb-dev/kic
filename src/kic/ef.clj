; kic - Kodkod in Clojure
; based on the Kodkod Constraint Solver, see http://alloy.mit.edu/kodkod/
; licensed under the MIT license

; Copyright (c) 2014 by Burkhardt Renz THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.


(ns ^{:doc    "A Clojure wrapper to the Kodkod constraint solver. 
               This part comprises the function for expressions and
               formulae, it is loaded in kic.core"
      :author "Burkhardt Renz THM"}
  kic.core
  (:refer-clojure :exclude [and or not some bit-and bit-or bit-xor bit-not mod count])
  (:import [kodkod.ast Relation Variable])
  (:import [kodkod.ast Formula ComparisonFormula])
  (:import [kodkod.ast Expression IntExpression IntConstant])
  (:import [kodkod.ast Decls Decl])
)           

;; ## Formulae
;;

;; Definition of the operators for kic formulae

(defn and
  "(and & fmls), the conjunction of the given fmls "
  [& fmls]
  (Formula/and ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

(defn or
  "(or & fmls), the disjunction of the given fmls"
  [& fmls]
  (Formula/or ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

(defn iff
  "(iff  fml1 fml2), the equivalence of the given fmls"
  [^Formula fml1 ^Formula fml2]
  (.iff fml1 fml2))

(defn impl
  "(impl  fml1 fml2), the implication fml1 -> fml2."
  [^Formula fml1 ^Formula fml2]
  (.implies fml1 fml2))

(defn not
  "(not fml), the negation of fml."
  [^Formula fml]
  (.not fml))

(def TRUE
  "TRUE, truth, i.e. the constant fml TRUE."
  Formula/TRUE)

(def FALSE
  "FALSE, contradiction, i.e. the constant fml FALSE."
  Formula/FALSE)

(defn in
  "(in expr1 expr2), expressing whether expr1 is a subset of expr2."
  [^Expression expr1 ^Expression expr2]
  {:pre (= (. expr1 arity) (. expr2 arity))}
  (.in expr1 expr2))

(defmulti eq
  "(eq expr1 expr2), expressing whether expr1 equals expr2.
   The types of expr1 and expr1 are Expression or IntExpression."
   (fn [e1 e2] [(class e1) (class e2)] ))

(defmethod eq [Expression Expression]
  [^Expression expr1  ^Expression expr2]
  (.eq expr1 expr2)) 

(defmethod eq [IntExpression IntExpression]
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.eq int-expr1 int-expr2))

(defn lone
  "(lone expr), expressing whether expr has at least one tuple."
  [^Expression expr]
  (.lone expr))

(defn one
  "(one expr), expressing whether expr has exactly one tuple."
  [^Expression expr]
  (.one expr))

(defn some
  "(some expr), expressing whether expr has 0..* tuples, or
   (some decls fml), the existential quantified formula fml."
  ([^Expression expr]
  (.some expr))
  ([^Decls decls ^Formula fml]
  (.forSome fml decls)))  

(defn no
  "(no expr), the formula expressing whether expr is empty."
  [^Expression expr]
  (.no expr))

(defn total-ordering
  "(total-ordering rel ordered first last),
   a formula expressing that rel imposes a total ordering over
   the unary relation (set) ordered where the first and last element 
   are given by the elements first and last resp."
  [^Relation rel ordered first last]
  {:pre (= (. rel arity) 2)} 
  (.totalOrder rel ordered first last))

(defn function
  "(function rel domain range),
   a formula expressing that rel is a function from expr domain to expr range."
  [^Relation rel domain range]
  {:pre (= (. rel arity) 2)} 
  (.function rel domain range))

(defn partial-function
  "(partial-function rel domain range),
   a formula expressing that rel is a partial function from expr domain to expr range."
  [^Relation rel domain range]
  {:pre (= (. rel arity) 2)} 
  (.partialFunction rel domain range))

(defn acyclic
  "(acyclic rel), a formula expressing that rel is acyclic."
  [^Relation rel]
  {:pre (= (. rel arity) 2)} 
  (.acyclic rel))

(defn lt
  "(lt int-expr1 int-expr2), expressing whether int-expr1 < int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.lt int-expr1 int-expr2))

(defn lte
  "(lte int-expr1 int-expr2), expressing whether int-expr1 <= int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.lte int-expr1 int-expr2))

(defn gt
  "(gt int-expr1 int-expr2),  expressing whether int-expr1 > int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.gt int-expr1 int-expr2))

(defn gte
  "(gte int-expr1 int-expr2), expressing whether int-expr1 >= int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.gte int-expr1 int-expr2))

(defn all
  "(all decls fml), universal quantifier over decls and fml."
  [^Decls decls ^Formula fml]
  (.forAll fml decls))

(declare decls decl)

(defn forall
  "fml holds for all variables in var-vec with values from the universe,
  e.g. (forall [x y] (complete x y))"
  [var-vec fml]
  (let [d (apply decls (map decl var-vec))]
    (.forAll fml d)))

;; Expressions denoting the manipulation of values of relational variables

(defn intersection
  "(intersection expr & more-exprs), the intersection of the arguments."
  [expr & more-exprs]
  (Expression/intersection ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

(defn override
  "(override expr & more-exprs), the override of the arguments."
  [expr & more-exprs]
  (Expression/override ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

(defn product
  "(product expr & more-exprs), the cartesian product of the arguments."
  [expr & more-exprs]
  (Expression/product ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

(defn union
  "(union expr & more-exprs), the union of the arguments."
  [expr & more-exprs]
  (Expression/union ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

(defn difference
  "(difference expr1 expr2), the difference of the arguments."
  [^Expression expr1 ^Expression expr2]
  (.difference expr1 expr2))

(defn dotjoin
  "(dotjoin expr1 expr2), the dotjoin of the arguments."
  [^Expression expr1 ^Expression expr2]
  (.join expr1 expr2))

(defn tclose
  "(tclose bin-expr), the transitive closure of binary expression bin-expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.closure expr))

(defn rtclose
  "(rtclose bin-expr), the reflexive transitive closure of bin-expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.reflexiveClosure expr))

(defn transpose
  "(transpose bin-expr), the transposition of bin-expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.transpose expr))

(defn project
  "(project expr int-expr & more-int-exprs)
   the projections of expr on the columns given by the int-expr arguments."
  [^Expression expr int-expr & more-int-exprs]
  (.project expr (into-array (cons int-expr more-int-exprs))))

(def NONE
  "NONE, the unary relation without tuples."
   Expression/NONE)

(def UNIV
  "UNIV, the universal relation, i.e. the unary relation containing
   one-tuples of all atoms of the universe."
   Expression/UNIV)

(def IDEN
  "IDEN, the binary identity relation over the universe."
   Expression/IDEN)

;; Integers as relation
(def INTS
  "INTS, the unary relation containing all atoms bound to integers."
   Expression/INTS)

(defmulti ite
  "(ite fml expr1 expr2)
   (ite fml int-expr1 int-expr2)
   if fml then expr1 else expr2"
   (fn [fml expr1 expr2] (class expr1)))

(defmethod ite Expression 
  [^Formula fml ^Expression expr1 ^Expression expr2]
  (.thenElse fml expr1 expr2))

(defmethod ite IntExpression 
  [^Formula fml ^IntExpression int-expr1 ^IntExpression int-expr2]
  (.thenElse fml  int-expr1 int-expr2))

(defn int-cast
  "(int-cast int-expr), a relational expression of the given int-expr."
  [^IntExpression int-expr]
  (.toExpression int-expr))

(defn bitset-cast
  "(bitset-cast int-expr), an expression whose meaning
   is the set containing the atoms 
	 that represent the powers of 2 (bits) present in the int-expr."
  [^IntExpression int-expr]
  (.toBitset int-expr))

(defn comprehension
  "(comprehension decls fml), the expression defined by decls and fml."
  [^Decls decls ^Formula fml]
  (.comprehension fml decls))

;; Variables used in expression with quantifiers

(defn variable
  "(variable :var-name), constructs a variable of arity 1
   (variable :var-name arity), constructs a var of the given arity."
  ([var-name]
    (Variable/unary (name var-name)))
  ([var-name arity]
    (Variable/nary (name var-name) arity)))

;; Integer expressions

(defn plus
  "(plus int-expr & more-int-exprs), the sum of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/plus ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

(defn mult
  "(mult int-expr & more-int-exprs), the product of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/multiply ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

(defn bit-and
  "(bit-and int-expr & more-int-exprs), the bitwise and of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/and ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

(defn bit-or
  "(bit-or int-expr & more-int-exprs), the bitwise or of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/or ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

(defn sh-left
  "(sh-left int-expr1 int-expr2), left shift int-expr1 << int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.shl int-expr1 int-expr2))

(defn sh-right-sign
  "(sh-right-sign int-expr1 int-expr2)right shift with sign int-epr1 >>> int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.sha int-expr1 int-expr2))

(defn sh-right-zero
  "(sh-right-zero int-expr1 int-expr2) right shift int-expr1 >> int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.shr int-expr1 int-expr2))

(defn minus
  "(minus int-expr1 int-expr2), substraction  int-expr1 - int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.minus int-expr1 int-expr2))

(defn div
  "(div int-expr1 int-expr2), division int-expr1 / int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.divide int-expr1 int-expr2))

(defn mod
  "(mod int-expr1 int-expr2), modulo int-expr1 % int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.modulo int-expr1 int-expr2))

(defn bit-xor
  "(bit-xor int-expr1 int-expr2), bitwise xor int-expr1 xor int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.xor int-expr1 int-expr2))

(defn neg
  "(neg int-expr), negation -int-xpr."
  [^IntExpression int-expr]
  (.negate int-expr))

(defn bit-not
  "(bit-not int-expr), bitwise negation of the argument."
  [^IntExpression int-expr]
  (.not int-expr))

(defn abs
  "(abs int-expr), absolute value of the argument."
  [^IntExpression int-expr]
  (.abs int-expr))

(defn sgn
  "(sgn int-expr), sign of the argument."
  [^IntExpression int-expr]
  (.signum int-expr))

(defmulti sum
  "(sum decls int-expr), sum of all values int-expr can have for the given decls,
   (sum expr), sum of the integer atoms in expr."
  (fn [first & more] (class first)))

(defmethod sum Decls
  [^Decls decls ^IntExpression int-expr]
  (.sum int-expr decls))

(defmethod sum Expression
  [^Expression expr]
  (.sum expr))

(defn count
  "(count expr), number of tuples in expr."
  [^Expression expr]
  (.count expr))

(defn iexp
  "(iexp int), int-expr for the given integer int."
  [int]
  (IntConstant/constant int))

;; Declarations

(defn decls
  "(decls decl & more-decls), the combined decls from the arguments."
  [decl & more-decls]
  (reduce #(.and ^Decls %1 ^Decls %2) decl more-decls)) 

(defn decl
  "(decl variable)      -> (decl variable UNIV :one), or
   (decl variable expr) -> (decl variable expr :one), or
   (decl variable expr :multiplicity)
                        -> decl
   for the variable and the expression and the
   multiplicity :one (default), :lone, :some, or :set."
  ([^Variable variable]
    (decl variable UNIV :one))
  ([^Variable variable ^Expression expr]
    (decl variable expr :one))
  ([^Variable variable ^Expression expr multiplicity]
    (case multiplicity
      :one  (.oneOf  variable expr)
      :lone (.loneOf variable expr)
      :some (.someOf variable expr)
      :set  (.setOf  variable expr))))

