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

;; Conjunction
(defn and
  "(and & fmls) -> fml, the conjunction of the given fmls "
  [& fmls]
  (Formula/and ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

;; Disjunction
(defn or
  "(or & fmls) -> fml, the disjunction of the given fmls"
  [& fmls]
  (Formula/or ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

;; Equivalence
(defn iff
  "(iff  fml1 fml2) -> fml, the equivalence of the given fmls"
  [^Formula fml1 ^Formula fml2]
  (.iff fml1 fml2))

;; Implication
(defn impl
  "(impl  fml1 fml2) -> fml, the implication fml1 -> fml2."
  [^Formula fml1 ^Formula fml2]
  (.implies fml1 fml2))

;; Negation
(defn not
  "(not  fml) -> fml, the negation of fml."
  [^Formula fml]
  (.not fml))

;; Truth
(def TRUE 
  "TRUE -> the constant fml TRUE."
  Formula/TRUE)

;; The Contradiction
(def FALSE 
  "FALSE -> the constant fml FALSE."
  Formula/FALSE)

;; subset
(defn in 
  "(in expr1 expr2) -> fml, the formula expressing whether expr1 is a subset of expr2."
  [^Expression expr1 ^Expression expr2]
  {:pre (= (. expr1 arity) (. expr2 arity))}
  (.in expr1 expr2))

;; Equality
(defmulti eq
  "Usage: (eq expr1 expr2) -> fml, the formula expressing whether expr1 equals expr2.
   The types of expr1 and expr1 are Expression or IntExpression."
   (fn [e1 e2] [(class e1) (class e2)] ))

(defmethod eq [Expression Expression]
  [^Expression expr1  ^Expression expr2]
  (.eq expr1 expr2)) 

(defmethod eq [IntExpression IntExpression]
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.eq int-expr1 int-expr2))

;; Cardinality at least 1
(defn lone
  "(lone expr) -> fml, the formula expressing whether expr has at least one tuple."
  [^Expression expr]
  (.lone expr))

;; Cardinality exactly 1
(defn one
  "(one expr) -> fml, the formula expressing whether expr has exactly one tuple."
  [^Expression expr]
  (.one expr))

;; Cardinality >= 0
(defn some
  "(some expr) -> fml, the formula expressing whether expr has 0..* tuples, or
   (some decls fml) -> fml, the existential quantified formula."
  ([^Expression expr]
  (.some expr))
  ([^Decls decls ^Formula fml]
  (.forSome fml decls)))  

;; Cardinality 0
(defn no
  "(no expr) -> fml, the formula expressing whether expr is empty."
  [^Expression expr]
  (.no expr))

;; Total ordering
(defn total-ordering
  "(total-ordering rel ordered first last) -> fml,
   a formula expressing that rel imposes a total ordering over
   the unary relation (set) ordered where the first and last element 
   are given by the elements first and last resp."
  [^Relation rel ordered first last]
  {:pre (= (. rel arity) 2)} 
  (.totalOrder rel ordered first last))

;; Function
(defn function 
  "(function rel domain range) -> fml,
   a formula expressing that rel is a function from expr domain to expr range."
  [^Relation rel domain range]
  {:pre (= (. rel arity) 2)} 
  (.function rel domain range))

;; Partial function
(defn partial-function 
  "(partial-function rel domain range) -> fml
   a formula expressing that rel is a partial function from expr domain to expr range."
  [^Relation rel domain range]
  {:pre (= (. rel arity) 2)} 
  (.partialFunction rel domain range))

;; acyclic
(defn acyclic 
  "(acyclic rel) -> fml, a formula expressing that rel is acyclic." 
  [^Relation rel]
  {:pre (= (. rel arity) 2)} 
  (.acyclic rel))

;; Less than
(defn lt  
  "(lt int-expr1 int-expr2) -> fml, expressing whether int-expr1 < int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.lt int-expr1 int-expr2))

;; Less than or equal
(defn lte   
  "(lte int-expr1 int-expr2) -> fml, expressing whether int-expr1 <= int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.lte int-expr1 int-expr2))

;; Greater than
(defn gt  
  "(gt int-expr1 int-expr2) -> fml,  expressing whether int-expr1 > int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.gt int-expr1 int-expr2))

;; Gerater than or equal
(defn gte   
  "(gte int-expr1 int-expr2) -> fml, expressing whether int-expr1 >= int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.gte int-expr1 int-expr2))

;; Universal quantifier
(defn all   
  "(all decls fml) -> fml, universal quantifier over decls and fml."
  [^Decls decls ^Formula fml]
  (.forAll fml decls))


;; Expressions denoting the manipulation of values of relational variables

;; Intersection
(defn intersection
  "(intersection expr & more-exprs) -> expr, the intersection of the arguments."
  [expr & more-exprs]
  (Expression/intersection ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

;; Override
(defn override 
  "(override expr & more-exprs) -> expr, the override of the arguments."
  [expr & more-exprs]
  (Expression/override ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

;; Cartesian Product
(defn product 
  "(product expr & more-exprs) -> expr, the cartesian product of the arguments."
  [expr & more-exprs]
  (Expression/product ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

;; Union
(defn union 
  "(union expr & more-exprs) -> expr, the union of the arguments."
  [expr & more-exprs]
  (Expression/union ^"[Lkodkod.ast.Expression;" (into-array (cons expr more-exprs))))

;; Difference
(defn difference 
  "(difference expr1 expr2) -> expr, the difference of the arguments."
  [^Expression expr1 ^Expression expr2]
  (.difference expr1 expr2))

;; Dotjoin
(defn dotjoin 
  "(dotjoin expr1 expr2) -> expr, the dotjoin of the arguments."
  [^Expression expr1 ^Expression expr2]
  (.join expr1 expr2))

;; Transitive closure of a binary expression
(defn tclose 
  "(tclose expr) -> expr, the transitive closure of expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.closure expr))

;; Reflexive transitive closure of a binary expression
(defn rtclose 
  "(rtclose expr) -> expr, the reflexive transitive closure of expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.reflexiveClosure expr))

;; Transposition
(defn transpose 
  "(transpose expr) -> expr, the transpose of expr."
  [^Expression expr]
  {:pre (= (. expr arity) 2)} 
  (.transpose expr))

;; Projection
(defn project 
  "(project expr int-expr & more-int-exprs) -> expr, 
   the projections of expr on the columns given by the int-expr arguments."
  [^Expression expr int-expr & more-int-exprs]
  (.project expr (into-array (cons int-expr more-int-exprs))))

;; Unary empty relation
(def NONE
  "NONE -> the unary relation without tuples." 
   Expression/NONE)

;; Universal relation
(def UNIV
  "UNIV -> the unary relation containing one-tuples of all atoms of the universe." 
   Expression/UNIV)

;; Identity relation
(def IDEN
  "IDEN -> the binary relation containing pairs of all identical atoms of the universe." 
   Expression/IDEN)

;; Integers as relation
(def INTS
  "INTS -> the unary relation containing all atoms bound to integers." 
   Expression/INTS)

;; If then else
(defmulti ite
  "Usage: (ite fml expr1 expr2) -> expr
          (ite fml int-expr1 int-expr1) -> int-expr
   Returns the second argument if fml is true, the third otherwise."
   (fn [fml expr1 expr2] (class expr1)) )

(defmethod ite Expression 
  [^Formula fml ^Expression expr1 ^Expression expr2]
  (.thenElse fml expr1 expr2))

(defmethod ite IntExpression 
  [^Formula fml ^IntExpression int-expr1 ^IntExpression int-expr2]
  (.thenElse fml  int-expr1 int-expr2))

;; Cast from integer
(defn int-cast
  "(int-cast int-expr) -> expr, a relational expression of the given int-expr."
  [^IntExpression int-expr]
  (.toExpression int-expr))

;; Cast from bitset
(defn bitset-cast
  "(bitset-cast int-expr) -> expr, an expression whose meaning 
   is the set containing the atoms 
	 that represent the powers of 2 (bits) present in the int-expr."
  [^IntExpression int-expr]
  (.toBitset int-expr))

;; Comprehension
(defn comprehension
  "(comprehension decls fml) -> expr, the expression defined by decls and fml."
  [^Decls decls ^Formula fml]
  (.comprehension fml decls))

;; Variables used in expression with quantifiers

;; Variable
(defn variable
  "(variable :var-name) -> unary var or
   (variable :var-name arity) -> var of the given arity."
  ([var-name]
    (Variable/unary (name var-name)))
  ([var-name arity]
    (Variable/nary (name var-name) arity)))

;; Integer expressions

;; +
(defn plus 
  "(plus int-expr & more-int-exprs) -> int-expr, the sum of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/plus ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

;; *
(defn mult 
  "(mult int-expr & more-int-exprs) -> int-expr, the product of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/multiply ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

;; bitwise and
(defn bit-and 
  "(bit-and int-expr & more-int-exprs) -> int-expr, the bitwise and of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/and ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

;; bitwise or
(defn bit-or 
  "(bit-or int-expr & more-int-exprs) -> int-expr, the bitwise or of the arguments."
  [int-expr & more-int-exprs]
  (IntExpression/or ^"[Lkodkod.ast.IntExpression;" (into-array (cons int-expr more-int-exprs))))

;; shift left
(defn sh-left 
  "(sh-left int-expr1 int-expr2) -> int-expr for int-expr1 << int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.shl int-expr1 int-expr2))

;; shift right with sign
(defn sh-right-sign 
  "(sh-right-sign int-expr1 int-expr2) -> int-expr for int-epr1 >>> int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.sha int-expr1 int-expr2))

;; shift right zero
(defn sh-right-zero 
  "(sh-right-zero int-expr1 int-expr2) -> int-expr for int-expr1 >> int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.shr int-expr1 int-expr2))

;; -
(defn minus 
  "(minus int-expr1 int-expr2) -> int-expr for int-expr1 - int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.minus int-expr1 int-expr2))

;; /
(defn div 
  "(div int-expr1 int-expr2) -> int-expr for int-expr1 / int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.divide int-expr1 int-expr2))

;; mod
(defn mod 
  "(mod int-expr1 int-expr2) -> int-expr for int-expr1 % int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.modulo int-expr1 int-expr2))

;; bitwise xor
(defn bit-xor 
  "(bit-xor int-expr1 int-expr2) -> int-expr for int-expr1 xor int-expr2."
  [^IntExpression int-expr1 ^IntExpression int-expr2]
  (.xor int-expr1 int-expr2))

;; negation of integer expression
(defn neg
  "(neg int-expr) -> int-expr for the negation of the argument."
  [^IntExpression int-expr]
  (.negate int-expr))

;; bitwise not
(defn bit-not
  "(bit-not int-expr) -> int-expr for the bitwise negation of the argument."
  [^IntExpression int-expr]
  (.not int-expr))

;; abs
(defn abs
  "(abs int-expr) -> int-expr for the absolute value of the argument."
  [^IntExpression int-expr]
  (.abs int-expr))

;; Signum
(defn sgn
  "(sgn int-expr) -> int-expr for the signum function of the argument."
  [^IntExpression int-expr]
  (.signum int-expr))

;; sum
(defmulti sum
  "(sum decls int-expr) -> int-expr of the sum of all values int-expr 
                           can have for the given decls,
   (sum expr) -> int-expr of the sum of the integer atoms in expr."
  (fn [first & more] (class first)))

(defmethod sum Decls
  [^Decls decls ^IntExpression int-expr]
  (.sum int-expr decls))

(defmethod sum Expression
  [^Expression expr]
  (.sum expr))

;; count
(defn count
  "(count expr) -> int-expr of the number of tuples in expr."
  [^Expression expr]
  (.count expr))

;; integer as expression
(defn iexp
  "(iexp int) -> int-expr for the given integer int."
  [int]
  (IntConstant/constant int))

;; Declarations

(defn decls
  "(decls decl & more-decls) -> decls, the combined decls from the arguments."
  [decl & more-decls]
  (reduce #(.and ^Decls %1 ^Decls %2) decl more-decls)) 

(defn decl
  "(decl variable) -> (decl variable UNIV :one), or
   (decl variable expr) -> (decl variable expr :one), or
   (decl variable expr :multiplicity) -> decl 
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
