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

(ns ^{:doc    "A Clojure wrapper to the Kodkod constraint solver."
      :author "Burkhardt Renz THM"}
  kic.core
  (:refer-clojure :exclude [and or not some bit-and bit-or bit-xor bit-not mod
                            count])
  (:require [clojure.string :as str])
  (:import (kodkod.ast Relation Variable))
  (:import [kodkod.ast Formula ComparisonFormula])
  (:import [kodkod.ast Expression IntExpression IntConstant])
  (:import [kodkod.ast Decls Decl])
  (:import [kodkod.ast.operator ExprCompOperator])
  (:import [kodkod.instance Universe Bounds TupleFactory Tuple TupleSet])
  (:import [kodkod.engine Solver Solution])
  (:import [kodkod.engine.config Options])
  (:import [kodkod.engine.satlab SATFactory])
)           

(load "ef")

;; ## Structure of the world for the problem to be solved.

;; The structure is represented by a map with
;; :univ  the universe of objects in the world
;; key/value pairs for the relational variables

(defn- relvar-entry
  "(relvar-entry [key arity] -> [key Relation of arity and name (name key)]"
  [[key arity]]
   {:pre [(keyword? key), (integer? arity) (> arity 0)]}
   [key (Relation/nary (name key) arity)])

(defn structure
  "(structure atom-vec relvar-map) -> {:univ Universe, relkey1 relation1, ...}
   atom-vec is a vector of the atoms of the universe
   relvar-map consists of pairs of relvar-names and arities.

   Returns the structure of a 'world' for the problem to be solved.
   The 'world' is a map with keys
   :univ for the universe i.e. a kodkod.instance.Universe object,
   :relkey<n> for the relvar i.e. a kodkod.ast.Relation object for the relkey."
  [atom-vec relvar-map]
  {:pre [(vector? atom-vec), (clojure.core/not (empty? atom-vec))
         (map? relvar-map), (clojure.core/not (empty? relvar-map))]}
  (let [univ {:univ (Universe. atom-vec)}
        relvars (apply conj{} (map relvar-entry relvar-map))]
    (conj relvars univ)))

(defn universe
  "(universe structure) -> universe as a kodkod.instance.Universe object"
  [structure]
  (structure :univ))

(defn univ
  "(univ structure) -> universe as an unary relation"
  [structure]
  (let [univ (universe structure)]
    (set (map #(vector %) univ))))
 
(defn relvar
  "(relvar structure keyword) -> relvar as a kodkod.ast.Relation"
  [structure keyword]
  (structure keyword))

(defn- sat-factory
  "(sat-factory solver) -> SATFactory for the given solver"
  [solver]
  (cond
    (= solver "CryptoMiniSat") (SATFactory/CryptoMiniSat)
    (= solver "MiniSat") (SATFactory/MiniSat)
    (= solver "MiniSatProver") (SATFactory/MiniSatProver)
    (= solver "Glucose") (SATFactory/Glucose)
    (= solver "LightSAT4J") (SATFactory/LightSAT4J)
    (= solver "Lingeling") (SATFactory/Lingeling)
    :else (SATFactory/DefaultSAT4J)))
; remark:
; kic supports out of the box just SAT4J
; to use other SAT solvers, their libraries have to be
; added to the project

(defn options
  "(options option-map) -> options as a kodkod.engine.config.Options object"
  [{:keys [solver reporter symmetry-breaking sharing 
           int-encoding bit-width skolem-depth
           log-translation core-granularity]}]
  (let [opts (Options.)]
    (.setSolver opts (sat-factory solver))
    (if (clojure.core/not (nil? reporter))       
      (.setReporter opts reporter))
    (if (integer? symmetry-breaking)
      (.setSymmetryBreaking opts symmetry-breaking))
    (if (integer? sharing)          
      (.setSharing opts sharing))
    (if (clojure.core/not (nil? int-encoding))   
      (.setIntEncoding opts int-encoding))
    (if (integer? bit-width)        
      (.setBitwidth opts bit-width))
    (if (integer? skolem-depth)     
      (.setSkolemDepth opts skolem-depth))
    (if (integer? log-translation)  
      (.setLogTranslation opts log-translation))
    (if (integer? core-granularity) 
      (.setCoreGranularity opts core-granularity))
    opts))
;TODO better checking the parameters

(defn tuple
  "(tuple factory vec) -> a kodkod.instance.Tuple 
   constructed from the given vec using the given factory"
  [^TupleFactory factory vec]
  (.tuple factory vec))

(defn tupleset
  "(tupleset structure rel) -> a kodkod.instance.TupleSet
   constructed from the given rel over the given structure."
  [structure rel]
  (let [^Universe u (universe structure)
        ^TupleFactory factory (.factory u)]
    (.setOf factory (vec (map #(tuple factory %) rel)))))  

(defn bounds
  "(bounds structure bounds-def) -> a kodkod.instance.Bounds
   constructed from the bounds-def
   
   bounds-def is a map {:rel [lower, upper] ...}"
  [structure bounds-def]
  (let [b (Bounds. (universe structure))]
    (doseq [r bounds-def]
      (.bound b (relvar structure (key r))
                (tupleset structure (first (val r)))
                (tupleset structure (second (val r)))))
    b))

(defn solve
  "(solve formula bounds) -> a kodkod.engine.Solution
   (solve formula bounds options) -> a kodkod.engine.Solution"
  ([formula bounds]
    (solve formula bounds (options {})))
  ([formula bounds options]
    (let [solver (Solver. options)]
     (.solve solver formula bounds))))


;; kodkod.instance.Tuple -> vector
(defn vector-from-tpl
  "vector <- kodkod.instance.Tuple"
  [^Tuple tpl]
   (vec (for [i (range 0 (.arity tpl))] (.atom tpl i))))

;; kodkod.instance.TupleSet -> set of vectors
(defn vecset-from-ts
  "set of vectors <- kodkod.instance.TupleSet"
  [^TupleSet ts]
  (set (map vector-from-tpl ts))) 

;; kodkod's Map<Relation, TupleSet> -> map of (relation name, set of vectors)
(defn relmap-from-instmap
  [instmap]
 (let [keys (for [^Relation r (keys instmap)] (keyword (.name r))),
       vals (for [^TupleSet t (vals instmap)] (vecset-from-ts t))] 
   (zipmap keys vals)))

;; kodkod.engine.Solution -> keyword for outcome
(defn outcome
  "Outcome of kodkod's run."
  [^Solution solution]
  (keyword (str/lower-case (.toString (.outcome solution)))))


;; kodkod.engine.Solution -> vector of outcome, relmap
(defn model
  "vector of outcome and instance <- kodkod.engine.Solution"
  [^Solution solution]
  [(outcome solution) (relmap-from-instmap (.relationTuples (.instance solution)))])

