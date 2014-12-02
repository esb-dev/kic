(ns kic.rel)

;; Representation of a relation: a set of vectors of a common arity.

(defn unary-rel 
  "Usage: (unary-rel coll)
   Returns a unary rel containing the elements of the collection coll."
  [coll]
  (set (map #(vector %) coll)))

(defn rel 
  "Usage: (rel vec-coll)
   Returns a rel consisting of the tuples vec-coll.

   Requires: All elements of vec-coll have the same arity."
  [vec-coll]
  (set vec-coll))

(defn crossjoin
  "Usage: (crossjoin & rels)
   Returns the cross join of the given rels."
  [& rels]
  (let [cross (fn cross [& seqs]
							  (when seqs
							    (if-let [s (first seqs)]
							      (if-let [ss (next seqs)]
							        (for [x  s
							              ys (apply cross ss)]
							          (reduce conj x ys))
							        s))))]
  (set (apply cross rels))))

(defn dotjoin
  "Usage: (dotjoin & rels)
   Returns the dot join of the given rels."
  [& rels]
  "to be implemented")

(defn join
  "Usage: (join & rels)
   Returns the join of the given rels."
  [& rels]
  "to be implemented")

(defn restrict
  "Usage: (restrict rel pred)

  Returns the restriction of the relation rel with respect
  to the given predicate. The predicate must be a function
  operating on a tuple and returning a boolean."
  [rel pred]
  (filter pred rel))

(defn proj 
  "Usage: (proj rel & idx)
   Returns the projection of rel on the given idxs"
  [rel & idx]
  "to be implemented")
; was ist mit (proj rel set-of-idxs)?

(defn transpose
  "Usage: (transpose bin-rel)
   Returns the transpose of the given binary relation."
  [bin-rel]
  "to be implemented")

(defn tclose
  "Usage: (tclose bin-rel)
   Returns the transitive closure of the given binary relation."
  [bin-rel]
  "to be implemented")

; use union from clojure.set

(defn intersection
  "Usage: (intersection & rels)
   Returns the intersection of the given rels which all have the same arity."
  [& rels]
  "to be implemented")

(defn difference
  "Usage: (difference rel1 rel2)
   Returns the difference of the rel1 and rel2 which both have the same arity."
  [rel1 rel2]
  "to be implemented")

(defn rsort
  "Usage: (rsort rel comparator)
   
   Returns the relation rel sorted using the given comparator."
  [rel comparator]
  "to be implemented")
; use core/sort or sort-by


(defn reflexive?
  "(reflexive? rel) -> boolean
   Returns whether the given binary rel is refelxive."
  [rel]
  "to be implemented")
; analog weitere Funktionen zum Check von Eigenschaften der Relationen