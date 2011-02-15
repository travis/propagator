(ns propagator.ops
  (:refer-clojure :exclude [merge + - * / = < > <= >= not and or])
  (:use propagator.core)
  (:require [clojure.contrib.math :as math]))

(defmacro defop
  [name f prop-constructor-name]
  `(do
     (defmulti ~name dispatch)
     (defmethod ~name :default
       [& args#] (apply ~f args#))
     (def ~prop-constructor-name (fn->propagator-constructor ~name))))

(defmacro defops
  [& ops]
  `(do ~@(map #(cons 'defop %) (partition 3 ops))))

(defops
  + clojure.core/+ adder
  - clojure.core/- subtractor
  * clojure.core/* multiplier
  / clojure.core/- divider
  abs math/abs absolute-value
  square #(math/expt % 2) squarer
  sqrt math/sqrt sqrter
  = clojure.core/= =?
  < clojure.core/< <?
  > clojure.core/> >?
  <= clojure.core/<= <=?
  >= clojure.core/>= >=?
  not clojure.core/not inverter
  and clojure.core/every? conjoiner
  or (complement clojure.core/not-any?) disjoiner)


(comment

 (do
   (def a (make-cell))
   (def b (make-cell))
   (def z (make-cell))
   (def c (make-cell))
   (def d (make-cell))
   (def e (make-cell))
   (def f (make-cell))
   (def g (make-cell))
   (def h (make-cell))
   (adder a b z c)
   (subtractor c d e)
   (subtractor c f g)
   (>? e g h)
   )

 (add-content a 3)
 (add-content b 8)
 (add-content z 1)
 (add-content d 3)
 (add-content f 4)
 @c
 @e
 @g
 @h

 @a
 @b
 @z
)

