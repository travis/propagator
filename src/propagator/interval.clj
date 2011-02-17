(ns propagator.interval
  (:refer-clojure :exclude [empty?])
  (:require [clojure.core :as c]
           [propagator.core :as p]
           [propagator.ops :as o]
           [clojure.contrib.math :as math]))

(def interval-=)

(deftype Interval [low high]
  Object
  (equals [x y] (interval-= x y))
  (toString [x] (str "[" (.low x) "," (.high x) "]")))

(defn interval-=
  [x y]
  (if (instance? Interval y)
    (and (= (.low x) (.low y)) (= (.high x) (.high y)))
    false))

(defn create [low high]
  "Useful for use in (require '[propagator.interval :as interval]"
  (Interval. low high))

(defmethod o/* [Interval Interval]
    [x y]
  (create (* (.low x) (.low y))
          (* (.high x) (.high y))))

(defmethod o/div [Interval Interval]
  [x y]
  (o/* x (create (/ 1 (.high y))
                 (/ 1 (.low y)))))

(defmethod o/square [Interval]
  [x]
  (create (* (.low x) (.low x))
          (* (.high x) (.high x))))

(defmethod o/sqrt [Interval]
  [x]
  (create (math/sqrt (.low x))
          (math/sqrt (.high x))))

(defn empty? [x]
  (> (.low x) (.high x)))

(defn intersect [x y]
  (create (max (.low x) (.low y))
          (min (.high x) (.high y))))

(defmethod p/merge [Interval Interval]
  [content increment]
  (let [new-range (intersect content increment)]
    (cond (= new-range content) content
          (= new-range increment) increment
          (empty? new-range) p/contradiction
          :else new-range)))

(defn ensure-inside
  [interval number]
  (if (<= (.low interval) number (.high interval))
    number
    p/contradiction))

(defmethod p/merge [Interval Number]
  [content increment]
  (ensure-inside content increment))

(defmethod p/merge [Number Interval]
  [content increment]
  (ensure-inside increment content))

(defmethod o/* [Number Interval]
  [content increment]
  (o/* (create content content) increment))

(defmethod o/* [Interval Number]
  [content increment]
  (o/* content (create increment increment)))

(defmethod o/div [Number Interval]
  [content increment]
  (o/div (create content content) increment))

(defmethod o/div [Interval Number]
  [content increment]
  (o/div content (create increment increment)))



(comment
  (defn fall-duration [t h]
    (o/compound-propagator
     [t]
     #(let [g (p/make-cell)
            one-half (p/make-cell)
            t2 (p/make-cell)
            gt2 (p/make-cell)]
        ((o/constant (create 9.789 9.832)) g)
        ((o/constant (create 1/2 1/2)) one-half)
        (o/squarer t t2)
        (o/multiplier g t2 gt2)
        (o/multiplier one-half gt2 h))))

  (defn similar-triangles [s-ba h-ba s h]
    (o/compound-propagator
     [s-ba h-ba s]
     #(let [ratio (p/make-cell)]
        (o/divider h-ba s-ba ratio)
        (o/multiplier s ratio h))))

  (do
    (def building-height (p/make-cell))

    (def fall-time (p/make-cell))
    (fall-duration fall-time building-height)

    (def baro-height (p/make-cell))
    (def baro-shadow (p/make-cell))
    (def building-shadow (p/make-cell))
    (similar-triangles baro-shadow baro-height
                       building-shadow building-height)
    )

  (p/add-content fall-time (create 2.9 3.1))


  (p/add-content building-shadow (create 54.9 55.1))
  (p/add-content baro-height (create 0.3 0.32))
  (p/add-content baro-shadow (create 0.36 0.37))
  @building-height

  ;;multi directional

  (defn md-fall-duration [t h]
    (o/compound-propagator
     [t]
     #(let [g (p/make-cell)
            one-half (p/make-cell)
            t2 (p/make-cell)
            gt2 (p/make-cell)]
        ((o/constant (create 9.789 9.832)) g)
        ((o/constant (create 1/2 1/2)) one-half)
        (o/quadratic t t2)
        (o/product g t2 gt2)
        (o/product one-half gt2 h))))

  (defn md-similar-triangles [s-ba h-ba s h]
    (o/compound-propagator
     [s-ba h-ba s]
     #(let [ratio (p/make-cell)]
        (o/product s-ba ratio h-ba)
        (o/product s ratio h))))

  (do
    (def building-height (p/make-cell))

    (def fall-time (p/make-cell))
    (md-fall-duration fall-time building-height)

    (def baro-height (p/make-cell))
    (def baro-shadow (p/make-cell))
    (def building-shadow (p/make-cell))
    (md-similar-triangles baro-shadow baro-height
                          building-shadow building-height)
    )

  (p/add-content fall-time (create 2.9 3.1))


  (p/add-content building-shadow (create 54.9 55.1))
  (p/add-content baro-height (create 0.3 0.32))
  (p/add-content baro-shadow (create 0.36 0.37))
  @building-height
  @baro-height
  @fall-time

  ;; bribe the super!

  (p/add-content building-height 45)

  @baro-height
  @baro-shadow
  @building-shadow
  @fall-time


  ;; increments and numbers
  (do
    (def fall-time (p/make-cell))
    (def building-height (p/make-cell))
    (md-fall-duration fall-time building-height))
  (p/add-content fall-time (create 2.9 3.1))
  @building-height

  (p/add-content building-height 45)
  @fall-time

  )


