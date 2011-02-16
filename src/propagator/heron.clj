(ns propagator.heron
  "Heron's method for approximating a square root"
  (:refer-clojure :exclude [merge + - * / = < > <= >= not and or])
  (:use propagator.core)
  (:use propagator.ops))

 (defn heron-step
   [x g h]
   (compound-propagator
    [x g]
    #(let [xdg (make-cell)
           g+xdg (make-cell)
           two (make-cell)]
       (divider x g xdg)
       (adder g xdg g+xdg)
       ((constant 2) two)
       (divider g+xdg two h))))

(comment
 (do
    (def x (make-cell))
    (def guess (make-cell))

    (def better-guess (make-cell))
    (heron-step x guess better-guess))

  (add-content x 2)
  (add-content guess 1.4)
  @better-guess)

(defn good-enough?
  [g x done]
  (compound-propagator
   [g x]
   #(let [g2 (make-cell)
          eps (make-cell)
          x-g2 (make-cell)
          ax-g2 (make-cell)]
      ((constant 0.00000001) eps)
       (multiplier g g g2)
       (subtractor x g2 x-g2)
       (absolute-value x-g2 ax-g2)
       (<? ax-g2 eps done))))

(comment
  (do
    (def g (make-cell))
    (def x (make-cell))
    (def done (make-cell))
    (good-enough? g x done))

  (add-content g 1.4142135623746899)
  (add-content g 1.414)
  (add-content x 2)
  @done
  )

 (defn sqrt-iter [x g answer]
   (compound-propagator
    [x g]
    #(let [done (make-cell)
           not-done (make-cell)
           x-if-not-done (make-cell)
           g-if-not-done (make-cell)
           new-g (make-cell)]
       (good-enough? g x done)
       (switch done g answer)
       (inverter done not-done)
       (switch not-done x x-if-not-done)
       (switch not-done g g-if-not-done)
       (heron-step x-if-not-done g-if-not-done new-g)
       (sqrt-iter x-if-not-done new-g answer)
   )))

(comment
  (do
    (def g (make-cell))
    (def x (make-cell))
    (def answer (make-cell))
    (sqrt-iter x g answer))
  (add-content g 1)
  (add-content x 2)
  (float @answer))


(defn sqrt-network [x answer]
   (compound-propagator
    [x]
    #(let [one (make-cell)]
       ((constant 1) one)
       (sqrt-iter x one answer))))

(comment
 (def x (make-cell))
 (def answer (make-cell))
 (sqrt-network x answer)
 (add-content x 2)
 (float @answer)
)
