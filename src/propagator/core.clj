(ns propagator.core
  (:refer-clojure :exclude [merge]))

(def dispatch (fn [& args] (vec (map class args))))
(def contradiction ::contradiction)

(defmulti merge dispatch)
(defmethod merge :default
  [content increment]
  (if (= content increment)
    content
    contradiction))

(defn contradictory?
  [x]
  (= contradiction x))

(def nothing? nil?)


(defmethod merge [Object nil]
  [content _]
  content)

(defmethod merge [nil Object]
  [_ increment]
  increment)


(defn make-cell
  []
  (agent nil
         :validator (complement contradictory?)
         :error-mode :continue))

(defn new-neighbor!
  [cell neighbor]
  (add-watch cell :change neighbor))

(defn add-content
  [cell increment]
  (send cell merge increment))

(defn propagator
  "Set up watchers and trigger initial 'change' for each neighbor"
  [neighbors to-do]
  (doseq [neighbor neighbors]
    (add-watch neighbor (keyword (str (hash neighbors) "->" (hash neighbor)))
               (fn [cell key old new] (if (not (= old new)) (to-do))))
    (send neighbor identity)))

(defn lift-to-cell-contents
  [f]
  (fn [& args]
    (if (not-any? nothing? args)
      (apply f args)
      nil)))

(defn fn->propagator-constructor
  [f]
  (fn [& cells]
    (let [output (last cells)
          inputs (butlast cells)
          lifted-f (lift-to-cell-contents f)]
      (propagator
       inputs
       #(add-content output
                     (apply lifted-f (map deref inputs)))))))


