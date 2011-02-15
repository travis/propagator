# propagator

Implementation of Propagators from [The Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215)

## Usage

    user> (use 'propagator.core 'propagator.ops)
    user> (do
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
       (add-content a 3)
       (add-content b 8)
       (add-content z 1)
       (add-content d 3)
       (add-content f 4))

    user> @c
    12
    user> @e
    9
    user> @g
    8
    user> @h
    true

## License

Copyright (C) 2010 Travis Vachon

Distributed under the Eclipse Public License, the same as Clojure.


