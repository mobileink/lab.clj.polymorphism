# lab.clj.protocols
Lab for experimenting with Clojure Protocols

## Group algebras

1. Fork/clone
2. ```lein repl```, then do ```refresh-all`` (see dev/user.clj for config stuff)

Then explore.  Three Groups are provided; using them as a pattern it
should be easy to implement your own.

* N0:  (N, 0, +), underlying set is Natural numbers with zero, identity element is 0, operation is addition
* N1:  (N, 1, \*), underlying set is positive Natural numbers, identity element is 1, operation is multiplication
* Q3+:  quotient group mod 3 over integers, identity is 0, op is addition

[source,clojure]
----
user => (register-struct (algebra.struct.N0.))
user => (register-struct (algebra.struct.N1.))
user => (register-struct (algebra.struct.Q3+.))
user => (set-model :n0)
user => (** 3 5)           ;; we use \*\* as the abstract Group multiplication operator
8
user => (set-model :n1)
user => (** 3 5)
15
user => (set-model :quotient-3)
user => (** 3 5)
2
----

## other stuff

The code in the `foo` subdir is a more basic example of protocols v. multimethods.


## the problem

