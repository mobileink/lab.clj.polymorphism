(ns rsch.test
  (:require [rsch.sig :as s]
            [clojure.repl :refer :all]
            [clojure.reflect :refer :all]
            [clojure.pprint :refer :all]
            [clojure.tools.namespace :refer :all]
            [clojure.tools.logging :as log :only [debug info]]))

(deftype N0 [])

(s/declare-signature! Magma
                      "I am the Magma docstring"
                      :opt1 "magma option 1"
                      :opt2 "magma option 2"
                      :ns a.b
                      ;; universe restriction allows us to express dependent types; e.g. type of Nat > n
                      :universe {:sym 'T, :restriction (fn [a] (> a 3))}
                      ;; :constants #{:foo}
                      :operators [(* [a b] "magma binop")]
                      :laws {:closure f}) ;; expresses: for all fns f and vals x, (f x...) in U

(pprint a.b/Magma)

(s/declare-signature! Monoid
                      "I am the Monoid docstring"
                      :expand [a.b/Magma]
                      :opt1 "monoid option 1"
                      :opt2 "monoid option 2"
                      :ns a.b
                      :universe {:sym 'U, :restriction (fn [a] (> a 3))}
                      :constants #{:e}
                      :operators [(mon1 [a] "monoid operation f1")]
                      :laws {:idenity #(= (mon1 % e) %)})

(pprint a.b/Monoid)

(s/declare-signature! Group
                      "I am the Group docstring"
                      :expand [a.b/Monoid]
                      :opt1 "group option 1"
                      :ns a.b
                      :universe {:sym 'G, :restriction pos?}
                      ;; :constants #{:f}
                      :operators [(g1 [a] "group operation f1")]
                      :laws {:inverse #(= (* % (inv %)) e)})

(pprint a.b/Group)
(pprint a.b/Monoid)
(pprint a.b/Magma)

;; {:method-map {:* :*},
;;  :opt2 "magma option 2",
;;  :opt1 "magma option 1",
;;  :on-interface a.b.Magma,
;;  :var #'a.b/Magma,
;;  :laws {:closure f},
;;  :on 'a.b.Magma,
;;  :method-builders
;;  {#'a.b/*
;;   #object[rsch.test$eval11308$fn__11313 0x2f60b3bb "rsch.test$eval11308$fn__11313@2f60b3bb"]},
;;  :universe {:sym 'T, :restriction (fn [a] (> a 3))},
;;  :sigs {:* {:name *, :arglists ([a b]), :doc "magma binop"}},
;;  :constants #{:foo}}

;; with-model? by-model? using-model?
(s/define-model! foo.bar/MagN0+ :for a.b/Magma
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :operators {:* +})

(pprint foo.bar/MagN0)

(s/define-model! foo.bar/MonN0+ :for a.b/Monoid
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0}
  :operators {:* +, :mon1 /})

(pprint foo.bar/MonN0+)

(s/define-model! foo.bar/MonN1* :for a.b/Monoid
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction pos?}}
  :constants {:e 1}
  :operators {:* *, :mon1 /})

(pprint foo.bar/MonN1*)

(s/define-model! foo.bar/GrpN0+ :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0}
  :operators {:* +, :mon1 identity, :g1 identity})

(pprint foo.bar/GrpN1*)

(s/define-model! foo.bar/GrpN1* :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction pos?}}
  :constants {:e 1}
  :operators {:* *, :mon1 identity, :g1 identity})

(pprint foo.bar/GrpN1*)

;; FIXME:  is the modulus a const of the sig? or part of the operation semantics
(s/define-model! foo.bar/GrpMod3-N0+ :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0}
  :operators {:* (fn [a b] (mod (+ a b) 3))
              :mon1 identity, :g1 identity})

(pprint foo.bar/GrpMod3-N0+)

(s/define-model! foo.bar/GrpMod3-N1* :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction pos?}}
  :constants {:e 1}
  :operators {:* (fn [a b] (mod (* a b) 3))
              :mon1 identity, :g1 identity})

(pprint foo.bar/GrpMod3-N1*)

 (s/with-model foo.bar/GrpN0+ ;; :for a.b/Group
   (* 3 4)) ; => 7

 (s/with-model foo.bar/GrpN1* ;; :for a.b/Group
   (* 3 4)) ; => 12

 (s/with-model foo.bar/GrpMod3-N0+ ;; :for a.b/Group
   (* 3 4)) ; => 1

 (s/with-model foo.bar/GrpMod3-N1* ;; :for a.b/Group
   (* 3 4)) ; => 0
