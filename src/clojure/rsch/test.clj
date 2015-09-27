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
                      :operators [(mon1 [a] "monoid operation f1")
                                  (mon2 [a b] "monoid operation f2")]
                      :laws {:idenity #(= (mon1 % e) %)})

(pprint a.b/Monoid)

(s/declare-signature! Group
                      "I am the Group docstring"
                      :expand [a.b/Monoid]
                      :opt1 "group option 1"
                      :ns a.b
                      :universe {:sym 'G, :restriction pos?}
                      ;; :constants #{:f}
                      :operators [(g1 [a] "group operation f1")
                                  (g2 [a b] "group operation f2")]
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
(s/define-model! foo.bar/MonN0 :for a.b/Monoid
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0 :foo 9}
  :operators {:* '+})

(pprint foo.bar/MonN0)
(pprint foo.bar/MagN0)

(s/define-model! foo.bar/GrpN0+ :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0}
  :operators {:* +})

(pprint foo.bar/GrpN0+)

(s/define-model! foo.bar/GrpN0* :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 1}
  :operators {:* *})

(pprint foo.bar/GrpN0*)

;; FIXME:  is the modulus a const of the sig? or part of the operation semantics
(s/define-model! foo.bar/GrpMod3 :for a.b/Group
  :universe {:sym :Nat ;; would be a type if we had genuine types
             :impl {:type java.lang.Long ; implementation type
                    :restriction #(> % -1)}}
  :constants {:e 0}
  :operators {:* (fn [a b] (mod (+ a b) 3))})

(pprint foo.bar/GrpMod3)

(let [* (:* (:operators foo.bar/GrpMod3))
      e 0]
  (pprint (* 3 e)))

(pprint a.b.N0)

(import 'a.b.Magma)

(type a.b.Magma)

;;(refresh)

(pprint (reflect a.b.Magma))

(meta #'a.b/g1)

(:method-builders a.b/Magma)
(:method-map a.b/Magma)
(:sigs a.b/Magma)

(var a.b/f1)

(type (:ns a.b/Magma))

(pprint a.b/Monoid)

(:reduct a.b/Monoid)
(:method-map a.b/Monoid)

;; (s/model-signature Magma
;;   :name MagN1
;;   :ns foo.bar
;;   :universe {:sym :Nat ;; would be a type if we had genuine types
;;              :impl {:type java.lang.Long ; implementation type
;;                     :restriction pos?}}
;;   :constants {:e 1}
;;   :operators {:* '*})



(defprotocol P
  "protocol docstring"
  :foo "this is an option"
    (foo [this]  "foo docstring")
    (bar-me [this] [this y]))

P
(:method-builders (var P))

(pprint (reflect rsch.test.P))

(:sigs P)

;; (s/model-signature 'S
;;                    :name M
;;                    :ns foo.bar
;;                    :dom {:impl-type java.lang.Long
;;                          :rule pos?} ;; should this be a law on sig? it expresses a semantic property
;;                    :constants {:e 0 :x 9}
;;                    :operators {:f1 impl/foo :f2 #(fbar %)})


;;  (s/with-model foo.bar/M
;;    (* 3 4))
