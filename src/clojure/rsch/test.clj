(ns rsch.test
  (:require [rsch.sig :as s]
            [clojure.repl :refer :all]
            [clojure.reflect :refer :all]
            [clojure.pprint :refer :all]
            [clojure.tools.namespace :refer :all]
            [clojure.tools.logging :as log :only [debug info]]))

(s/declare-signature! Magma
                      "I am the Magma docstring"
                      :opt1 "magma option 1"
                      :opt2 "magma option 2"
                      ; meta:
                      :ns a.b
                      ;; universe restriction allows us to express dependent types; e.g. type of Nat > n
                      :universe {:sym 'T, :restriction (fn [a] (> a 3))}
                      :laws {:closure f} ;; expresses: for all x, (f1 x) in U, (f2 x y) in U
                      ;; :constants and :operators must come last
                      {:constants #{:e}
                      ;; :operators {:f1 [a], :f2 [a b]}
                      :operators [(mag1 [a] "magma operation f1")
                                  (mag2 [a b] "magma operation f2")]})

(s/declare-signature! Monoid
                      "I am the Monoid docstring"
                      :expand [a.b/Magma]
                      :opt1 "monoid option 1"
                      :opt2 "monoid option 2"
                      ; meta:
                      :ns a.b
                      ;; universe restriction allows us to express dependent types; e.g. type of Nat > n
                      :universe {:sym 'T, :restriction (fn [a] (> a 3))}
                      :laws {:idenity #()}
                      ;; :constants and :operators must come last
                      {:constants #{:e :x}
                      ;; :operators {:f1 [a], :f2 [a b]}
                      :operators [(mon1 [a] "monoid operation f1")
                                  (mon2 [a b] "monoid operation f2")]})

(pprint a.b/Monoid)
(pprint a.b/Magma)

(import 'a.b.Magma)

(type a.b.Magma)

;;(refresh)

(pprint (reflect a.b.Magma))

(meta #'a.b/f1)

(:method-builders a.b/Magma)
(:method-map a.b/Magma)
(:sigs a.b/Magma)

(var a.b/f1)

(type (:ns a.b/Magma))

(pprint a.b/Monoid)

(:reduct a.b/Monoid)
(:method-map a.b/Monoid)


(defprotocol P
  "protocol docstring"
  :foo "this is an option"
    (foo [this]  "foo docstring")
    (bar-me [this] [this y]))

P
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
