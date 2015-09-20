(ns algebra.models.group
  (:refer-clojure :exclude [count])
  (:require [algebra.signature.group :as g]
            [algebra.signature.group.n0 :as g0]
            [algebra.signature.group.n1 :as g1]
            algebra.struct ; deftypes for implementation struct types
            [algebra.struct.default :as default]
            [algebra.struct.n0 :as n0]
            [algebra.struct.n1 :as n1]
            [algebra.struct.d0 :as d0]
            [algebra.struct.d1 :as d1]
            [algebra.struct.keyword :as kw]
            [clojure.tools.logging :as log :only [debug info]]
            ))

(println "loading algebra.models.group")

;; an 'extend' expression specifies a model - a mapping from signature
;; to structure.  specifically it extends the signature of a type to
;; include some new operations, and maps the new ops to structure
;; ops (i.e. implementations).

;; default model
(extend java.lang.Object
  g/OpSigGroup
  {:name (fn [_] "Default group (Z,0,+) with java.lang.Object")
   :struct-kw default/struct-kw
   :** default/mult
   :id default/id
   :id? default/id?})

;; parameterized models - all calls to these ops will be parameterized
;; by a type datum.  e.g. instead of (add a b), (add t a b).  the only
;; purpose of the type parameter is to determine dispatch-by-type -
;; its value is not used.

(extend algebra.struct.N0
  g/OpSigGroup
  {:name n0/group-name
   :struct-kw n0/struct-kw
   :** n0/mult
   :id n0/id
   :id? n0/id?})

(extend algebra.struct.N1
  g/OpSigGroup
  {:name n1/group-name
   :struct-kw n1/struct-kw
   :** n1/mult
   :id n1/id
   :id? n1/id?})

;;;;;;;;;;;;;;;; other stuff
(extend java.lang.Long
  g0/OpSigGroupN0
  {:** n0/mult
   ;; :id n0/id
   :id? n0/id?}
  g1/OpSigGroupN1
  {:** n1/mult
   ;; :id n1/id
   :id? n1/id?})

(extend java.lang.Double
  g0/OpSigGroupN0
  {:** d0/mult
   ;; :id d0/id
   :id? d0/id?}
  g1/OpSigGroupN1
  {:** d1/mult
   ;; :id d1/id
   :id? d1/id?})

(extend clojure.lang.Keyword
  g/OpSigGroup
  {:** kw/mult
   ;; :id kw/id
   :id? kw/id?})
