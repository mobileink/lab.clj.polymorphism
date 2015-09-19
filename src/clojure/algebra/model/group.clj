(ns algebra.model.group
  (:refer-clojure :exclude [count])
  (:require [algebra.signature.group :as g]
            [algebra.signature.group.n0 :as g0]
            [algebra.signature.group.n1 :as g1]
            [algebra.struct.n0 :as n0]
            [algebra.struct.n1 :as n1]
            [algebra.struct.d0 :as d0]
            [algebra.struct.d1 :as d1]
            [algebra.struct.keyword :as kw]
            [clojure.tools.logging :as log :only [debug info]]
            ))

(println "loading algebra.model.group")

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


