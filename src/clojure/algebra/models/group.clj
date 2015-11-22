(ns algebra.models.group
  (:refer-clojure :exclude [count])
  (:require [algebra.signature.meta :as ameta]
            [algebra.signature.group :as g]
            ;; [algebra.signature.group.n0 :as g0]
            ;; [algebra.signature.group.n1 :as g1]
            types.group ; deftypes for implementation struct types
            [algebra.structure.group.default :as default]
            [algebra.structure.group.n0 :as n0]
            ;; [algebra.structure.group.q1 :as n1]
            ;; [algebra.structure.group.q3 :as q3]
            ;; [algebra.structure.group.d0 :as d0]
            ;; [algebra.structure.group.d1 :as d1]
            ;; [algebra.structure.group.keyword :as kw]
            [clojure.tools.logging :as log :only [debug info]]
            ))

(println "loading algebra.models.group")

;; default model
(extend java.lang.Object
  ameta/AlgebraMeta {:name default/algebra-name
                 :structure default/structure
                 ;; :install! default/install!
                 ;; :activate! default/activate!
                 ;; :active-model? default/active-model?
                 ;; :active-model default/active-model
                 :constants default/constants}
  g/Operators {:** default/mult})

(extend types.group.N0+
  ameta/AlgebraMeta {:name n0/algebra-name
                 :structure n0/structure
                 ;; :install! n0/install!
                 ;; :activate! n0/activate!
                 ;; :active-model? n0/active-model?
                 ;; :active-model n0/active-model
                 :constants n0/constants}
  g/Operators {:** n0/mult})

;; (extend types.group.Q1*
;;   ameta/AlgebraMeta {:name n1/algebra-name
;;                  :structure n1/structure
;;                  :install! n1/install!
;;                  :activate! n1/activate!
;;                  :active-model? n1/active-model?
;;                  :active-model n1/active-model
;;                  :constants n1/constants}
;;   g/Operators {:** n1/mult})

;; (extend types.group.Q3+
;;   ameta/AlgebraMeta {:name q3/algebra-name
;;                  :structure q3/structure
;;                  :install! q3/install!
;;                  :activate! q3/activate!
;;                  :active-model? q3/active-model?
;;                  :active-model q3/active-model
;;                  :constants q3/constants}
;;   g/Operators {:** q3/mult})

;; ;;;;;;;;;;;;;;;; other stuff
;; (extend java.lang.Long
;;   g0/OpSigGroupN0
;;   {:** n0/mult}
;;    ;; :id n0/id
;;    ;; :id? n0/id?}
;;   g1/OpSigGroupN1
;;   {:** n1/mult}
;;    ;; :id n1/id
;;    ;; :id? n1/id?}
;;   )

;; (extend java.lang.Double
;;   g0/OpSigGroupN0
;;   {:** d0/mult}
;;    ;; :id d0/id
;;    ;; :id? d0/id?}
;;   g1/OpSigGroupN1
;;   {:** d1/mult})
;;    ;; :id d1/id
;;    ;; :id? d1/id?})

;; (extend clojure.lang.Keyword
;;   g/OpSigGroup
;;   {:** kw/mult})
;;    ;; :id kw/id
;;    ;; :id? kw/id?})
