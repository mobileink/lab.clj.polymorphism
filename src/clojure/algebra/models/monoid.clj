(ns algebra.models.monoid
  (:refer-clojure :exclude [count])
  (:require [algebra.signature.meta :as m]
            [algebra.signature.monoid :as monoid]
            ;; [algebra.signature.group.n0 :as g0]
            ;; [algebra.signature.group.n1 :as g1]
            types.monoid ; deftypes for implementation struct types
            [algebra.structure.monoid.default :as default]
            [algebra.structure.monoid.n0 :as n0]
            [algebra.structure.monoid.n1 :as n1]
            [algebra.structure.monoid.q3 :as q3]
            [algebra.structure.monoid.d0 :as d0]
            [algebra.structure.monoid.d1 :as d1]
            [algebra.structure.monoid.keyword :as kw]
            [clojure.tools.logging :as log :only [debug info]]
            ))

(println "loading algebra.models.monoid")

;; default model
(extend java.lang.Object
  m/AlgebraMeta {:name default/algebra-name}
                 ;; :install! default/install!
                 ;; :activate! default/activate!
                 ;; :active-model? default/active-model?
                 ;; :active-model default/active-model
  monoid/Operators {:** default/mult
                    ;; :idem default/idem
                    :constants default/constants
                    :structure default/structure
                    :typ default/typ})

;; parameterized models
(extend types.monoid.N0
  m/AlgebraMeta {:name n0/algebra-name}
                 ;; :install! n0/install!
                 ;; :activate! n0/activate!
                 ;; :active-model? n0/active-model?
                 ;; :active-model n0/active-model
  monoid/Operators {:** n0/mult
                    :structure n0/structure
                    :constants n0/constants})

(extend types.monoid.N1
  m/AlgebraMeta {:name n1/algebra-name}
                 ;; :install! n1/install
                 ;; :activate! n1/activate
                 ;; :active-model? n1/active-model?
                 ;; :active-model n1/active-model
  monoid/Operators {:** n1/mult
                    ;; :idem n1/idem
                    :structure n1/structure
                    :constants n1/constants
                    :typ n1/typ})

;; (extend types.monoid.Q3+
;;   m/AlgebraMeta {:name q3/algebra-name}
;;                  ;; :install! q3/install
;;                  ;; :activate! q3/activate
;;                  ;; :active-model? q3/active-model?
;;                  ;; :active-model q3/active-model
;;   monoid/OpSigGroup {:** q3/mult
;;                      :structure q3/structure
;;                      :constants q3/constants})
