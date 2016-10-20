(ns algebra.core
  (:require [algebra.signature.monoid :as m]
            ;; algebra.models.monoid
            ;; [algebra.structure.monoid.n1 :as n1]
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.core")

(m/install! (java.lang.Object.))
(m/activate! :default)

;; to register a structure for a model of a
;; signature (i.e. an implementation):

;;(g/register-struct :default) ;; algebra.struct.Quotient-3)

;(set-current-struct-type :n0)
