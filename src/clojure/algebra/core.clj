(ns algebra.core
  (:require [algebra.signature.group :as g]
            [algebra.struct.n0]
            [algebra.struct.n1 :as n1]
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading core")

(g/register-struct (java.lang.Object.))
(g/set-model :default)

;; to register a structure for a model of a
;; signature (i.e. an implementation):

;;(g/register-struct :default) ;; algebra.struct.Quotient-3)

;(set-current-struct-type :n0)
