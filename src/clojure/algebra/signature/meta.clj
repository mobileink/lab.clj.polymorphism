(ns algebra.signature.meta
  (:refer-clojure :exclude [name])
  (:require ;algebra.models.group
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.meta")

(defprotocol AlgebraMeta
  "Operator Signature for Algebra metadata and metaops"
  (name [t])
  (^Keyword structure [t]))
  ;; (install! [t])
  ;; (activate! [t])
  ;; (active-model? [t])
  ;; (active-model [t]))
