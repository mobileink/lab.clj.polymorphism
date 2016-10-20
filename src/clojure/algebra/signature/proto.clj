(ns algebra.signature.proto
  (:refer-clojure :exclude [name])
  (:require ;algebra.models.group
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.proto")

(defprotocol ProtoAlgebra
  "Operator Signature for Algebra metadata and metaops"
  (name [t])
  (^Keyword structure [t]))
  ;; (install! [t])
  ;; (activate! [t])
  ;; (active-model? [t])
  ;; (active-model [t]))
