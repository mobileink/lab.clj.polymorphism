(ns algebra.signature.group
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.group")

(defprotocol OpSigGroup
  "Operator Signature for Groups"
  (** [operand1 operand2])
  ;; (id [op])
  (id? [operand]))
