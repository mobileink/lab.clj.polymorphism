(ns algebra.signature.group.n0
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.group.n0")

(defprotocol OpSigGroupN0
  "Operator Signature for N0+ Group"
  (** [operand1 operand2])
;  (id [op])
  (id? [operand]))

(defn id [] 0)
