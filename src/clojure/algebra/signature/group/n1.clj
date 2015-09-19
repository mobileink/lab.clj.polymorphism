(ns algebra.signature.group.n1
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.group.n1")

(defprotocol OpSigGroupN1
  "Operator Signature for N1* Group"
  (** [operand1 operand2])
  ;; (id [op])
  (id? [operand]))

(defn id [] 1)
