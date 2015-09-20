(ns algebra.struct.q3
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn group-name [_] "Quotient Group modulo 3 (addition)")
(defn id [_] 0)
(defn struct-kw [_] :quotient-3)

(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (mod (+ operand1 operand2) 3))

(defn id?
  [t operand]
  (= operand 0))
