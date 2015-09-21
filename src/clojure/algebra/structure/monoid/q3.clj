(ns algebra.structure.monoid.q3
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn algebra-name [_] "Quotient Group modulo 3 (addition)")
(defn structure [_] :quotient-3)
(defn constants [_] {:id 0})

(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (mod (+ operand1 operand2) 3))
