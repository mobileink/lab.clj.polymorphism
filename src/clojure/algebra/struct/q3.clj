(ns algebra.struct.q3
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn group-name [_] "Quotient Group modulo 3")
(defn struct-kw [_] :quotient-3)

(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (if (and (> operand1 0)
           (> operand2 0))
    (* operand1 operand2)
    (throw (RuntimeException. (str "Undefined on "
                                   operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2) ")")))))

(defn id [t] 0)

(defn id?
  [t operand]
  ;; (if (= (class operand) java.lang.Long)
  (if (< operand 1)
    (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
    (= operand 1)))
