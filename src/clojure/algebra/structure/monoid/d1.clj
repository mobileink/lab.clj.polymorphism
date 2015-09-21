(ns algebra.structure.monoid.d1
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn mult
  [operand1 operand2]
  (log/info "mult")
  (if (and (>= operand1 1)
           (>= operand2 1))
    (* operand1 operand2)
    (throw (RuntimeException. (str "Undefined on "
                                   operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2) ")")))))

(defn id [] 1)

(defn id? [operand]
  ;; (if (= (class operand) java.lang.Long)
  (if (< operand 1)
    (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
    (= operand 1.0)))

