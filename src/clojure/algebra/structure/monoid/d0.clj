(ns algebra.structure.monoid.d0
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn mult
  [operand1 operand2]
  (log/info "mult")
  (if (and ;;(float? operand1)
           (> operand1 -1)
           ;;(float? operand2)
           (> operand2 -1))
           (+ operand1 operand2)
    (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2))))))

(defn id [] 0)

(defn id? [operand]
  ;; (if (= (class operand) java.lang.Long)
  (if (< operand 0)
    (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
    (= operand 0)))
