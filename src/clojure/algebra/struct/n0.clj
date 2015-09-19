(ns algebra.struct.n0
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn mult
  [operand1 operand2]
  (log/info "mult")
  (if (and ;; (integer? operand1)
           (> operand1 -1)
           ;; (integer? operand2)
           (> operand2 -1))
           (+ operand1 operand2)
    (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2))))))

(defn id? [operand]
  ;; (if (= (class operand) java.lang.Long)
  (if (< operand 0)
    (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
    (= operand 0)))
