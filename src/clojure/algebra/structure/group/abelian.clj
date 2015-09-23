(ns algebra.structure.group.abelian
  (:require [clojure.tools.logging :as log :only [debug info]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  OpSigGroup operator implementations

(defn algebra-name [_] "Group (N,0,+)")
(defn structure [_]
  (log/debug "structure")
  :n0)
(defn constants [_] {:id 0})

(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (if (and ;; (integer? operand1)
           (> operand1 -1)
           ;; (integer? operand2)
           (> operand2 -1))
           (+ operand1 operand2)
    (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2))))))

(defn install! [t])
(defn activate! [t])
(defn active-model? [t])
(defn active-model [t])
