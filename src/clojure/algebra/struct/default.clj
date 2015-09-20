(ns algebra.struct.default
  (:require [algebra.signature.group :as g :refer [*current-model*]]
            [clojure.tools.logging :as log :only [debug info]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  OpSigGroup operations

;;(defn group-name [_] "Default Group")
(defn struct-kw [_] :default)

(defn mult
  [operand1 operand2]
  (log/info "mult")
  ;; if current model not :default then
  ;; dispatch to current struct-type
  ;; (log/debug (str "current model: " g/*current-model*))
  (if (not= g/*current-model* :default)
    (do
      ;; (log/debug "dispatching to " *current-model*)
      (let [param (g/dispatch-param)]
        ;; (log/debug "param: " (class param))
        (g/** param operand1 operand2)))
    (if (and ;;(float? operand1)
         (> operand1 -1)
         ;;(float? operand2)
         (> operand2 -1))
      (+ operand1 operand2)
      (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                     operand2 " (" (type operand2)))))))

(defn id [_] 0)

(defn id? [operand]
  ;; (if (= (class operand) java.lang.Long)
  (if (< operand 0)
    (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
    (= operand 0)))
