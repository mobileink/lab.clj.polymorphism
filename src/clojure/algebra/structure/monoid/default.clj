(ns algebra.structure.monoid.default
  (:require [algebra.signature.monoid :as g :refer [*active-model*]]
            [algebra.signature.meta :as m]  ;; :refer [*active-model*]]
            [clojure.tools.logging :as log :only [debug info]]))

;;;; operators
(defn algebra-name [_] "Default Monoid (Z,0,+) with java.lang.Object")

(defn type [t] t)

(defn structure [_] :default)

(defn constants [_] {:id 0})

;; operators
(defn mult
  [operand1 operand2]
  (log/info "mult" operand1 operand2 g/*active-model*)
  ;; if active model not :default then
  ;; dispatch to active struct-type
  ;; (log/debug (str "active model: " g/*active-model*))
  (if (not= g/*active-model* :default)
    (do
      (log/debug "dispatching to " *active-model*)
      (let [param (g/dispatch-type)]
        (log/debug "param: " (class param))
        (g/** param operand1 operand2)))
    (if (and (> operand1 -1)
             (> operand2 -1))
      (+ operand1 operand2)
      (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                     operand2 " (" (type operand2)))))))

