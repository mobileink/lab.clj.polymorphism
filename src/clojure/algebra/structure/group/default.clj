(ns algebra.structure.group.default
  (:require ;;[algebra.signature.meta :as g :refer [*active-model*]]
            ;; [algebra.signature.meta :as g :refer [active-model]]
            [clojure.tools.logging :as log :only [debug info]]))


;; meta
(def ^:dynamic *active-model* :default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  OpSigGroup operations

(defn algebra-name [_] "Default group (Z,0,+) with java.lang.Object")
(defn structure [_] :default)
(defn constants [_] {:id 0})

(defn mult
  [operand1 operand2]
  (log/info "mult")
  ;; ;; if active model not :default then
  ;; ;; dispatch to active struct-type
  ;; ;; (log/debug (str "active model: " g/*active-model*))
  ;; (if (not= (*active-model* :default))
  ;;   (do
  ;;     ;; (log/debug "dispatching to " (g/active-model))
  ;;     (let [param (g/dispatch-type)]
  ;;       ;; (log/debug "param: " (class param))
  ;;       (g/** param operand1 operand2)))
  ;;   (if (and ;;(float? operand1)
  ;;        (> operand1 -1)
  ;;        ;;(float? operand2)
  ;;        (> operand2 -1))
  ;;     (+ operand1 operand2)
  ;;     (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
  ;;                                    operand2 " (" (type operand2)))))))
)

(defn install! [t])
(defn activate! [t])
(defn active-model? [t])
(defn active-model [t])
