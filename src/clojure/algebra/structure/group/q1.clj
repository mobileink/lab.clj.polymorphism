(ns algebra.structure.group.q1
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn algebra-name [_] "Group (N,1,*)")
(defn structure [_] :n1)
(defn constants [_] {:id 1})

(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (if (and (> operand1 0)
           (> operand2 0))
    (* operand1 operand2)
    (throw (RuntimeException. (str "Undefined on "
                                   operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2) ")")))))

;; (defn id? [operand]
;;   ;; (if (= (class operand) java.lang.Long)
;;   (if (< operand 1)
;;     (throw (RuntimeException. (str "Undefined on " operand " (" (type operand))))
;;     (= operand 1)))
