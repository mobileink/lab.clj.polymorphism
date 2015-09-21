(ns algebra.structure.monoid.n0
  (:require types.monoid ;; deftype dispatch param types
            [clojure.tools.logging :as log :only [debug info]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  meta
(defonce type-object 'types.monoid.N0) ;; FIXME

(defn algebra-name [_] "Monoid (N,0,+)")

(defn structure [_]
  (log/debug "structure")
  :n0)

(defn install! [t]
  (log/debug "install!"))

(defn activate! [t])


(defn constants [_] {:id 0})

;; operators
(defn mult
  [t operand1 operand2]
  (log/info "mult")
  (if (and ;; (integer? operand1)
           (> operand1 -1)
           ;; (integer? operand2)
           (> operand2 -1))
;    (with-meta
      (+ operand1 operand2)
;      {:type 'type-object})
    (throw (RuntimeException. (str "Undefined on " operand1 " (" (type operand1) "), "
                                   operand2 " (" (type operand2))))))

(defn active-model? [t]
  (log/debug "active-model?" t)
  )

(defn active-model [t]
  (log/debug "active-model")
  )
