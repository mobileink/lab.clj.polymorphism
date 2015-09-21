(ns algebra.structure.monoid.keyword
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn op2
  [op]
  )

(defn mult
  [operand1 operand2]
  ;; (log/info "mult " operand1 " " operand2)
  (case operand1
    :a (case operand2
         :a :a
         :b :b
         :c :c)
    :b (case operand2
         :a :b
         :b :c
         :c :a)
    :c (case operand2
         :a :c
         :b :a
         :c :b)
    (let [ex (apply vector (remove (set [:a :b :c]) [operand1 operand2]))]
      (throw (RuntimeException. (str "Undefined on " ex "; only :a :b :c allowed"))))))

(defn id [op] :a)

(defn id? [operand]
  (if (every? (set [:a :b :c]) [operand])
    (= :a operand)
    (let [ex (apply vector (remove (set [:a :b :c]) [operand]))]
      (throw (RuntimeException. (str "Undefined on " ex "; only :a :b :c allowed"))))))
