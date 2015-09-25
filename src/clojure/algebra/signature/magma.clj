(ns algebra.signature.magma
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.magma")

(defn mag [a] (log/debug "mag"))
(def magma true)
(defn nbr-mult [t a b] (* a b))
(defn str-concat [a b] (str a b))


(defprotocol Operators
  "Operator Signature for Magmas"
  (** [a b] [t a b]))

(deftype Magma [])

(extend Magma
  Operators
  {:** nbr-mult})

(def M (Magma.))
(extend java.lang.Long
  Operators
  {:** (fn [a b] (** M a b))})

