(ns foo.multi.core
  (:refer-clojure :exclude [count])
  (:require [foo.multi.count :refer :all]
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading foo.proto.api")

;; an api the multimethod way.

(defn l-or-v-foo?
  [datum]
  (log/info "l-or-v-foo?")
  (some? (some #{:foo} datum)))

(defn m-or-s-foo?
  [datum]
  (log/info "m-or-s-foo?")
  (contains? datum :foo))

(defmulti foo? class)

(defmethod foo?  clojure.lang.IPersistentList
  [datum]
  (log/info "IPersistentList.foo?")
  (l-or-v-foo? datum))

(defmethod foo?  clojure.lang.IPersistentMap
  [datum]
  (log/info "IPersistentMap.foo?")
  (m-or-s-foo? datum))

(defmethod foo?  clojure.lang.IPersistentSet
  [datum]
  (log/info "IPersistentSet.foo?")
  (l-or-v-foo? datum))

(defmethod foo?  clojure.lang.IPersistentVector
  [datum]
  (log/info "IPersistentVector.foo?")
  (l-or-v-foo? datum))

(load "sum")
