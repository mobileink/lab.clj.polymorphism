(ns dependent-types.foo.multi.count
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading dependent-types.foo.proto.api")

;; (defprotocol PFoo
;;   "protocol for Foo datatype"
;;   (foo? [datum])
;;   (count [datum])
;;   (sum [datum])
;;   (mult [datum])
;;   (mean [datum])
;;   (mean-geometric [datum])
;;   (median [datum])
;;   (mode [datum])
;;   )

(defn l-or-v-count
  [datum]
  (log/info "l-or-v-count")
  (some? (some #{:foo} datum)))

(defn m-or-s-count
  [datum]
  (log/info "m-or-s-count")
  (contains? datum :foo))

(defmulti count class)

(defmethod count  clojure.lang.IPersistentList
  [datum]
  (log/info "IPersistentList.count")
  (l-or-v-count datum))

(defmethod count  clojure.lang.IPersistentMap
  [datum]
  (log/info "IPersistentMap.count")
  (m-or-s-count datum))

(defmethod count  clojure.lang.IPersistentSet
  [datum]
  (log/info "IPersistentSet.count")
  (l-or-v-count datum))

(defmethod count  clojure.lang.IPersistentVector
  [datum]
  (log/info "IPersistentVector.count")
  (l-or-v-count datum))
